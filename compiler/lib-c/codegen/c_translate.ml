(* C_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* The translation of the instructions of a function: its context, its
   variables, the closures, the applications and the tail calls *)

open! Stdlib
open Code
open! C_repr
open! C_output
open! C_primitives
open! C_facts

(* Functions *)

type location =
  | Local  (** A variable of the function *)
  | Env of int  (** A field of the closure *)
  | Sibling of int  (** A function of the same closure, at this offset in words *)
  | Global of string  (** A toplevel value stored in a C global *)
  | Static of string  (** A closed closure or a constant: static data *)

type trap =
  { exn_var : Var.t
  ; id : int
  }

type fctx =
  { out : output
  ; facts : facts
  ; global_flow_info : Global_flow.info
  ; blocks : block Addr.Map.t
  ; closures : Closure_conversion.closure Var.Map.t
  ; env_var : Var.t  (** Stands for the closure of the function *)
  ; fn : Var.t option  (** The function, [None] for the toplevel code *)
  ; arity : repr list option
        (** The representation of the parameters of the function, not the toplevel *)
  ; static_env : bool  (** The closure is static data *)
  ; locations : location Var.Hashtbl.t
  ; slots : int Var.Hashtbl.t  (** The variables in the frame *)
  ; traps : trap list Addr.Hashtbl.t  (** The handlers in scope in each block *)
  ; handler_ids : int Addr.Hashtbl.t  (** The handler of the [Pushtrap] ending a block *)
  ; local_names : int Var.Hashtbl.t
  ; labels : int Addr.Hashtbl.t
        (** The variables and the blocks are numbered in each function, so that
            the code of a function does not depend on the rest of the program *)
  ; buf : Buffer.t
  ; mutable position : Parse_info.t option  (** The last debug event *)
  ; source_name : string option  (** The name of the function in the source *)
  ; raises : bool  (** The function may raise *)
  ; ret : repr  (** The representation of its result *)
  ; mutable framed : bool  (** The frame is allocated in the current block *)
  ; mutable framed_blocks : Addr.Set.t option
        (** The blocks where the frame is allocated, if not all *)
  ; mutable frame_push : string  (** Allocates the frame *)
  }

(* Record a backtrace slot, if debug information is enabled: [`Call] when
   an exception goes through a call, [`Raise reraise] when one is raised
   (with the exception [e]) *)
let backtrace_stmt ctx kind e =
  match ctx.position with
  | Some pi when ctx.out.debug_info -> (
      match kind with
      | `Call ->
          Printf.sprintf
            "if (Caml_state->backtrace_active) camlc_backtrace_call(%s); "
            (location ctx.out pi ~fun_name:ctx.source_name ~is_raise:false)
      | `Raise reraise ->
          Printf.sprintf
            "if (Caml_state->backtrace_active) camlc_backtrace_raise(%s, %s, %d); "
            e
            (location ctx.out pi ~fun_name:ctx.source_name ~is_raise:true)
            (if reraise then 1 else 0))
  | Some _ | None -> ""

let location ctx x =
  match Var.Hashtbl.find_opt ctx.locations x with
  | Some _ as l -> l
  | None -> (
      match Var.Hashtbl.find_opt ctx.out.globals x with
      | Some g -> Some (Global g)
      | None -> (
          match Var.Hashtbl.find_opt ctx.out.statics x with
          | Some e -> Some (Static e)
          | None -> None))

let local_name ctx x =
  if Var.equal x ctx.env_var
  then "env"
  else
    let n =
      match Var.Hashtbl.find_opt ctx.local_names x with
      | Some n -> n
      | None ->
          let n = Var.Hashtbl.length ctx.local_names in
          Var.Hashtbl.add ctx.local_names x n;
          n
    in
    Printf.sprintf "v%d" n

let label ctx pc =
  let n =
    match Addr.Hashtbl.find_opt ctx.labels pc with
    | Some n -> n
    | None ->
        let n = Addr.Hashtbl.length ctx.labels in
        Addr.Hashtbl.add ctx.labels pc n;
        n
  in
  Printf.sprintf "L%d" n

(* A variable is read from its C variable. Those in the frame are also
   written to their slot when defined, and their C variable is reloaded
   from it after each point where the GC may run (see [reload]). *)
let local ctx x = local_name ctx x

let var ctx x =
  match location ctx x with
  | Some Local -> local ctx x
  | Some (Global g) -> g
  | Some (Static e) -> e
  | Some (Env i) -> Printf.sprintf "Field(%s, %d)" (local ctx ctx.env_var) i
  | Some (Sibling 0) -> local ctx ctx.env_var
  | Some (Sibling d) ->
      Printf.sprintf "((value)((value *)%s + %d))" (local ctx ctx.env_var) d
  | None -> failwith (Format.asprintf "c_of_ocaml: unbound variable %a" Var.print x)

let slot ctx x =
  match location ctx x with
  | Some Local -> (
      (* Assigned as [fp[i] = x = e] *)
      match Var.Hashtbl.find_opt ctx.slots x with
      | Some i -> Printf.sprintf "fp[%d] = %s" i (local_name ctx x)
      | None -> local ctx x)
  | Some (Global g) -> g
  | Some (Env _ | Sibling _ | Static _) | None ->
      failwith (Format.asprintf "c_of_ocaml: variable %a is not local" Var.print x)

(* The local variables used by an expression, the variables of the
   enclosing functions standing for the closure *)
let use ctx x acc =
  match location ctx x with
  | Some Local -> Var.Set.add x acc
  | Some (Env _ | Sibling _) -> Var.Set.add ctx.env_var acc
  | Some (Global _ | Static _) | None -> acc

let use_list ctx l acc = List.fold_left l ~init:acc ~f:(fun acc x -> use ctx x acc)

(* Reload the C variables of the variables in the frame among the variables
   [l] (the closure standing for itself) or the closure they are read
   from *)
let reload ctx l =
  String.concat
    ~sep:""
    (List.filter_map
       (Var.Set.elements
          (List.fold_left l ~init:Var.Set.empty ~f:(fun acc x ->
               if Var.equal x ctx.env_var then Var.Set.add x acc else use ctx x acc)))
       ~f:(fun x ->
         match Var.Hashtbl.find_opt ctx.slots x with
         | Some i -> Some (Printf.sprintf "%s = fp[%d]; " (local_name ctx x) i)
         | None -> None))

let var_repr ctx x =
  if Var.equal x ctx.env_var
  then Value
  else
    match location ctx x with
    | Some Local -> repr_of_type (Typing.var_type ctx.out.types x)
    | Some (Env _ | Sibling _ | Global _ | Static _) | None -> Value

(* The variable [x] in representation [r]: boxed values are unboxed, while
   the typing ensures that unboxed numbers are never needed boxed (they are
   boxed where they are defined) *)
let as_repr ctx r x =
  let e = var ctx x in
  let own = var_repr ctx x in
  if repr_equal own r
  then e
  else
    match own with
    | Value -> unbox r e
    | Double | Unboxed_int64 | Unboxed_int32 | Unboxed_nativeint ->
        failwith (Format.asprintf "c_of_ocaml: %a would need to be boxed" Var.print x)

let as_value ctx x = as_repr ctx Value x

let constant_repr out c r =
  match r, c with
  | Value, _ -> code_constant out c
  | Double, Float f -> Printf.sprintf "camlc_double_of_bits(0x%LxULL)" f
  | Unboxed_int64, Int64 i -> Printf.sprintf "((int64_t)0x%LxULL)" i
  | Unboxed_int32, Int32 i -> Printf.sprintf "((int32_t)0x%lxU)" i
  | Unboxed_nativeint, NativeInt i -> Printf.sprintf "((intnat)0x%LxULL)" i
  | _ -> failwith "c_of_ocaml: a constant of the wrong type"

let prim_arg ctx a =
  match a with
  | Pv x ->
      { a_typ = Typing.var_type ctx.out.types x
      ; a_const = None
      ; get = (fun r -> as_repr ctx r x)
      }
  | Pc c ->
      { a_typ = Typing.constant_type c
      ; a_const = Some c
      ; get = (fun r -> constant_repr ctx.out c r)
      }

let line ctx fmt = Printf.bprintf ctx.buf ("  " ^^ fmt ^^ "\n")

(* [x = e], [e] being in representation [r] *)
let define ctx x r e =
  let own = var_repr ctx x in
  let e =
    if repr_equal own r
    then e
    else
      match r, own with
      | Value, _ -> unbox own e
      | _, Value -> box r e
      | _ -> failwith (Format.asprintf "c_of_ocaml: %a of the wrong type" Var.print x)
  in
  line ctx "%s = %s;" (slot ctx x) e

(* Return [e], in representation [r] *)
let return_stmt ?(r = Value) ctx e =
  if not (repr_equal r ctx.ret)
  then failwith "c_of_ocaml: a function returning an unboxed number returns a value";
  if not ctx.framed
  then Printf.sprintf "return %s;" e
  else
    Printf.sprintf "{ %s res = %s; camlc_sp = fp + FRAME_SIZE; return res; }" (c_type r) e

(* Jump to the handler of the innermost trap, or return the exception
   result [r] *)
let raise_stmt ?bt ctx traps r =
  let pre =
    match bt with
    | None -> ""
    | Some kind -> backtrace_stmt ctx kind (Printf.sprintf "Extract_exception(%s)" r)
  in
  let stmt =
    match traps with
    | [] -> return_stmt ctx r
    | { exn_var; id; _ } :: _ ->
        Printf.sprintf "{ %s = Extract_exception(%s); goto H%d; }" (slot ctx exn_var) r id
  in
  if String.equal pre "" then stmt else Printf.sprintf "{ %s%s }" pre stmt

(* An overflow of the shadow stack *)
let overflow_stmt ctx traps =
  if List.is_empty traps && not ctx.raises
  then "camlc_fatal_stack_overflow();"
  else raise_stmt ctx traps "camlc_stack_overflow()"

let branch ctx (pc, args) =
  let params = (Addr.Map.find pc ctx.blocks).params in
  let moves =
    List.filter_map (List.combine params args) ~f:(fun (p, a) ->
        if Var.equal p a then None else Some (p, a))
  in
  (* Entering the blocks where the frame is allocated *)
  let push =
    match ctx.framed_blocks with
    | Some s when (not ctx.framed) && Addr.Set.mem pc s -> ctx.frame_push
    | Some _ | None -> ""
  in
  match moves with
  | [] when String.equal push "" -> Printf.sprintf "goto %s;" (label ctx pc)
  | [] -> Printf.sprintf "{ %s goto %s; }" push (label ctx pc)
  | [ (p, a) ] ->
      Printf.sprintf
        "{ %s%s = %s; goto %s; }"
        push
        (slot ctx p)
        (as_repr ctx (var_repr ctx p) a)
        (label ctx pc)
  | _ ->
      let temps =
        List.mapi moves ~f:(fun i (p, a) ->
            let r = var_repr ctx p in
            Printf.sprintf "%s t%d = %s;" (c_type r) i (as_repr ctx r a))
      in
      let assigns =
        List.mapi moves ~f:(fun i (p, _) -> Printf.sprintf "%s = t%d;" (slot ctx p) i)
      in
      Printf.sprintf
        "{ %s%s %s goto %s; }"
        push
        (String.concat ~sep:" " temps)
        (String.concat ~sep:" " assigns)
        (label ctx pc)

(* Allocate a block whose fields are [fields] (C expressions, read after
   the allocation), and store it in [dst] *)
let alloc_block ?(reload = "") ctx ~dst ~tag ~prefix fields =
  let wosize = List.length prefix + List.length fields in
  if wosize <= 256
  then (
    line ctx "b = camlc_alloc_small(%d, %d);" wosize tag;
    if not (String.equal reload "") then line ctx "%s" reload;
    List.iteri prefix ~f:(fun i e -> line ctx "Field(b, %d) = %s;" i e);
    List.iteri fields ~f:(fun i e ->
        line ctx "Field(b, %d) = %s;" (i + List.length prefix) e))
  else (
    line ctx "b = caml_alloc_shr(%d, %d);" wosize tag;
    if not (String.equal reload "") then line ctx "%s" reload;
    List.iteri prefix ~f:(fun i e -> line ctx "Field(b, %d) = %s;" i e);
    List.iteri fields ~f:(fun i e ->
        line ctx "caml_initialize(&Field(b, %d), %s);" (i + List.length prefix) e));
  line ctx "%s = b;" dst

(* The start of the environment, and the position of the code pointer of
   each function. In a closure with several functions, a function of arity 2
   or more takes three words, as in native code: the compactor of OCaml 4.14
   finds the Infix headers from the arities. With effect handlers, each
   function takes three words: its code pointer, its closure info and the
   code pointer of its CPS version (or 0). *)
let closure_layout functions =
  let size arity =
    if double_translation () || (List.length functions > 1 && arity >= 2) then 3 else 2
  in
  let rec layout pos l =
    match l with
    | [] -> assert false
    | [ (_, arity) ] -> pos + size arity, [ pos ]
    | (_, arity) :: r ->
        let startenv, positions = layout (pos + size arity + 1) r in
        startenv, pos :: positions
  in
  layout 0 functions

(* The code pointers and headers of a closure, before its free variables *)
let closure_prefix out functions =
  let startenv, positions = closure_layout functions in
  List.concat
    (List.map2 functions positions ~f:(fun (g, arity) pos ->
         let code =
           [ Printf.sprintf "(value)&%s" (fun_ref out g)
           ; Printf.sprintf "Make_closinfo(%d, %d)" arity (startenv - pos)
           ]
         in
         let code =
           if double_translation ()
           then
             code
             @ [ (match Var.Map.find_opt g out.cps_pairs with
                 | Some c -> Printf.sprintf "(value)&%s" (fun_ref out c)
                 | None -> "CAMLC_NO_CPS")
               ]
           else if List.length functions > 1 && arity >= 2
           then code @ [ "Val_unit" ]
           else code
         in
         if pos = 0
         then code
         else Printf.sprintf "Make_header(%d, Infix_tag, 0)" pos :: code))

(* A closed closure is static data: the address of each function *)
let static_closure out functions =
  let prefix = closure_prefix out functions in
  let n =
    static_block out (header ~wosize:(List.length prefix) ~tag:Obj.closure_tag :: prefix)
  in
  let _, positions = closure_layout functions in
  List.map2 functions positions ~f:(fun (g, _) pos ->
      g, Printf.sprintf "(value)&%s[%d]" n (pos + 1))

let translate_closure ctx f =
  let info = Var.Map.find f ctx.closures in
  match info.functions with
  | _ when Var.Set.mem f ctx.out.cps_sides ->
      (* A CPS version, in the closure of its direct-style version *)
      ()
  | (f0, _) :: _ when not (Var.equal f f0) ->
      (* Allocated with the first function of the closure *)
      ()
  | functions ->
      if List.is_empty info.free_variables
      then
        List.iter functions ~f:(fun (g, _) ->
            line ctx "%s = %s;" (slot ctx g) (Var.Hashtbl.find ctx.out.statics g))
      else
        let _, positions = closure_layout functions in
        alloc_block
          ~reload:(reload ctx info.free_variables)
          ctx
          ~dst:(slot ctx f)
          ~tag:Obj.closure_tag
          ~prefix:(closure_prefix ctx.out functions)
          (List.map info.free_variables ~f:(fun x -> as_value ctx x));
        List.iter2 functions positions ~f:(fun (g, _) pos ->
            if pos > 0 then line ctx "%s = (value)((value *)b + %d);" (slot ctx g) pos)

let apply_arity out n = out.apply_arities <- IntSet.add n out.apply_arities

let out_cps_apply_arity out n =
  out.cps_apply_arities <- IntSet.add n out.cps_apply_arities

let param_reprs ctx g =
  Var.Hashtbl.find_opt ctx.out.fun_reprs g |> Option.value ~default:[]

(* A CPS call: its last argument is the continuation *)
let is_cps_call ctx x = double_translation () && Var.Set.mem x ctx.out.in_cps

type callee =
  | Known of Var.t  (** The function statically known to be called *)
  | Known_direct of Var.t
      (** A CPS call of a function without CPS version: it is called in
          direct style, and its result passed to the continuation *)
  | Unknown

let resolve_callee global_flow_info closures x ~f ~exact =
  if exact
  then
    match Global_flow.get_unique_closure global_flow_info ~call:x f with
    | Some (g, _, cont) when Var.Map.mem g closures ->
        if cont then Known_direct g else Known g
    | Some _ | None -> Unknown
  else Unknown

let callee ctx x ~f ~exact = resolve_callee ctx.global_flow_info ctx.closures x ~f ~exact

(* The representation of the parameters of the function called *)
let callee_reprs ctx x ~f ~args ~exact =
  match callee ctx x ~f ~exact with
  | Known g -> param_reprs ctx g
  | Known_direct g -> param_reprs ctx g @ [ Value ]
  | Unknown -> List.map args ~f:(fun _ -> Value)

(* The arguments of an application which are unboxed numbers passed to a
   parameter holding values: they are boxed before the call *)
let boxed_arguments ctx x ~f ~args ~exact =
  List.map2 args (callee_reprs ctx x ~f ~args ~exact) ~f:(fun a r ->
      repr_equal r Value && not (repr_equal (var_repr ctx a) Value))

(* The C function called by an application, its arguments ([n] arguments
   and a closure in all cases), and the representation of its parameters.
   The numbers to box are boxed first, into slots of the shadow stack
   ([bx], see [boxing_prelude]). For [Known_direct], the arguments are those
   of the function, its closure, then the continuation. *)
let apply ctx x ~f ~args ~exact =
  let n = List.length args in
  let reprs = callee_reprs ctx x ~f ~args ~exact in
  let boxed = boxed_arguments ctx x ~f ~args ~exact in
  let j = ref 0 in
  let args =
    List.map2 (List.combine args reprs) boxed ~f:(fun (a, r) b ->
        if b
        then (
          let e = Printf.sprintf "bx[%d]" !j in
          incr j;
          e)
        else as_repr ctx r a)
  in
  let fe = as_value ctx f in
  match callee ctx x ~f ~exact with
  | Known g -> fun_ref ctx.out g, args @ [ fe ], reprs
  | Known_direct g ->
      let args, k = List.take (n - 1) args in
      fun_ref ctx.out g, args @ [ fe ] @ k, reprs
  | Unknown ->
      if is_cps_call ctx x
      then (
        out_cps_apply_arity ctx.out n;
        ( Printf.sprintf (if exact then "camlc_call_cps%d" else "camlc_apply_cps%d") n
        , fe :: args
        , reprs ))
      else if exact
      then Printf.sprintf "((%s)Field(%s, 0))" (fun_type n) fe, args @ [ fe ], reprs
      else (
        apply_arity ctx.out n;
        Printf.sprintf "camlc_apply%d" n, fe :: args, reprs)

(* Box the numbers passed to parameters holding values into [bx], a part
   of the shadow stack (an allocation may move the previous ones). Returns
   the number of slots. *)
let boxing_prelude ctx traps x ~f ~args ~exact =
  let boxed =
    List.filter_map
      (List.combine args (boxed_arguments ctx x ~f ~args ~exact))
      ~f:(fun (a, b) -> if b then Some a else None)
  in
  let k = List.length boxed in
  if k > 0
  then (
    line ctx "bx = camlc_sp - %d;" k;
    line ctx "if (CAMLC_UNLIKELY(bx < camlc_stack_limit)) %s" (overflow_stmt ctx traps);
    line ctx "camlc_sp = bx;";
    List.iteri boxed ~f:(fun i _ -> line ctx "bx[%d] = Val_unit;" i);
    List.iteri boxed ~f:(fun i a ->
        let r = var_repr ctx a in
        line ctx "bx[%d] = %s;" i (box r (var ctx a)));
    line ctx "%s" (reload ctx (f :: args)));
  k

(* Whether a call of [fn] may be recursive: an unknown call, or a call of a
   function of its strongly connected component *)
let recursive_call out global_flow_info closures ~fn (x, f, exact) =
  match resolve_callee global_flow_info closures x ~f ~exact with
  | Known g -> (
      match fn with
      | Some fn -> (
          match
            Var.Hashtbl.find_opt out.components fn, Var.Hashtbl.find_opt out.components g
          with
          | Some c, Some c' -> c = c'
          | _ -> false)
      | None -> false)
  | Known_direct _ | Unknown -> true

(* An application whose result is returned: the frame is released before
   the call, which is a tail call when the callee has the signature of the
   function (as required by [musttail]). Otherwise, it would be a tail call
   only if the function had no other return: clang merges the returns, and
   the backend of wasm does not split them again for the call. With
   [CAMLC_TAIL_CALLS], the call then goes through a function of the
   signature of the caller ([camlc_t_...], defined by [declare_symbols]),
   called with [musttail], which finds the callee and its arguments in
   [camlc_seg] and makes the call, its only return: a tail call as well.
   Mutually recursive functions of different signatures then loop in
   constant space, as in OCaml, rather than growing the stack of the
   engine, out of reach of the frame checks (see [camlc_segment]). Only
   for the calls which may be recursive (see [recursive_call]), and those
   of functions which may make an unknown tail call (by themselves or
   through known tail calls: a loop of tail calls through closures, as the
   continuations of a function in CPS): the others end, and the C compiler
   may inline them.

   A CPS call which may bounce, unless it is a tail call (with
   [CAMLC_TAIL_CALLS]), first decrements
   [camlc_cps_depth]: when it gets negative, the call is returned to the
   trampoline ([camlc_cps_bounce]), which performs it on a shorter stack.
   Without [CAMLC_TAIL_CALLS], so do the calls of the continuations and the
   backward jumps of the CPS code ([camlc_cps_bounce_direct]). *)
let translate_tail_call ctx x ~f ~args ~exact =
  let n = List.length args in
  let k = boxing_prelude ctx [] x ~f ~args ~exact in
  let kind = callee ctx x ~f ~exact in
  let callee, args, reprs = apply ctx x ~f ~args ~exact in
  let types =
    List.map
      (match kind with
      | Known_direct _ -> fst (List.take (n - 1) reprs) @ [ Value; Value ]
      | Known _ | Unknown -> reprs @ [ Value ])
      ~f:c_type
  in
  let temps =
    List.mapi (List.combine types args) ~f:(fun i (t, a) ->
        Printf.sprintf "%s t%d = %s;" t i a)
  in
  let t i = Printf.sprintf "t%d" i in
  let ret =
    match kind with
    | Known g -> Var.Hashtbl.find ctx.out.fun_returns g
    | Known_direct _ | Unknown -> Value
  in
  (* The typing ensures that an unboxed result is never boxed *)
  if (not (repr_equal ret ctx.ret)) && repr_equal ctx.ret Value
  then failwith "c_of_ocaml: an unboxed result would need to be boxed";
  let musttail =
    match kind, ctx.arity with
    | (Known _ | Unknown), Some reprs' ->
        List.equal ~eq:repr_equal reprs reprs' && repr_equal ret ctx.ret
    | Known_direct _, _ | _, None -> false
  in
  let bounce =
    if
      Var.Set.mem x ctx.out.trampolined
      && (not (is_cps_call ctx x))
      && exact
      && List.for_all reprs ~f:(fun r -> repr_equal r Value)
    then
      (* A call of a continuation or of the closure of a block of CPS code,
         in direct style (the arguments, then the closure): a plain call
         without [CAMLC_TAIL_CALLS] *)
      let closure, arguments = n, List.init ~len:n ~f:Fun.id in
      Printf.sprintf
        " if (!CAMLC_TAIL_CALLS && CAMLC_UNLIKELY(--camlc_cps_depth < 0)) { %s return \
         camlc_cps_bounce_direct(%s, %d); }"
        (String.concat
           ~sep:" "
           (List.mapi arguments ~f:(fun i j ->
                Printf.sprintf "camlc_bounce_args[%d] = %s;" i (t j))))
        (t closure)
        n
    else if
      is_cps_call ctx x
      && Var.Set.mem x ctx.out.trampolined
      && List.for_all reprs ~f:(fun r -> repr_equal r Value)
    then
      (* The closure, then the arguments *)
      let closure, arguments =
        match kind with
        | Known _ -> n, List.init ~len:n ~f:Fun.id
        | Known_direct _ -> n - 1, List.init ~len:(n - 1) ~f:Fun.id @ [ n ]
        | Unknown -> 0, List.init ~len:n ~f:(fun i -> i + 1)
      in
      Printf.sprintf
        " if (%sCAMLC_UNLIKELY(--camlc_cps_depth < 0)) { %s return camlc_cps_bounce(%s, \
         %d); }"
        (* No need to bounce for a tail call *)
        (if musttail then "!CAMLC_TAIL_CALLS && " else "")
        (String.concat
           ~sep:" "
           (List.mapi arguments ~f:(fun i j ->
                Printf.sprintf "camlc_bounce_args[%d] = %s;" i (t j))))
        (t closure)
        n
    else ""
  in
  let pop =
    if not ctx.framed
    then if k > 0 then Printf.sprintf " camlc_sp = bx + %d;" k else ""
    else " camlc_sp = fp + FRAME_SIZE;"
  in
  (* Without [CAMLC_TAIL_CALLS], a tail call which may loop is counted, and
     left pending beyond [CAMLC_TAIL_DEPTH] (see [CAMLC_PENDING]): only
     in direct style (the CPS code bounces), between values *)
  let pending call =
    match kind, ctx.fn with
    | (Known _ | Unknown), Some fn
      when String.equal bounce ""
           && (not (is_cps_call ctx x))
           && (not (Var.Set.mem fn ctx.out.cps_sides))
           && repr_equal ret Value
           && repr_equal ctx.ret Value
           && List.for_all reprs ~f:(fun r -> repr_equal r Value)
           && (recursive_call
                 ctx.out
                 ctx.global_flow_info
                 ctx.closures
                 ~fn:ctx.fn
                 (x, f, exact)
              ||
              match kind with
              | Known g -> Var.Set.mem g ctx.out.unknown_tail_calls
              | Known_direct _ | Unknown -> false) ->
        (* The closure, then the arguments *)
        let closure, arguments =
          match kind with
          | Known _ -> n, List.init ~len:n ~f:Fun.id
          | Unknown when exact -> n, List.init ~len:n ~f:Fun.id
          | Unknown | Known_direct _ -> 0, List.init ~len:n ~f:(fun i -> i + 1)
        in
        Printf.sprintf
          " if (!CAMLC_TAIL_CALLS) { if (CAMLC_UNLIKELY(++camlc_tail_depth > \
           CAMLC_TAIL_DEPTH)) { camlc_tail_depth--; %s return camlc_tail_pending(%s, \
           %d); } value tr = %s; camlc_tail_depth--; return tr; }"
          (String.concat
             ~sep:" "
             (List.mapi arguments ~f:(fun i j ->
                  Printf.sprintf "camlc_bounce_args[%d] = %s;" i (t j))))
          (t closure)
          n
          call
    | _ -> ""
  in
  match kind with
  | Known_direct _ ->
      (* The continuation is kept on the shadow stack during the call *)
      let call = c_call callee (List.init ~len:n ~f:t) in
      line
        ctx
        "{ %s%s%s value *ks = camlc_sp - 1; if (CAMLC_UNLIKELY(ks < camlc_stack_limit)) \
         return camlc_stack_overflow(); *ks = t%d; camlc_sp = ks; value r = \
         Camlc_resolve(%s); camlc_sp = ks + 1; if \
         (CAMLC_UNLIKELY(Is_exception_result(r))) return r; return ((value (*)(value, \
         value))Field(*ks, 0))(r, *ks); }"
        (String.concat ~sep:" " temps)
        pop
        bounce
        n
        call
  | Known _ | Unknown ->
      let call = c_call callee (List.mapi args ~f:(fun i _ -> t i)) in
      let through =
        match ctx.arity with
        | Some reprs'
          when (not musttail)
               && repr_equal ret ctx.ret
               && (recursive_call
                     ctx.out
                     ctx.global_flow_info
                     ctx.closures
                     ~fn:ctx.fn
                     (x, f, exact)
                  ||
                  match kind with
                  | Known g -> Var.Set.mem g ctx.out.unknown_tail_calls
                  | Known_direct _ | Unknown -> false) ->
            let callee_reprs =
              match kind with
              | Known _ -> reprs @ [ Value ]
              | Known_direct _ | Unknown -> List.map args ~f:(fun _ -> Value)
            in
            let caller_reprs = reprs' @ [ Value ] in
            let letters l =
              String.concat
                ~sep:""
                (List.map l ~f:(fun r -> String.make 1 (repr_letter r)))
            in
            let name =
              Printf.sprintf
                "camlc_t_%c_%s_%s"
                (repr_letter ret)
                (letters caller_reprs)
                (letters callee_reprs)
            in
            String.Hashtbl.replace ctx.out.segment_thunks name ();
            Printf.sprintf
              " if (CAMLC_TAIL_CALLS) { camlc_seg_fn = (void *)%s; %s CAMLC_MUSTTAIL \
               return %s(%s); }"
              callee
              (String.concat
                 ~sep:" "
                 (List.mapi callee_reprs ~f:(fun i r ->
                      Printf.sprintf "camlc_seg[%d].%c = t%d;" i (repr_letter r) i)))
              name
              (String.concat
                 ~sep:", "
                 (List.map caller_reprs ~f:(fun r -> Printf.sprintf "(%s)0" (c_type r))))
        | _ -> ""
      in
      line
        ctx
        "{ %s%s%s%s%s %sreturn %s; }"
        (String.concat ~sep:" " temps)
        pop
        bounce
        (pending call)
        through
        (if musttail then "CAMLC_MUSTTAIL " else "")
        (if repr_equal ret ctx.ret
         then call
         else unbox ctx.ret (Printf.sprintf "Camlc_resolve(%s)" call))

let translate_instr ctx traps i =
  match i with
  | Let (x, e) -> (
      let dst = slot ctx x in
      let checked ?(bt = `Raise false) ?(pending = false) call =
        line ctx "r = %s;" call;
        line
          ctx
          "if (CAMLC_UNLIKELY(Is_exception_result(r))%s) %s"
          (* A call: a tail call of the callee may be left pending *)
          (if pending then " && Camlc_raised(r)" else "")
          (raise_stmt ~bt ctx traps "r");
        define ctx x Value "r"
      in
      match e with
      | Apply { f; args; exact } ->
          if is_cps_call ctx x then failwith "c_of_ocaml: a CPS call is not a tail call";
          let k = boxing_prelude ctx traps x ~f ~args ~exact in
          let raises, ret =
            match callee ctx x ~f ~exact with
            | Known g ->
                Var.Set.mem g ctx.out.raising, Var.Hashtbl.find ctx.out.fun_returns g
            | Known_direct _ | Unknown -> true, Value
          in
          (* The typing ensures that an unboxed result is never boxed *)
          if (not (repr_equal ret Value)) && repr_equal (var_repr ctx x) Value
          then failwith "c_of_ocaml: an unboxed result would need to be boxed";
          (* Whether the callee may return a tail call left pending *)
          let pending =
            match callee ctx x ~f ~exact with
            | Known g -> Var.Set.mem g ctx.out.pending
            | Known_direct _ | Unknown -> true
          in
          let callee, args, _ = apply ctx x ~f ~args ~exact in
          let resolve call =
            if pending && repr_equal ret Value
            then Printf.sprintf "Camlc_resolve(%s)" call
            else call
          in
          if not raises
          then
            if k = 0
            then define ctx x ret (resolve (c_call callee args))
            else (
              line ctx "{ %s rv = %s;" (c_type ret) (resolve (c_call callee args));
              line ctx "camlc_sp = bx + %d;" k;
              define ctx x ret "rv";
              line ctx "}")
          else if k = 0
          then checked ~bt:`Call ~pending (c_call callee args)
          else (
            line ctx "r = %s;" (c_call callee args);
            line ctx "camlc_sp = bx + %d;" k;
            line
              ctx
              "if (CAMLC_UNLIKELY(Is_exception_result(r))%s) %s"
              (if pending then " && Camlc_raised(r)" else "")
              (raise_stmt ~bt:`Call ctx traps "r");
            define ctx x Value "r")
      | Block (tag, a, _, _, _) ->
          if Array.length a = 0
          then line ctx "%s = Atom(%d);" dst tag
          else if tag = Obj.double_array_tag
          then (
            let wosize = Array.length a * 8 / word_size () in
            if wosize <= 256
            then line ctx "b = camlc_alloc_small(%d, Double_array_tag);" wosize
            else line ctx "b = caml_alloc_shr(%d, Double_array_tag);" wosize;
            line ctx "%s" (reload ctx (Array.to_list a));
            Array.iteri a ~f:(fun i x ->
                line ctx "Store_double_flat_field(b, %d, %s);" i (as_repr ctx Double x));
            line ctx "%s = b;" dst)
          else
            alloc_block
              ~reload:(reload ctx (Array.to_list a))
              ctx
              ~dst
              ~tag
              ~prefix:[]
              (Array.to_list (Array.map a ~f:(fun x -> as_value ctx x)))
      | Field (y, i, (Non_float | Immediate)) ->
          define ctx x Value (Printf.sprintf "Field(%s, %d)" (as_value ctx y) i)
      | Field (y, i, Float) ->
          define
            ctx
            x
            Double
            (Printf.sprintf "Double_flat_field(%s, %d)" (as_value ctx y) i)
      | Closure _ -> translate_closure ctx x
      | Constant c -> (
          match var_repr ctx x with
          | Value -> line ctx "%s = %s;" dst (code_constant ctx.out c)
          | r -> line ctx "%s = %s;" dst (constant_repr ctx.out c r))
      | Special (Alias_prim name) ->
          failwith (Printf.sprintf "c_of_ocaml: unsupported alias primitive %s" name)
      | Prim (Extern ("caml_cps_closure", _), [ Pv d; Pv _ ]) ->
          (* The closure of the direct-style version holds both versions *)
          define ctx x Value (as_value ctx d)
      | Prim (Extern (name, _), args) -> (
          let args = List.map args ~f:(fun a -> prim_arg ctx a) in
          let n = List.length args in
          match primitive ctx.out name args with
          | Plain (r, e, _) -> define ctx x r e
          | Ccallv ->
              (* More than 5 arguments: the C function takes an array *)
              line ctx "argv = camlc_sp - %d;" n;
              line
                ctx
                "if (CAMLC_UNLIKELY(argv < camlc_stack_limit)) %s"
                (raise_stmt ctx traps "camlc_stack_overflow()");
              line ctx "camlc_sp = argv;";
              List.iteri args ~f:(fun i a -> line ctx "argv[%d] = %s;" i (a.get Value));
              line ctx "r = camlc_ccallv((camlc_primv)%s, argv, %d);" name n;
              line ctx "camlc_sp = argv + %d;" n;
              line
                ctx
                "if (CAMLC_UNLIKELY(Is_exception_result(r))) %s"
                (raise_stmt ~bt:(`Raise false) ctx traps "r");
              define ctx x Value "r"
          | Checked e -> checked e
          | Guarded (check, r, e) ->
              line ctx "r = %s;" check;
              line
                ctx
                "if (CAMLC_UNLIKELY(Is_exception_result(r))) %s"
                (raise_stmt ~bt:(`Raise false) ctx traps "r");
              define ctx x r e)
      | Prim (p, args) -> (
          let args = List.map args ~f:(fun a -> (prim_arg ctx a).get Value) in
          let plain e = define ctx x Value e in
          match p, args with
          | Vectlength Value, [ a ] ->
              plain (Printf.sprintf "Val_long(Camlc_wosize_val(%s))" a)
          | Vectlength Float, [ a ] ->
              plain (Printf.sprintf "Val_long(Camlc_wosize_val(%s) / Double_wosize)" a)
          | Vectlength Generic, [ a ] ->
              plain (Printf.sprintf "Val_long(camlc_array_length(%s))" a)
          | Array_get (Non_float | Immediate | Float), [ a; i ] ->
              plain (Printf.sprintf "Field(%s, Long_val(%s))" a i)
          | Not, [ a ] -> plain (Printf.sprintf "Val_bool(%s == Val_false)" a)
          | IsInt _, [ a ] -> plain (Printf.sprintf "Val_bool(Is_long(%s))" a)
          | Eq _, [ a; b ] -> plain (Printf.sprintf "Val_bool(%s == %s)" a b)
          | Neq _, [ a; b ] -> plain (Printf.sprintf "Val_bool(%s != %s)" a b)
          | Lt, [ a; b ] -> plain (Printf.sprintf "Val_bool((intnat)%s < (intnat)%s)" a b)
          | Le, [ a; b ] ->
              plain (Printf.sprintf "Val_bool((intnat)%s <= (intnat)%s)" a b)
          | Ult, [ a; b ] ->
              plain (Printf.sprintf "Val_bool((uintnat)%s < (uintnat)%s)" a b)
          | (Vectlength _ | Array_get _ | Not | IsInt _ | Eq _ | Neq _ | Lt | Le | Ult), _
            -> assert false
          | Extern _, _ -> assert false))
  | Assign (x, y) -> line ctx "%s = %s;" (slot ctx x) (as_repr ctx (var_repr ctx x) y)
  | Set_field (x, i, (Non_float | Immediate), y) ->
      line ctx "camlc_modify(&Field(%s, %d), %s);" (as_value ctx x) i (as_value ctx y)
  | Set_field (x, i, Float, y) ->
      line
        ctx
        "Store_double_flat_field(%s, %d, %s);"
        (as_value ctx x)
        i
        (as_repr ctx Double y)
  | Offset_ref (x, n) ->
      let x = as_value ctx x in
      line ctx "Field(%s, 0) = (value)((uintnat)Field(%s, 0) + %d);" x x (2 * n)
  | Array_set (x, i, y) ->
      line
        ctx
        "camlc_modify(&Field(%s, Long_val(%s)), %s);"
        (as_value ctx x)
        (as_value ctx i)
        (as_value ctx y)
  | Event pi -> ctx.position <- Some pi

let translate_last ctx traps last =
  match last with
  | Return x -> line ctx "%s" (return_stmt ~r:ctx.ret ctx (as_repr ctx ctx.ret x))
  | Raise (x, kind) -> (
      let bt =
        match kind with
        | `Normal -> backtrace_stmt ctx (`Raise false) (as_value ctx x)
        | `Reraise -> backtrace_stmt ctx (`Raise true) (as_value ctx x)
        | `Notrace -> ""
      in
      if not (String.equal bt "") then line ctx "%s" bt;
      match traps with
      | [] ->
          line
            ctx
            "%s"
            (return_stmt
               ctx
               (Printf.sprintf "Make_exception_result(%s)" (as_value ctx x)))
      | { exn_var; id; _ } :: _ ->
          line ctx "%s = %s; goto H%d;" (slot ctx exn_var) (as_value ctx x) id)
  | Stop -> line ctx "%s" (return_stmt ctx "Val_unit")
  | Branch cont -> line ctx "%s" (branch ctx cont)
  | Cond (x, c1, c2) ->
      line ctx "if (%s != Val_false) %s" (as_value ctx x) (branch ctx c1);
      line ctx "else %s" (branch ctx c2)
  | Switch (x, conts) ->
      line ctx "switch (Long_val(%s)) {" (as_value ctx x);
      let n = Array.length conts in
      Array.iteri conts ~f:(fun i cont ->
          if i = n - 1
          then line ctx "default: %s" (branch ctx cont)
          else line ctx "case %d: %s" i (branch ctx cont));
      line ctx "}"
  | Pushtrap (cont, _, _) -> line ctx "%s" (branch ctx cont)
  | Poptrap cont -> line ctx "%s" (branch ctx cont)

(* The blocks of a function, and the handlers in scope in each of them *)
let function_blocks ctx pc =
  let traps = ctx.traps in
  let next_id = ref 0 in
  let order = ref [] in
  let rec visit pc t =
    match Addr.Hashtbl.find_opt traps pc with
    | Some t' ->
        if List.length t <> List.length t'
        then failwith "c_of_ocaml: inconsistent exception handlers"
    | None -> (
        Addr.Hashtbl.add traps pc t;
        order := pc :: !order;
        let block = Addr.Map.find pc ctx.blocks in
        match block.branch with
        | Return _ | Raise _ | Stop -> ()
        | Branch (pc', _) -> visit pc' t
        | Poptrap (pc', _) -> (
            match t with
            | _ :: t' -> visit pc' t'
            | [] -> failwith "c_of_ocaml: unbalanced exception handlers")
        | Cond (_, (pc1, _), (pc2, _)) ->
            visit pc1 t;
            visit pc2 t
        | Switch (_, conts) -> Array.iter conts ~f:(fun (pc', _) -> visit pc' t)
        | Pushtrap ((pc1, _), x, (pc2, _)) ->
            let id = !next_id in
            incr next_id;
            Addr.Hashtbl.add ctx.handler_ids pc id;
            visit pc1 ({ exn_var = x; id } :: t);
            visit pc2 t)
  in
  visit pc [];
  List.rev !order
