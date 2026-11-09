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

(* The frame of a function: liveness, the points where the GC may run, the
   polls, the variables kept in the frame and their slots *)

open! Stdlib
open Code
open! C_repr
open! C_output
open! C_primitives
open! C_facts
open! C_translate

(* Liveness, to find the variables which must survive a garbage
   collection: they are kept in the frame of the function *)

let use_prim_args ctx l acc =
  List.fold_left l ~init:acc ~f:(fun acc a ->
      match a with
      | Pv x -> use ctx x acc
      | Pc _ -> acc)

let expr_uses ctx x e acc =
  match e with
  | Apply { f; args; _ } -> use ctx f (use_list ctx args acc)
  | Block (_, a, _, _, _) -> use_list ctx (Array.to_list a) acc
  | Field (y, _, _) -> use ctx y acc
  | Closure _ -> (
      match Var.Map.find_opt x ctx.closures with
      | Some info -> use_list ctx info.free_variables acc
      | None -> acc)
  | Constant _ | Special _ -> acc
  | Prim (Extern ("caml_cps_closure", _), [ Pv d; _ ]) -> use ctx d acc
  | Prim (_, args) -> use_prim_args ctx args acc

(* The primitives which may trigger a garbage collection only when they
   raise (to allocate the exception) *)
let gc_on_raise_primitives =
  StringSet.of_list
    [ "caml_string_get"
    ; "caml_bytes_get"
    ; "caml_string_set"
    ; "caml_bytes_set"
    ; "caml_check_bound"
    ; "caml_check_bound_float"
    ; "caml_check_bound_gen"
    ]

(* How an instruction may trigger a garbage collection: [`Call] if its
   operands are read before, [`Alloc] if they are read after, [`Raise] if
   only when it raises *)
let gc_point ctx i =
  match i with
  | Let (x, Apply { f; args; exact }) -> (
      if List.exists (boxed_arguments ctx x ~f ~args ~exact) ~f:Fun.id
      then `Alloc
      else
        match callee ctx x ~f ~exact with
        | Known g when not (is_cps_call ctx x) ->
            if Var.Set.mem g ctx.out.collecting
            then `Call
            else if Var.Set.mem g ctx.out.collecting_on_raise
            then `Raise
            else `No
        | Known _ | Known_direct _ | Unknown -> `Call)
  | Let (_, Block (_, [||], _, _, _)) -> `No
  | Let (_, Block _) -> `Alloc
  | Let (x, Closure _) -> (
      match Var.Map.find_opt x ctx.closures with
      | Some { functions = (f0, _) :: _; free_variables = _ :: _; _ } when Var.equal x f0
        -> `Alloc
      | Some _ | None -> `No)
  | Let (_, Prim (Extern ("caml_cps_closure", _), _)) -> `No
  | Let (x, Field (_, _, Float)) ->
      (* The float is boxed if the variable holds a value *)
      if repr_equal (var_repr ctx x) Value then `Call else `No
  | Let (x, Prim (Extern (name, _), args)) -> (
      let dummy a = { (prim_arg ctx a) with get = (fun _ -> "_") } in
      let dummy a =
        match a with
        | Pc c ->
            { a_typ = Typing.constant_type c; a_const = Some c; get = (fun _ -> "_") }
        | Pv _ -> dummy a
      in
      match primitive ctx.out name (List.map args ~f:dummy) with
      | Plain (r, _, gc) ->
          if gc || ((not (repr_equal r Value)) && repr_equal (var_repr ctx x) Value)
          then `Call
          else `No
      | Checked _ ->
          if StringSet.mem name nogc_primitives
          then `No
          else if StringSet.mem name gc_on_raise_primitives
          then `Raise
          else `Call
      | Guarded (_, r, _) ->
          (* The result is boxed after the check *)
          if (not (repr_equal r Value)) && repr_equal (var_repr ctx x) Value
          then `Call
          else `Raise
      | Ccallv -> `Call)
  | Let (_, Prim ((Vectlength Generic | Array_get Float), _)) -> `No
  | Let (_, (Field _ | Constant _ | Special _ | Prim _)) -> `No
  | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> `No

let instr_defs i =
  match i with
  | Let (x, _) | Assign (x, _) -> Some x
  | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> None

let instr_uses ctx i acc =
  match i with
  | Let (x, e) -> expr_uses ctx x e acc
  | Assign (_, y) -> use ctx y acc
  | Set_field (x, _, _, y) -> use ctx x (use ctx y acc)
  | Offset_ref (x, _) -> use ctx x acc
  | Array_set (x, y, z) -> use ctx x (use ctx y (use ctx z acc))
  | Event _ -> acc

let cont_uses ctx live_in (pc, args) acc =
  let params = (Addr.Map.find pc ctx.blocks).params in
  let l = Addr.Hashtbl.find_opt live_in pc |> Option.value ~default:Var.Set.empty in
  use_list
    ctx
    args
    (Var.Set.union acc (List.fold_left params ~init:l ~f:(fun l x -> Var.Set.remove x l)))

let rec live_out ctx live_in pc =
  let block = Addr.Map.find pc ctx.blocks in
  let acc =
    match block.branch with
    | Return x | Raise (x, _) -> use ctx x Var.Set.empty
    | Stop -> Var.Set.empty
    | Branch cont | Poptrap cont | Pushtrap (cont, _, _) ->
        cont_uses ctx live_in cont Var.Set.empty
    | Cond (x, c1, c2) ->
        use ctx x (cont_uses ctx live_in c1 (cont_uses ctx live_in c2 Var.Set.empty))
    | Switch (x, conts) ->
        use
          ctx
          x
          (Array.fold_left conts ~init:Var.Set.empty ~f:(fun acc c ->
               cont_uses ctx live_in c acc))
  in
  (* The handler of the innermost trap may be reached from any point of
     the block *)
  Var.Set.union acc (handler_live ctx live_in pc)

(* The variables live at the start of the handler of the innermost trap
   of a block *)
and handler_live ctx live_in pc =
  match Addr.Hashtbl.find ctx.traps pc with
  | [] -> Var.Set.empty
  | { exn_var; id } :: _ ->
      let handler =
        Addr.Hashtbl.fold
          (fun pc' id' h ->
            if id = id'
            then
              match (Addr.Map.find pc' ctx.blocks).branch with
              | Pushtrap (_, _, h) -> Some h
              | _ -> h
            else h)
          ctx.handler_ids
          None
      in
      let h =
        match handler with
        | Some h -> h
        | None -> assert false
      in
      Var.Set.remove exn_var (cont_uses ctx live_in h Var.Set.empty)

(* Polls (see [Camlc_poll_needed] in camlc.h), when the program uses
   threads or domains: the code must let the other threads run, and take
   part in the collections of the other domains, even when it does not
   allocate. A function polls at the start of its loops, and at its start
   if it makes a tail call, which may loop as well. With OCaml 5, an
   allocation of the minor heap polls already, but for the exceptions of
   the asynchronous actions it runs, raised at the next poll (see
   [camlc_alloc_small_gc] in camlc_runtime.c): there is no poll at the start
   of a loop or of a function whose first block allocates, unless the
   program uses [Gc.Memprof], whose callbacks may raise to interrupt a
   computation. Returns whether it polls at its start, and the start of its
   loops. *)
let polls out blocks pc =
  if not out.polling
  then false, Addr.Set.empty
  else
    let allocates pc =
      Ocaml_version.compare Ocaml_version.current [ 5; 0 ] >= 0
      && (not out.memprof)
      && List.exists (Addr.Map.find pc blocks).body ~f:(fun i ->
          match i with
          | Let (_, Block (_, a, _, _, _)) ->
              (* Allocated in the minor heap, even with unboxed floats *)
              Array.length a > 0 && Array.length a <= 128
          | _ -> false)
    in
    let state = Addr.Hashtbl.create 16 in
    let headers = ref Addr.Set.empty in
    let tail_call = ref false in
    let rec visit pc =
      match Addr.Hashtbl.find_opt state pc with
      | Some true -> if not (allocates pc) then headers := Addr.Set.add pc !headers
      | Some false -> ()
      | None ->
          Addr.Hashtbl.add state pc true;
          let block = Addr.Map.find pc blocks in
          (match List.rev block.body, block.branch with
          | Let (x, Apply _) :: _, Return y when Var.equal x y -> tail_call := true
          | _ -> ());
          Code.fold_children blocks pc (fun pc' () -> visit pc') ();
          Addr.Hashtbl.replace state pc false
    in
    visit pc;
    !tail_call && not (allocates pc), !headers

(* The program samples its allocations ([Gc.Memprof]): the exceptions of
   the callbacks, which an allocation runs, are raised at the polls (see
   [camlc_alloc_small_gc] in camlc_runtime.c) *)
let uses_memprof (p : program) =
  Addr.Map.fold
    (fun _ block acc ->
      acc
      || List.exists block.body ~f:(fun i ->
          match i with
          | Let (_, Prim (Extern ("caml_memprof_start", _), _)) -> true
          | _ -> false))
    p.blocks
    false

let uses_threads (p : program) =
  Addr.Map.fold
    (fun _ block acc ->
      acc
      || List.exists block.body ~f:(fun i ->
          match i with
          | Let
              ( _
              , Prim
                  ( Extern
                      ( ( "caml_thread_initialize"
                        | "caml_thread_new"
                        | "caml_domain_spawn" )
                      , _ )
                  , _ ) ) -> true
          | _ -> false))
    p.blocks
    false

(* Whether a variable may hold a pointer to the heap *)
let gc_value ctx x =
  if Var.equal x ctx.env_var
  then not ctx.static_env
  else
    repr_equal (var_repr ctx x) Value
    && (match Typing.var_type ctx.out.types x with
      | Int _ -> false
      | _ -> true)
    && not
         (BitSet.mem ctx.facts.is_int (Var.idx x)
         || BitSet.mem ctx.facts.is_static (Var.idx x))

(* A poll: the values of the C variables [vars] are saved on the shadow
   stack meanwhile (in the reserve below the limit, if needed); [raise]
   raises the exception result [r] *)
let poll_stmt vars ~raise =
  let n = List.length vars in
  let save =
    if n = 0
    then ""
    else
      Printf.sprintf
        "value *pp = camlc_sp - %d; camlc_sp = pp; %s "
        n
        (String.concat
           ~sep:" "
           (List.mapi vars ~f:(fun i x -> Printf.sprintf "pp[%d] = %s;" i x)))
  in
  let restore =
    if n = 0
    then ""
    else
      Printf.sprintf
        " %s camlc_sp = pp + %d;"
        (String.concat
           ~sep:" "
           (List.mapi vars ~f:(fun i x -> Printf.sprintf "%s = pp[%d];" x i)))
        n
  in
  Printf.sprintf
    "if (CAMLC_UNLIKELY(Camlc_poll_needed())) { %sr = camlc_poll();%s if \
     (CAMLC_UNLIKELY(Is_exception_result(r))) %s }"
    save
    restore
    raise

(* The variables which must be kept in the frame *)
let rooted_variables ctx pcs ~params =
  let live_in = Addr.Hashtbl.create 64 in
  let block_live_in pc live =
    let block = Addr.Map.find pc ctx.blocks in
    List.fold_right block.body ~init:live ~f:(fun i live ->
        let live =
          match instr_defs i with
          | Some x -> Var.Set.remove x live
          | None -> live
        in
        instr_uses ctx i live)
  in
  let changed = ref true in
  let rpcs = List.rev pcs in
  while !changed do
    changed := false;
    List.iter rpcs ~f:(fun pc ->
        let l = block_live_in pc (live_out ctx live_in pc) in
        match Addr.Hashtbl.find_opt live_in pc with
        | Some l' when Var.Set.equal l l' -> ()
        | _ ->
            Addr.Hashtbl.replace live_in pc l;
            changed := true)
  done;
  let rooted = ref Var.Set.empty in
  List.iter pcs ~f:(fun pc ->
      let block = Addr.Map.find pc ctx.blocks in
      ignore
        (List.fold_right block.body ~init:(live_out ctx live_in pc) ~f:(fun i live ->
             let def = instr_defs i in
             let live_after =
               match def with
               | Some x -> Var.Set.remove x live
               | None -> live
             in
             (match gc_point ctx i with
             | `No -> ()
             | `Raise -> rooted := Var.Set.union !rooted (handler_live ctx live_in pc)
             | `Call -> rooted := Var.Set.union !rooted live_after
             | `Alloc ->
                 let l = instr_uses ctx i live_after in
                 rooted :=
                   Var.Set.union
                     !rooted
                     (match def with
                     | Some x -> Var.Set.remove x l
                     | None -> l));
             instr_uses ctx i live_after)));
  ignore params;
  Var.Set.filter (fun x -> gc_value ctx x) !rooted, live_in

(* The slots of the rooted variables: two variables live at the same time
   have different slots. A variable interferes with the variables live
   after its definition, and the parameters of the function with each other
   and the variables live at its start. A slot may still hold a dead value,
   which remains valid as it was a root. *)
let assign_slots ctx pcs ~params ~entry rooted live_in =
  let edges = Var.Hashtbl.create 64 in
  let add x y =
    if not (Var.equal x y)
    then (
      Var.Hashtbl.replace
        edges
        x
        (Var.Set.add
           y
           (Var.Hashtbl.find_opt edges x |> Option.value ~default:Var.Set.empty));
      Var.Hashtbl.replace
        edges
        y
        (Var.Set.add
           x
           (Var.Hashtbl.find_opt edges y |> Option.value ~default:Var.Set.empty)))
  in
  let define x live =
    if Var.Set.mem x rooted
    then Var.Set.iter (fun y -> add x y) (Var.Set.inter live rooted)
  in
  let live_at pc =
    Addr.Hashtbl.find_opt live_in pc |> Option.value ~default:Var.Set.empty
  in
  let start =
    List.fold_left
      (ctx.env_var :: params)
      ~init:(Var.Set.union (live_at (fst entry)) (use_list ctx (snd entry) Var.Set.empty))
      ~f:(fun acc x -> if Var.Set.mem x rooted then Var.Set.add x acc else acc)
  in
  List.iter (ctx.env_var :: params) ~f:(fun x -> define x start);
  List.iter pcs ~f:(fun pc ->
      let block = Addr.Map.find pc ctx.blocks in
      let l = live_at pc in
      List.iter block.params ~f:(fun x -> define x l);
      (match Addr.Hashtbl.find ctx.traps pc with
      | { exn_var; _ } :: _ -> define exn_var (handler_live ctx live_in pc)
      | [] -> ());
      ignore
        (List.fold_right block.body ~init:(live_out ctx live_in pc) ~f:(fun i live ->
             (* The functions of a closure are all defined with the first
                one *)
             (match i with
             | Let (x, Closure _) -> (
                 match Var.Map.find_opt x ctx.closures with
                 | Some { functions = (f0, _) :: _ as functions; _ } when Var.equal x f0
                   ->
                     let live =
                       List.fold_left functions ~init:live ~f:(fun live (g, _) ->
                           if Var.Set.mem g rooted then Var.Set.add g live else live)
                     in
                     List.iter functions ~f:(fun (g, _) -> define g live)
                 | Some _ | None -> ())
             | _ -> ());
             let live_after =
               match instr_defs i with
               | Some x ->
                   define x live;
                   Var.Set.remove x live
               | None -> live
             in
             instr_uses ctx i live_after)));
  (* Greedy coloring, the parameters first *)
  let order =
    List.filter (ctx.env_var :: params) ~f:(fun x -> Var.Set.mem x rooted)
    @ Var.Set.elements
        (List.fold_left (ctx.env_var :: params) ~init:rooted ~f:(fun s x ->
             Var.Set.remove x s))
  in
  List.iter order ~f:(fun x ->
      let used =
        Var.Set.fold
          (fun y acc ->
            match Var.Hashtbl.find_opt ctx.slots y with
            | Some i -> IntSet.add i acc
            | None -> acc)
          (Var.Hashtbl.find_opt edges x |> Option.value ~default:Var.Set.empty)
          IntSet.empty
      in
      let rec first i = if IntSet.mem i used then first (i + 1) else i in
      Var.Hashtbl.add ctx.slots x (first 0))
