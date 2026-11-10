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

(* Translation of the intermediate code into C, for the runtime described
   in runtime/c/camlc.h.

   Each function of the program becomes a C function. Its variables are C
   variables; those live where the GC may run are also stored in a frame
   on the shadow stack ([fp[i]]), so that they are seen and updated by the
   GC, and reloaded from it after. The variables of the enclosing functions
   are read from the closure. Blocks are labels, and branches are gotos.
   An exception is returned as an exception result: each call is followed
   by a check, which jumps to the handler of the innermost [Pushtrap] of
   the function or returns the exception. *)

open! Stdlib
open Code
open! C_repr
open! C_output
open! C_primitives
open! C_facts
open! C_translate
open! C_frame
open! C_summary
open! C_function
open! C_naming
open! C_toplevel

type mode =
  [ `Program
  | `Dynlink
  | `Plugin
  ]

type result =
  { files : string list
  ; primitives : (string * int) list
  }

(* The maximal arity of the functions of the plugins of a program compiled
   with --dynlink (the runtime handles their partial applications with the
   tables of the program) *)
let dynlink_max_arity = 64

(* The names of the compilation units defined by [p], and of those it
   refers to *)
let units (p : program) =
  let defined = String.Hashtbl.create 16 in
  let imported = String.Hashtbl.create 16 in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let
              ( _
              , Prim
                  ( Extern ("caml_register_global", _)
                  , [ _; Pc (String n | NativeString (Byte n | Utf (Utf8 n))) ] ) ) ->
              String.Hashtbl.replace defined n ()
          | Let
              ( _
              , Prim
                  ( Extern ("caml_get_global", _)
                  , [ Pc (String n | NativeString (Byte n | Utf (Utf8 n))) ] ) ) ->
              String.Hashtbl.replace imported n ()
          | _ -> ()))
    p.blocks;
  let names t =
    List.sort ~cmp:String.compare (String.Hashtbl.fold (fun n () acc -> n :: acc) t [])
  in
  names defined, names imported

let f
    (p : program)
    ?(mode : mode = `Program)
    ?(interfaces = [])
    ~global_flow_data
    ~deadcode_sentinel
    ~debug_info
    ~in_cps
    ~trampolined_calls
    ?(toplevel_chunk_size = 2000)
    ?part_size
    closures =
  let global_flow_state, global_flow_info = global_flow_data in
  (* Toplevel values in globals: they are no longer free variables of the
     closures *)
  let globals = Var.Hashtbl.create 1024 in
  let global_list =
    Var.Set.elements (Globalize.f p (Structure.build_graph p.blocks p.start) closures)
  in
  name_globals p global_list globals;
  (* Nor are the constants and the closed closures, static data (and the
     variables bound to the closure of a function with two versions, see
     [caml_cps_closure]), which may close other closures *)
  let constants = Var.Hashtbl.create 1024 in
  let assigned = Var.Hashtbl.create 16 in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, Constant c) -> Var.Hashtbl.replace constants x c
          | Assign (x, _) -> Var.Hashtbl.replace assigned x ()
          | _ -> ()))
    p.blocks;
  Var.Hashtbl.iter (fun x () -> Var.Hashtbl.remove constants x) assigned;
  let closed = Var.Hashtbl.create 1024 in
  Var.Hashtbl.iter (fun x _ -> Var.Hashtbl.replace closed x ()) constants;
  let rec close_closures closures =
    Var.Map.iter
      (fun _ (info : Closure_conversion.closure) ->
        if List.is_empty info.free_variables
        then List.iter info.functions ~f:(fun (g, _) -> Var.Hashtbl.replace closed g ()))
      closures;
    Var.Hashtbl.iter
      (fun x (d, _) -> if Var.Hashtbl.mem closed d then Var.Hashtbl.replace closed x ())
      global_flow_info.Global_flow.info_cps_closures;
    let changed = ref false in
    let closures =
      Var.Map.map
        (fun (info : Closure_conversion.closure) ->
          let free_variables =
            List.filter info.free_variables ~f:(fun x ->
                not (Var.Hashtbl.mem globals x || Var.Hashtbl.mem closed x))
          in
          if List.length free_variables <> List.length info.free_variables
          then changed := true;
          { info with free_variables })
        closures
    in
    if !changed then close_closures closures else closures
  in
  let closures = close_closures closures in
  let facts = compute_facts p closures in
  let cps_pairs, cps_sides, public_name =
    Var.Hashtbl.fold
      (fun x (d, c) (cps_pairs, cps_sides, public_name) ->
        ( Var.Map.add d c cps_pairs
        , Var.Set.add c cps_sides
        , Var.Map.add d x (Var.Map.add c x public_name) ))
      global_flow_info.Global_flow.info_cps_closures
      (Var.Map.empty, Var.Set.empty, Var.Map.empty)
  in
  let located_functions = collect_functions p in
  let functions =
    List.map located_functions ~f:(fun (f, params, cont, _) -> f, params, cont)
  in
  (* Which numbers can be unboxed. Closures hold values, and so do the
     parameters of the CPS versions, which the runtime may call. *)
  Typing.reset ();
  register_typing_primitives ();
  Typing.prepare ~global_flow_state p;
  let fun_info = Call_graph_analysis.f p global_flow_info in
  let boxed =
    Var.Map.fold
      (fun _ (info : Closure_conversion.closure) acc -> info.free_variables @ acc)
      closures
      global_list
  in
  let boxed =
    List.fold_left functions ~init:boxed ~f:(fun acc (f, params, _) ->
        if Var.Set.mem f cps_sides then params @ acc else acc)
  in
  let typing ~unboxed_returns =
    Typing.f
      ~global_flow_state
      ~global_flow_info
      ~fun_info
      ~unboxed_returns
      ~deadcode_sentinel
      ~boxed
      p
  in
  (* First with boxed return values, to find the functions which cannot
     raise (see below) *)
  let types = typing ~unboxed_returns:(fun _ -> false) in
  let out =
    { data = Buffer.create 65536
    ; static_names = String.Hashtbl.create 1024
    ; static_list = []
    ; definitions = []
    ; refs = Var.Set.empty
    ; constants = Int.Hashtbl.create 1024
    ; primitives = String.Hashtbl.create 128
    ; apply_arities = IntSet.empty
    ; max_arity = 1
    ; global_data_size = global_data_size p
    ; fun_names = Var.Hashtbl.create 1024
    ; types
    ; fun_reprs = Var.Hashtbl.create 1024
    ; fun_returns = Var.Hashtbl.create 1024
    ; debug_info
    ; locations = String.Hashtbl.create 1024
    ; segment_thunks = String.Hashtbl.create 16
    ; cps_fallback = Var.Set.empty
    ; components = Var.Hashtbl.create 16
    ; unknown_tail_calls = Var.Set.empty
    ; pending = Var.Set.empty
    ; in_cps
    ; trampolined = trampolined_calls
    ; cps_pairs
    ; cps_sides
    ; cps_apply_arities = IntSet.empty
    ; globals
    ; statics = Var.Hashtbl.create 1024
    ; split = Option.is_some part_size
    ; polling = uses_threads p || uses_memprof p
    ; memprof = uses_memprof p
    ; raising = Var.Set.empty
    ; collecting = Var.Set.empty
    ; collecting_on_raise = Var.Set.empty
    }
  in
  name_functions out p.blocks located_functions;
  let set_reprs () =
    List.iter functions ~f:(fun (f, params, _) ->
        Var.Hashtbl.replace
          out.fun_reprs
          f
          (List.map params ~f:(fun x -> repr_of_type (Typing.var_type out.types x)));
        Var.Hashtbl.replace
          out.fun_returns
          f
          (repr_of_type (Typing.return_type out.types f)))
  in
  set_reprs ();
  List.iter functions ~f:(fun (f, params, _) ->
      out.max_arity <- max out.max_arity (List.length params);
      ignore (fun_name out f));
  let functions_max_arity = out.max_arity in
  (match mode with
  | `Dynlink -> out.max_arity <- max out.max_arity dynlink_max_arity
  | `Program | `Plugin -> ());
  (* The static data of the constants and the closed closures *)
  Var.Hashtbl.iter
    (fun x c -> Var.Hashtbl.replace out.statics x (code_constant out c))
    constants;
  Var.Map.iter
    (fun f (info : Closure_conversion.closure) ->
      match info.functions with
      | (f0, _) :: _
        when Var.equal f f0
             && List.is_empty info.free_variables
             && not (Var.Set.mem f cps_sides) ->
          List.iter (static_closure out info.functions) ~f:(fun (g, e) ->
              Var.Hashtbl.replace out.statics g e)
      | _ -> ())
    closures;
  Var.Hashtbl.iter
    (fun x (d, _) ->
      match Var.Hashtbl.find_opt out.statics d with
      | Some e -> Var.Hashtbl.replace out.statics x e
      | None -> ())
    global_flow_info.Global_flow.info_cps_closures;
  let data_refs = out.refs in
  analyze_functions out facts global_flow_info closures p.blocks functions;
  (* The functions which cannot raise may return unboxed numbers (when they
     are only called directly), except those with two versions and those
     called in direct style by CPS code, whose result goes to a
     continuation. Whether a function raises does not depend on the
     boxing of numbers. *)
  let direct_style =
    Addr.Map.fold
      (fun _ block acc ->
        List.fold_left block.body ~init:acc ~f:(fun acc i ->
            match i with
            | Let (x, Apply { f; exact; _ }) -> (
                match resolve_callee global_flow_info closures x ~f ~exact with
                | Known_direct g -> Var.Set.add g acc
                | Known _ | Unknown -> acc)
            | _ -> acc))
      p.blocks
      Var.Set.empty
  in
  let unboxed_returns f =
    not
      (Var.Set.mem f out.raising
      || Var.Set.mem f cps_sides
      || Var.Map.mem f cps_pairs
      || Var.Set.mem f direct_style)
  in
  if List.exists functions ~f:(fun (f, _, _) -> unboxed_returns f)
  then (
    out.types <- typing ~unboxed_returns;
    set_reprs ();
    analyze_functions out facts global_flow_info closures p.blocks functions);
  List.iter functions ~f:(fun (f, params, cont) ->
      let info = Var.Map.find f closures in
      let startenv, positions = closure_layout info.Closure_conversion.functions in
      let k =
        let rec find l pos =
          match l, pos with
          | (g, _) :: _, pos :: _ when Var.equal g f -> pos
          | _ :: r, _ :: rpos -> find r rpos
          | _ -> assert false
        in
        find info.functions positions
      in
      let env =
        List.mapi info.free_variables ~f:(fun j x -> x, Env (startenv - k + j))
        @ List.concat
            (List.map2 info.functions positions ~f:(fun (g, _) pos ->
                 (* The code refers to a function with two versions by the
                    variable bound to its closure *)
                 let l = Sibling (pos - k) in
                 match Var.Map.find_opt g public_name with
                 | Some x -> [ g, l; x, l ]
                 | None -> [ g, l ]))
      in
      translate_function
        out
        facts
        global_flow_info
        p.blocks
        closures
        ~fn:(Some f)
        ~name:
          (Printf.sprintf
             "%s %s"
             (c_type (Var.Hashtbl.find out.fun_returns f))
             (fun_name out f))
        ~params
        ~param_reprs:(Var.Hashtbl.find out.fun_reprs f)
        ~env
        ~static_env:(List.is_empty info.free_variables)
        ~source_name:(Var.get_name f)
        ~raises:(Var.Set.mem f out.raising)
        cont);
  let toplevel_name =
    match mode with
    | `Program | `Dynlink -> "camlc_program"
    | `Plugin -> "camlc_plugin_program"
  in
  let translate_toplevel ~name blocks pc =
    translate_function
      out
      facts
      global_flow_info
      blocks
      closures
      ~fn:None
      ~name
      ~params:[]
      ~param_reprs:[]
      ~env:[]
      ~static_env:true
      ~source_name:None
      ~raises:true
      (pc, [])
  in
  (match
     toplevel_chunks
       out
       facts
       global_flow_info
       closures
       p.blocks
       ~toplevel_chunk_size
       ~free_pc:p.free_pc
       p.start
   with
  | [] -> translate_toplevel ~name:("value " ^ toplevel_name) p.blocks p.start
  | chunks ->
      List.iter chunks ~f:(fun (name, pc, blocks) ->
          translate_toplevel ~name:("__attribute__((noinline)) value " ^ name) blocks pc);
      let b = Buffer.create 4096 in
      List.iter chunks ~f:(fun (name, _, _) -> Printf.bprintf b "value %s(void);\n" name);
      Printf.bprintf b "\nvalue %s(void) {\n  value r;\n  (void)r;\n" toplevel_name;
      let n = List.length chunks in
      List.iteri chunks ~f:(fun i (name, _, _) ->
          if i = n - 1
          then Printf.bprintf b "  return %s();\n" name
          else
            Printf.bprintf
              b
              "  r = Camlc_resolve(%s());\n\
              \  if (CAMLC_UNLIKELY(Is_exception_result(r))) return r;\n"
              name);
      Buffer.add_string b "}\n\n";
      out.definitions <-
        { def_fn = None
        ; def_name = toplevel_name
        ; def_text = Buffer.contents b
        ; def_size = Buffer.length b
        ; def_refs = Var.Set.empty
        }
        :: out.definitions);
  (* Assemble the output. Split, the functions are grouped in order into
     parts of about [part_size] bytes each, after a part with the static
     data and the tables. A function is static unless another part, or the
     static data, refers to it. *)
  let definitions = List.rev out.definitions in
  let parts =
    match part_size with
    | None -> [ definitions ]
    | Some size ->
        (* A part ends after a definition chosen by the hash of its name,
           with a probability proportional to its size, once the part has
           half the size: the parts after a definition which changes are
           left unchanged *)
        let ends_part d =
          Hashtbl.hash d.def_name land 0xfffff < d.def_size * 0x100000 / max 1 (size / 2)
        in
        let rec group parts part n l =
          match l with
          | [] -> List.rev (if List.is_empty part then parts else List.rev part :: parts)
          | d :: r ->
              if n > 0 && n + d.def_size > 2 * size
              then group (List.rev part :: parts) [] 0 l
              else
                let n = n + d.def_size in
                if n >= size / 2 && ends_part d
                then group (List.rev (d :: part) :: parts) [] 0 r
                else group parts (d :: part) n r
        in
        group [] [] 0 definitions
  in
  let split = Option.is_some part_size in
  let shared = Var.Hashtbl.create 1024 in
  if split
  then (
    let part_of = Var.Hashtbl.create 1024 in
    List.iteri parts ~f:(fun i l ->
        List.iter l ~f:(fun d ->
            Option.iter d.def_fn ~f:(fun f -> Var.Hashtbl.replace part_of f i)));
    Var.Set.iter (fun f -> Var.Hashtbl.replace shared f ()) data_refs;
    List.iteri parts ~f:(fun i l ->
        List.iter l ~f:(fun d ->
            Var.Set.iter
              (fun g ->
                match Var.Hashtbl.find_opt part_of g with
                | Some j when j = i -> ()
                | _ -> Var.Hashtbl.replace shared g ())
              d.def_refs)));
  let storage f = if Var.Hashtbl.mem shared f then "" else "static " in
  let dt = double_translation () in
  (* The locations, constants and globals that some code refers to: the
     identifiers of its text starting with [camlc_l_], [camlc_k_] and
     [camlc_g_] *)
  let symbols texts =
    let is_ident c =
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
      | _ -> false
    in
    let found = String.Hashtbl.create 64 in
    List.iter texts ~f:(fun text ->
        let n = String.length text in
        let i = ref 0 in
        while !i + 8 <= n do
          if
            Char.equal text.[!i] 'c'
            && (!i = 0 || not (is_ident text.[!i - 1]))
            && String.equal (String.sub text ~pos:!i ~len:6) "camlc_"
            && Char.equal text.[!i + 7] '_'
            &&
            match text.[!i + 6] with
            | 'l' | 'k' | 'g' | 's' | 'c' | 't' -> true
            | _ -> false
          then (
            let j = ref (!i + 8) in
            while !j < n && is_ident text.[!j] do
              incr j
            done;
            String.Hashtbl.replace found (String.sub text ~pos:!i ~len:(!j - !i)) ();
            i := !j)
          else incr i
        done);
    List.sort
      ~cmp:String.compare
      (String.Hashtbl.fold (fun name () acc -> name :: acc) found [])
  in
  let declare_symbols b names =
    List.iter names ~f:(fun name ->
        match name.[6] with
        | 'l' ->
            Printf.bprintf
              b
              "static const struct camlc_location %s = %s;\n"
              name
              (String.Hashtbl.find out.locations name)
        | 'k' -> Printf.bprintf b "extern value %s[];\n" name
        | 'c' ->
            (* camlc_c_N: applies the CPS version of a function of arity N
               (see [camlc_cps_stack]). Not inlined, its arguments in an
               array on the shadow stack: an array in a C variable would
               take room on emscripten's stack, which the segmented stack
               does not measure nor switch *)
            let n =
              int_of_string (String.sub name ~pos:8 ~len:(String.length name - 8))
            in
            let args = List.init ~len:n ~f:(fun i -> Printf.sprintf "a%d" i) in
            Printf.bprintf
              b
              "static __attribute__((noinline)) value %s(value f, %s) { value *args = \
               camlc_sp - %d, r; if (CAMLC_UNLIKELY(args < camlc_stack_limit)) return \
               camlc_stack_overflow(); camlc_sp = args; %s r = camlc_cps_stack(f, %d, \
               args); camlc_sp = args + %d; return r; }\n"
              name
              (String.concat ~sep:", " (List.map args ~f:(fun a -> "value " ^ a)))
              n
              (String.concat
                 ~sep:" "
                 (List.mapi args ~f:(fun i a -> Printf.sprintf "args[%d] = %s;" i a)))
              n
              n
        | 's' ->
            (* camlc_s_R_ARGS: calls a function of result R and arguments
               ARGS (see [repr_letter]) on a new segment, through its
               thunk, which finds them in [camlc_seg] *)
            let c_type c =
              match c with
              | 'v' -> "value"
              | 'd' -> "double"
              | 'l' -> "int64_t"
              | 'i' -> "int32_t"
              | _ -> "intnat"
            in
            let args = String.sub name ~pos:10 ~len:(String.length name - 10) in
            let args = List.init ~len:(String.length args) ~f:(fun i -> args.[i]) in
            let ret = name.[8] in
            Printf.bprintf
              b
              "static void %s_thunk(void) { camlc_seg[0].%c = ((%s \
               (*)(%s))camlc_seg_fn)(%s); }\n\
               static __attribute__((noinline)) %s %s(%s) { %s camlc_seg_fn = f; \
               camlc_segment(%s_thunk, words); return camlc_seg[0].%c; }\n"
              name
              ret
              (c_type ret)
              (String.concat ~sep:", " (List.map args ~f:c_type))
              (String.concat
                 ~sep:", "
                 (List.mapi args ~f:(fun i c ->
                      Printf.sprintf "camlc_seg[%d].%c" (i + 1) c)))
              (c_type ret)
              name
              (String.concat
                 ~sep:", "
                 ("void *f"
                 :: "mlsize_t words"
                 :: List.mapi args ~f:(fun i c -> Printf.sprintf "%s a%d" (c_type c) i)))
              (String.concat
                 ~sep:" "
                 (List.mapi args ~f:(fun i c ->
                      Printf.sprintf "camlc_seg[%d].%c = a%d;" (i + 1) c i)))
              name
              ret
        | 't' ->
            (* camlc_t_R_CALLER_CALLEE: a function of the signature of a
               caller (result R and arguments CALLER, see [repr_letter])
               which calls [camlc_seg_fn] with arguments CALLEE found in
               [camlc_seg] (see [translate_tail_call]) *)
            let c_type c =
              match c with
              | 'v' -> "value"
              | 'd' -> "double"
              | 'l' -> "int64_t"
              | 'i' -> "int32_t"
              | _ -> "intnat"
            in
            let ret = name.[8] in
            let caller, callee =
              match
                String.split_on_char
                  ~sep:'_'
                  (String.sub name ~pos:10 ~len:(String.length name - 10))
              with
              | [ a; b ] -> a, b
              | _ -> assert false
            in
            let chars s = List.init ~len:(String.length s) ~f:(fun i -> s.[i]) in
            Printf.bprintf
              b
              "static __attribute__((noinline)) %s %s(%s) { return ((%s \
               (*)(%s))camlc_seg_fn)(%s); }\n"
              (c_type ret)
              name
              (String.concat
                 ~sep:", "
                 (List.mapi (chars caller) ~f:(fun i c ->
                      Printf.sprintf "%s a%d" (c_type c) i)))
              (c_type ret)
              (String.concat ~sep:", " (List.map (chars callee) ~f:c_type))
              (String.concat
                 ~sep:", "
                 (List.mapi (chars callee) ~f:(fun i c ->
                      Printf.sprintf "camlc_seg[%d].%c" i c)))
        | _ -> Printf.bprintf b "extern value %s;\n" name);
    Buffer.add_char b '\n'
  in
  let preamble b =
    Buffer.add_string b "/* Generated by c_of_ocaml */\n#include \"camlc.h\"\n\n";
    Printf.bprintf
      b
      "_Static_assert(sizeof(value) == %d, \"c_of_ocaml: wrong word size\");\n\n"
      (word_size ());
    String.Hashtbl.iter
      (fun name n ->
        if n <= 5
        then
          Printf.bprintf
            b
            "extern value %s(%s);\n"
            name
            (if n = 0
             then "void"
             else String.concat ~sep:", " (List.init ~len:n ~f:(fun _ -> "value")))
        else Printf.bprintf b "extern value %s(value *, int);\n" name)
      out.primitives;
    Buffer.add_char b '\n';
    match mode with
    | `Plugin ->
        (* The symbols of a plugin are local to it, except its entry point:
           they cannot conflict with those of the program or of other
           plugins *)
        Buffer.add_string b "#pragma GCC visibility push(hidden)\n\n"
    | `Program | `Dynlink -> ()
  in
  let prototypes b l =
    List.iter l ~f:(fun f ->
        Printf.bprintf
          b
          "%s%s %s(%s);\n"
          (storage f)
          (c_type (Var.Hashtbl.find out.fun_returns f))
          (fun_name out f)
          (String.concat
             ~sep:", "
             (List.map (Var.Hashtbl.find out.fun_reprs f @ [ Value ]) ~f:c_type)));
    Buffer.add_char b '\n'
  in
  let tables b =
    C_tables.tables
      ~out
      ~mode
      ~dt
      ~globals
      ~global_list
      ~definitions
      ~interfaces
      ~units
      ~p
      ~functions_max_arity
      b
  in
  let helpers b = C_tables.helpers ~out ~mode ~dt b in
  let add_definitions b l =
    List.iter l ~f:(fun d ->
        (match d.def_fn with
        | Some f -> Buffer.add_string b (storage f)
        | None -> ());
        Buffer.add_string b d.def_text)
  in
  let files =
    if not split
    then (
      let b =
        Buffer.create
          (Buffer.length out.data
          + 65536
          + List.fold_left definitions ~init:0 ~f:(fun n d -> n + d.def_size))
      in
      preamble b;
      prototypes b (List.map functions ~f:(fun (f, _, _) -> f));
      tables b;
      String.Hashtbl.fold (fun name _ acc -> name :: acc) out.locations []
      |> String.Hashtbl.fold (fun name () acc -> name :: acc) out.segment_thunks
      |> List.sort ~cmp:String.compare
      |> declare_symbols b;
      helpers b;
      add_definitions b definitions;
      [ Buffer.contents b ])
    else
      let main =
        let b = Buffer.create (Buffer.length out.data + 65536) in
        preamble b;
        prototypes b (Var.Set.elements data_refs);
        tables b;
        Buffer.contents b
      in
      main
      :: List.map parts ~f:(fun l ->
          let b =
            Buffer.create (List.fold_left l ~init:65536 ~f:(fun n d -> n + d.def_size))
          in
          preamble b;
          declare_symbols b (symbols (List.map l ~f:(fun d -> d.def_text)));
          let used =
            List.fold_left l ~init:Var.Set.empty ~f:(fun s d ->
                let s = Var.Set.union s d.def_refs in
                match d.def_fn with
                | Some f -> Var.Set.add f s
                | None -> s)
          in
          prototypes b (Var.Set.elements used);
          helpers b;
          add_definitions b l;
          Buffer.contents b)
  in
  let files =
    match mode, files with
    | (`Program | `Dynlink), main :: parts ->
        (* The digest of the program, for the code pointers of marshaled
           closures (see camlc_runtime.c) *)
        let digest =
          Digest.string (String.concat ~sep:"" (List.map files ~f:Digest.string))
        in
        Printf.sprintf
          "%s\nconst unsigned char camlc_program_digest[16] = { %s };\n"
          main
          (String.concat
             ~sep:", "
             (List.init ~len:16 ~f:(fun i -> string_of_int (Char.code digest.[i]))))
        :: parts
    | _ -> files
  in
  { files
  ; primitives =
      List.sort
        ~cmp:(fun (a, _) (b, _) -> String.compare a b)
        (String.Hashtbl.fold
           (fun name n l -> (name, if n <= 5 then n else -1) :: l)
           out.primitives
           [])
  }
