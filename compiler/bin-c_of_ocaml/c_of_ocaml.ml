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

open! Stdlib
open C_of_ocaml_compiler

let times = Debug.find "times"

(* With --debug times, the size of the heap after each phase *)
let memory () =
  if times ()
  then
    let st = Gc.quick_stat () in
    let mb w = w * (Sys.word_size / 8) / 1048576 in
    Format.eprintf
      "    heap: %d MB (peak %d MB)@."
      (mb st.heap_words)
      (mb st.top_heap_words)

let usage =
  "Usage: c_of_ocaml [options] program.bc -o program.c\n\
  \       c_of_ocaml [options] program.bc --split DIR\n\
  \       c_of_ocaml --runtime-files DIR"

(* The C files of a split program, see [write_parts] *)
let part_file dir i = Filename.concat dir (Printf.sprintf "part%d.c" i)

let part_index name =
  let n = String.length name in
  if
    n > 6
    && String.equal (String.sub name ~pos:0 ~len:4) "part"
    && Filename.check_suffix name ".c"
  then int_of_string_opt (String.sub name ~pos:4 ~len:(n - 6))
  else None

let read_file name =
  let ic = open_in_bin name in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

(* DIR/part0.c, DIR/part1.c, ...: the files left unchanged are not
   rewritten, and those of a previous, larger output are removed *)
let write_parts dir parts =
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  let n = List.length parts in
  Array.iter (Sys.readdir dir) ~f:(fun name ->
      match part_index name with
      | Some i when i >= n -> Sys.remove (Filename.concat dir name)
      | Some _ | None -> ());
  List.iteri parts ~f:(fun i contents ->
      let name = part_file dir i in
      if not (Sys.file_exists name && String.equal (read_file name) contents)
      then (
        let oc = open_out_bin name in
        output_string oc contents;
        close_out oc))

(* Write the runtime support (camlc.h, camlc_runtime.c) to [dir] *)
let write_runtime_files dir =
  List.iter Runtime_files.files ~f:(fun (name, contents) ->
      let oc = open_out_bin (Filename.concat dir name) in
      output_string oc contents;
      close_out oc)

(* The primitives without side effects which cannot raise. We do not use
   the purity declared by the JavaScript runtime: some of its primitives
   cannot raise, while their C version can. The other primitives are
   assumed to have side effects and to raise. *)
let pure_primitives =
  [ "%int_add"
  ; "%int_sub"
  ; "%int_mul"
  ; "%int_neg"
  ; "%int_and"
  ; "%int_or"
  ; "%int_xor"
  ; "%int_lsl"
  ; "%int_lsr"
  ; "%int_asr"
  ; "%direct_obj_tag"
  ; "caml_int_compare"
  ; "caml_bswap16"
  ; "caml_string_compare"
  ; "caml_string_equal"
  ; "caml_string_notequal"
  ; "caml_string_lessthan"
  ; "caml_string_lessequal"
  ; "caml_string_greaterthan"
  ; "caml_string_greaterequal"
  ; "caml_ml_string_length"
  ; "caml_ml_bytes_length"
  ; "caml_string_unsafe_get"
  ; "caml_string_concat"
  ; "caml_bytes_of_string"
  ; "caml_string_of_bytes"
  ; "caml_obj_tag"
  ; "caml_hash"
  ; "caml_lazy_make_forward"
  ; "caml_sys_const_backend_type"
  ; "caml_sys_const_big_endian"
  ; "caml_sys_const_int_size"
  ; "caml_sys_const_max_wosize"
  ; "caml_sys_const_ostype_cygwin"
  ; "caml_sys_const_ostype_unix"
  ; "caml_sys_const_ostype_win32"
  ; "caml_sys_const_word_size"
  ; "caml_sys_const_naked_pointers_checked"
  ; "caml_sys_const_runtime5"
  ]
  @ List.map
      ~f:(fun f -> "caml_" ^ f ^ "_float")
      [ "add"
      ; "sub"
      ; "mul"
      ; "div"
      ; "neg"
      ; "abs"
      ; "sqrt"
      ; "cbrt"
      ; "exp"
      ; "exp2"
      ; "expm1"
      ; "log"
      ; "log10"
      ; "log1p"
      ; "log2"
      ; "sin"
      ; "cos"
      ; "tan"
      ; "asin"
      ; "acos"
      ; "atan"
      ; "atan2"
      ; "sinh"
      ; "cosh"
      ; "tanh"
      ; "asinh"
      ; "acosh"
      ; "atanh"
      ; "erf"
      ; "erfc"
      ; "floor"
      ; "ceil"
      ; "trunc"
      ; "round"
      ; "fmod"
      ; "power"
      ; "hypot"
      ; "copysign"
      ; "ldexp"
      ; "frexp"
      ; "modf"
      ; "nextafter"
      ; "fma"
      ; "signbit"
      ; "classify"
      ; "eq"
      ; "neq"
      ; "lt"
      ; "le"
      ; "gt"
      ; "ge"
      ]
  @ [ "caml_float_of_int"; "caml_int_of_float"; "caml_float_compare" ]
  @ List.concat_map
      ~f:(fun t ->
        List.map
          ~f:(fun f -> Printf.sprintf "caml_%s_%s" t f)
          [ "add"
          ; "sub"
          ; "mul"
          ; "neg"
          ; "and"
          ; "or"
          ; "xor"
          ; "shift_left"
          ; "shift_right"
          ; "shift_right_unsigned"
          ; "of_int"
          ; "to_int"
          ; "of_float"
          ; "to_float"
          ; "of_int32"
          ; "to_int32"
          ; "of_nativeint"
          ; "to_nativeint"
          ; "bits_of_float"
          ; "float_of_bits"
          ; "compare"
          ; "bswap"
          ])
      [ "int64"; "int32"; "nativeint" ]

(* Primitives without side effects reading mutable data *)
let mutable_primitives =
  [ "caml_bytes_unsafe_get"; "caml_array_unsafe_get"; "caml_floatarray_unsafe_get" ]

let register_primitives () =
  List.iter ~f:(fun name -> Primitive.register name `Pure None None) pure_primitives;
  List.iter ~f:(fun name -> Primitive.register name `Mutable None None) mutable_primitives

let uses_primitive (p : Code.program) prim =
  Code.Addr.Map.fold
    (fun _ (block : Code.block) acc ->
      acc
      || List.exists block.body ~f:(fun i ->
          match i with
          | Code.Let (_, Prim (Extern (name, _), _)) -> String.equal name prim
          | _ -> false))
    p.blocks
    false

(* Whether the program uses effect handlers *)
let uses_effects (p : Code.program) =
  Code.Addr.Map.fold
    (fun _ (block : Code.block) acc ->
      acc
      || List.exists block.body ~f:(fun i ->
          match i with
          | Code.Let (_, Prim (Extern (name, _), _)) -> (
              match name with
              | "%perform" | "%reperform" | "%resume" | "%with_stack" | "%with_stack_bind"
                -> true
              | _ -> false)
          | _ -> false))
    p.blocks
    false

let compile ~profile ~effects ~debug_info ~input_file ~output =
  let ic = open_in_bin input_file in
  (match Parse_bytecode.from_channel ic with
  | `Exe -> ()
  | `Cmo _ | `Cma _ -> failwith "c_of_ocaml: only bytecode executables are supported");
  let t = Timer.make () in
  let parse ~link_info =
    seek_in ic 0;
    Parse_bytecode.from_exe
      ~includes:[]
      ~include_cmis:false
      ~link_info
      ~linkall:false
      ~debug:debug_info
      ic
  in
  let one = parse ~link_info:false in
  (* The sections of the bytecode executable read by the toplevel
     (Symtable.init_toplevel) are linked in as data, if the program reads
     them *)
  let one =
    if uses_primitive one.code "caml_dynlink_get_bytecode_sections"
    then parse ~link_info:true
    else one
  in
  close_in ic;
  if times () then Format.eprintf "  parsing: %a@." Timer.print t;
  (* Little of what each phase allocates is still live at its end: without a
     compaction, the next phase would grow the heap further *)
  Gc.compact ();
  memory ();
  Config.set_effects_backend
    (match effects with
    | Some e -> e
    | None ->
        (* Effect handlers in dead code only, the module Effect linked with
           -linkall for instance, do not need the double translation, which
           doubles the size of the code *)
        if
          Ocaml_version.compare Ocaml_version.current [ 5; 0 ] >= 0
          && uses_effects one.code
          && uses_effects (fst (Deadcode.f (Pure_fun.f one.code) one.code))
        then `Double_translation
        else `Disabled);
  let optimized, global_flow_data =
    Driver.optimize_for_wasm ~profile ~shapes:false one.code
  in
  Gc.compact ();
  memory ();
  let t = Timer.make () in
  let p, closures = Closure_conversion.f optimized.program in
  let c =
    Generate.f
      p
      ~global_flow_data
      ~deadcode_sentinel:optimized.deadcode_sentinel
      ~debug_info
      ~in_cps:optimized.in_cps
      ~trampolined_calls:optimized.trampolined_calls
      ?part_size:
        (match output with
        | `File _ -> None
        | `Split (_, size) -> Some size)
      closures
  in
  if times () then Format.eprintf "  code generation: %a@." Timer.print t;
  memory ();
  match output, c with
  | `File name, [ c ] ->
      let oc = open_out_bin name in
      output_string oc c;
      close_out oc
  | `Split (dir, _), parts -> write_parts dir parts
  | `File _, _ -> assert false

let () =
  let input_file = ref None in
  let runtime_dir = ref None in
  let output_file = ref None in
  let split_dir = ref None in
  let part_size = ref 2048 in
  let profile = ref Profile.O1 in
  let debug_info = ref false in
  let effects = ref None in
  let word_size = ref 32 in
  let spec =
    [ "-o", Arg.String (fun s -> output_file := Some s), "FILE Write the C code to FILE"
    ; ( "--split"
      , Arg.String (fun s -> split_dir := Some s)
      , "DIR Write the C code to several files, which can be compiled separately: \
         DIR/part0.c (the static data and the tables), DIR/part1.c, ... (the functions). \
         The files left unchanged are not rewritten" )
    ; ( "--part-size"
      , Arg.Int
          (fun n ->
            if n <= 0 then raise (Arg.Bad "--part-size: expected a positive size");
            part_size := n)
      , "KB With --split, the size of the C code of the functions of each file, in \
         kilobytes (default 2048)" )
    ; ( "--opt"
      , Arg.Int
          (fun n ->
            match Profile.of_int n with
            | Some p -> profile := p
            | None -> raise (Arg.Bad "--opt: expected 1, 2 or 3"))
      , "N Optimization profile (1, 2 or 3)" )
    ; ( "--runtime-files"
      , Arg.String (fun s -> runtime_dir := Some s)
      , "DIR Write the C runtime support of the generated code to DIR" )
    ; ( "--debug-info"
      , Arg.Set debug_info
      , " Record backtraces with the locations of the debug events (ocamlc -g)" )
    ; ( "--word-size"
      , Arg.Symbol ([ "32"; "64" ], fun s -> word_size := int_of_string s)
      , " Word size of the target, in bits: 32 for wasm32 (31-bit integers, the \
         default), 64 for wasm64 (63-bit integers)" )
    ; ( "--effects"
      , Arg.Symbol
          ( [ "disabled"; "double-translation" ]
          , fun s ->
              effects :=
                Some
                  (match s with
                  | "disabled" -> `Disabled
                  | _ -> `Double_translation) )
      , " Support of effect handlers (default: double-translation if the program \
         performs effects)" )
    ; "--debug", Arg.String Debug.enable, "NAME Enable the debug output NAME"
    ; "--enable", Arg.String Config.Flag.enable, "OPT Enable the option OPT"
    ; "--disable", Arg.String Config.Flag.disable, "OPT Disable the option OPT"
    ]
  in
  Arg.parse spec (fun s -> input_file := Some s) usage;
  let output =
    match !output_file, !split_dir with
    | Some f, None -> Some (`File f)
    | None, Some d -> Some (`Split (d, !part_size * 1024))
    | _ -> None
  in
  match !input_file, output, !runtime_dir with
  | None, None, Some dir when Option.is_none !output_file && Option.is_none !split_dir ->
      write_runtime_files dir
  | Some input_file, Some output, None -> (
      Config.set_target `C;
      if !word_size = 64 then Targetint.set_num_bits 63;
      register_primitives ();
      if !debug_info then Config.Flag.enable "debuginfo";
      try
        compile
          ~profile:!profile
          ~effects:!effects
          ~debug_info:!debug_info
          ~input_file
          ~output
      with Failure msg ->
        prerr_endline msg;
        exit 1)
  | _ ->
      Arg.usage spec usage;
      exit 2
