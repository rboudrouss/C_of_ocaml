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

(* No release: ocaml-emcc runs --version only to check that the options before it are
   known *)
let version = "dev"

let usage =
  "Usage: c_of_ocaml [options] program.bc -o program.c\n\
  \       c_of_ocaml [options] program.bc --split DIR\n\
  \       c_of_ocaml [options] plugin.cmo|plugin.cma (-o plugin.c | --split DIR)\n\
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

(* The manifest of the C code: what its host needs to know to compile and
   link it, without reading it. Lines "KEY VALUE":
   - [format 1]: the version of this format
   - [word_size 32|64]
   - [mode program|dynlink|plugin]
   - [effects disabled|double-translation]: the effect handlers of the
     program (with [dynlink], its plugins must have the same)
   - [debug_info true|false]: backtraces are recorded
   - [primitive NAME ARITY], for each C primitive the code calls, sorted:
     the number of [value] arguments of its calls, or -1 for the calls
     with an array of arguments and its length ([value *, int]) *)
let manifest ~word_size ~mode ~debug_info primitives =
  let b = Buffer.create 4096 in
  Printf.bprintf b "format 1\n";
  Printf.bprintf b "word_size %d\n" word_size;
  Printf.bprintf
    b
    "mode %s\n"
    (match mode with
    | `Program -> "program"
    | `Dynlink -> "dynlink"
    | `Plugin -> "plugin");
  Printf.bprintf
    b
    "effects %s\n"
    (match Config.effects () with
    | `Double_translation -> "double-translation"
    | `Disabled -> "disabled"
    | `Cps | `Jspi | `Native -> assert false);
  Printf.bprintf b "debug_info %b\n" debug_info;
  List.iter primitives ~f:(fun (name, arity) ->
      Printf.bprintf b "primitive %s %d\n" name arity);
  Buffer.contents b

(* Writes [contents] to the file [name], unless it has them already *)
let write_if_changed name contents =
  if not (Sys.file_exists name && String.equal (read_file name) contents)
  then (
    let oc = open_out_bin name in
    output_string oc contents;
    close_out oc)

(* DIR/part0.c, DIR/part1.c, ...: the files left unchanged are not
   rewritten, and those of a previous, larger output are removed *)
let write_parts dir parts =
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  let n = List.length parts in
  Array.iter (Sys.readdir dir) ~f:(fun name ->
      match part_index name with
      | Some i when i >= n -> Sys.remove (Filename.concat dir name)
      | Some _ | None -> ());
  List.iteri parts ~f:(fun i contents -> write_if_changed (part_file dir i) contents)

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

(* The code without the registrations of the compilation units by name
   (caml_register_global), which keep them all alive *)
let without_registrations (p : Code.program) =
  { p with
    blocks =
      Code.Addr.Map.map
        (fun (block : Code.block) ->
          { block with
            body =
              List.filter block.body ~f:(fun i ->
                  match i with
                  | Code.Let (_, Prim (Extern ("caml_register_global", _), _)) -> false
                  | _ -> true)
          })
        p.blocks
  }

let compile
    ~profile
    ~effects
    ~dynlink
    ~debug_info
    ~word_size
    ~input_file
    ~output
    ~manifest_file
    ~toplevel_chunk_size =
  let ic = open_in_bin input_file in
  let input = Parse_bytecode.from_channel ic in
  let mode =
    match input with
    | `Exe -> if dynlink then `Dynlink else `Program
    | `Cmo _ | `Cma _ -> `Plugin
  in
  (* Backtraces with locations by default when the bytecode has debug
     events (ocamlc -g) *)
  let debug_info =
    match debug_info with
    | Some d -> d
    | None -> (
        match input with
        | `Exe -> Parse_bytecode.has_debug_section ic
        | `Cmo unit -> unit.Cmo_format.cu_debug <> 0
        | `Cma lib ->
            List.exists lib.Cmo_format.lib_units ~f:(fun u -> u.Cmo_format.cu_debug <> 0))
  in
  if debug_info then Config.Flag.enable "debuginfo";
  let t = Timer.make () in
  let one =
    match input with
    | `Exe ->
        (* With --dynlink, all the compilation units are registered by name *)
        let parse ~link_info =
          seek_in ic 0;
          Parse_bytecode.from_exe
            ~includes:[]
            ~include_cmis:false
            ~link_info
            ~linkall:dynlink
            ~debug:debug_info
            ic
        in
        let one = parse ~link_info:false in
        (* The sections of the bytecode executable read by the toplevel
           (Symtable.init_toplevel) are linked in as data, if the program
           reads them *)
        if uses_primitive one.code "caml_dynlink_get_bytecode_sections"
        then parse ~link_info:true
        else one
    | `Cmo unit -> Parse_bytecode.from_cmo ~debug:debug_info unit ic
    | `Cma lib -> Parse_bytecode.from_cma ~debug:debug_info lib ic
  in
  (* The interfaces the program or the plugin is compiled against, checked
     when a plugin is loaded *)
  let interfaces =
    let imports =
      match input, mode with
      | `Exe, `Dynlink -> Parse_bytecode.read_crcs ic
      | `Exe, (`Program | `Plugin) -> []
      | `Cmo unit, _ -> Ocaml_compiler.Cmo_format.imports unit
      | `Cma lib, _ ->
          List.concat_map lib.Cmo_format.lib_units ~f:Ocaml_compiler.Cmo_format.imports
    in
    let t = String.Hashtbl.create 64 in
    List.iter imports ~f:(fun i ->
        match Ocaml_compiler.Import_info.crc i with
        | None -> ()
        | Some crc -> (
            let name = Ocaml_compiler.Import_info.name i in
            let crc = Digest.to_hex crc in
            match String.Hashtbl.find_opt t name with
            | Some crc' when not (String.equal crc crc') ->
                failwith
                  (Printf.sprintf
                     "c_of_ocaml: inconsistent assumptions over interface %s"
                     name)
            | _ -> String.Hashtbl.replace t name crc));
    List.sort
      ~cmp:(fun (a, _) (b, _) -> String.compare a b)
      (String.Hashtbl.fold (fun n crc acc -> (n, crc) :: acc) t [])
  in
  close_in ic;
  if times () then Format.eprintf "  parsing: %a@." Timer.print t;
  (* Little of what each phase allocates is still live at its end: without a
     compaction, the next phase would grow the heap further *)
  Gc.compact ();
  memory ();
  (* Whether the code performs effects out of its dead code, Stdlib.Effect
     linked with -linkall for instance. The compilation units of a program
     compiled with --dynlink are all registered for its plugins: not roots
     here, or Stdlib.Effect would always count. Those of a plugin are, as the
     plugins loaded after it may use them *)
  let performs_effects =
    let needed =
      match effects, mode with
      | None, (`Program | `Dynlink) -> not (Config.Flag.cps_recursions ())
      | Some _, _ | None, `Plugin -> false
    in
    if
      (not needed)
      || Ocaml_version.compare Ocaml_version.current [ 5; 0 ] < 0
      || not (uses_effects one.code)
    then false
    else
      let p =
        match mode with
        | `Dynlink -> without_registrations one.code
        | `Program | `Plugin -> one.code
      in
      uses_effects (fst (Deadcode.f (Pure_fun.f p) p))
  in
  (if Config.Flag.cps_recursions ()
   then
     match effects with
     | None | Some `Double_translation -> ()
     | Some _ -> failwith "c_of_ocaml: --cps-stack needs --effects=double-translation");
  Config.set_effects_backend
    (match effects, mode with
    | Some e, _ -> e
    | None, (`Program | `Dynlink | `Plugin) when Config.Flag.cps_recursions () ->
        `Double_translation
    | None, `Plugin ->
        (* That of the program, which its host gives *)
        `Disabled
    | None, (`Program | `Dynlink) ->
        (* Effect handlers in dead code only do not need the double
           translation, which doubles the size of the code. A program which
           performs no effect but whose plugins perform them through its
           functions needs --effects double-translation *)
        if performs_effects then `Double_translation else `Disabled);
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
      ~mode
      ~interfaces
      ~global_flow_data
      ~deadcode_sentinel:optimized.deadcode_sentinel
      ~debug_info
      ~in_cps:optimized.in_cps
      ~trampolined_calls:optimized.trampolined_calls
      ~toplevel_chunk_size
      ?part_size:
        (match output with
        | `File _ -> None
        | `Split (_, size) -> Some size)
      closures
  in
  if times () then Format.eprintf "  code generation: %a@." Timer.print t;
  memory ();
  (match output, c.files with
  | `File name, [ c ] ->
      let oc = open_out_bin name in
      output_string oc c;
      close_out oc
  | `Split (dir, _), parts -> write_parts dir parts
  | `File _, _ -> assert false);
  let manifest_file =
    match manifest_file, output with
    | Some f, _ -> Some f
    | None, `Split (dir, _) -> Some (Filename.concat dir "manifest")
    | None, `File _ -> None
  in
  Option.iter manifest_file ~f:(fun f ->
      write_if_changed f (manifest ~word_size ~mode ~debug_info c.primitives))

let () =
  let input_file = ref None in
  let runtime_dir = ref None in
  let output_file = ref None in
  let split_dir = ref None in
  let manifest_file = ref None in
  let part_size = ref 2048 in
  let toplevel_chunk_size = ref 2000 in
  let profile = ref Profile.O1 in
  let debug_info = ref None in
  let effects = ref None in
  let dynlink = ref false in
  let word_size = ref 32 in
  let spec =
    [ "-o", Arg.String (fun s -> output_file := Some s), "FILE Write the C code to FILE"
    ; ( "--split"
      , Arg.String (fun s -> split_dir := Some s)
      , "DIR Write the C code to several files, which can be compiled separately: \
         DIR/part0.c (the static data and the tables), DIR/part1.c, ... (the functions). \
         The files left unchanged are not rewritten" )
    ; ( "--manifest"
      , Arg.String (fun s -> manifest_file := Some s)
      , "FILE Write the manifest of the C code to FILE: what its host needs to know to \
         compile and link it, as the primitives it calls (default with --split: \
         DIR/manifest)" )
    ; ( "--part-size"
      , Arg.Int
          (fun n ->
            if n <= 0 then raise (Arg.Bad "--part-size: expected a positive size");
            part_size := n)
      , "KB With --split, the size of the C code of the functions of each file, in \
         kilobytes (default 2048)" )
    ; ( "--toplevel-chunk-size"
      , Arg.Int
          (fun n ->
            if n < 0
            then raise (Arg.Bad "--toplevel-chunk-size: expected a non-negative size");
            toplevel_chunk_size := n)
      , "N Split the toplevel code into functions of about N instructions of the \
         intermediate code each, to bound the time the C compiler takes to optimize it \
         (0: not split; default 2000)" )
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
      , Arg.Unit (fun () -> debug_info := Some true)
      , " Record backtraces with the locations of the debug events (ocamlc -g): the \
         default when the bytecode has some" )
    ; ( "--no-debug-info"
      , Arg.Unit (fun () -> debug_info := Some false)
      , " No backtraces, even when the bytecode has debug events" )
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
         performs effects out of its dead code, also with --dynlink, where the effects \
         of its plugins do not count; disabled for plugins, which need that of the \
         program)" )
    ; ( "--cps-stack"
      , Arg.Unit (fun () -> Config.Flag.enable "cps-recursions")
      , " Deep recursions go on in CPS, on the heap, past a depth: the functions which \
         may recurse get a CPS version (double translation), as with effect handlers. \
         Larger code, but no limit of the stack of the wasm engine nor JSPI" )
    ; ( "--dynlink"
      , Arg.Set dynlink
      , " Compile a program which can load plugins: its compilation units are registered \
         by name, and kept whole. A .cmo or .cma file is compiled to a plugin" )
    ; ( "--version"
      , Arg.Unit
          (fun () ->
            print_endline version;
            exit 0)
      , " Print the version and exit" )
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
      try
        compile
          ~profile:!profile
          ~effects:!effects
          ~dynlink:!dynlink
          ~debug_info:!debug_info
          ~word_size:!word_size
          ~manifest_file:!manifest_file
          ~toplevel_chunk_size:!toplevel_chunk_size
          ~input_file
          ~output
      with Failure msg ->
        prerr_endline msg;
        exit 1)
  | _ ->
      Arg.usage spec usage;
      exit 2
