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

let usage =
  "Usage: c_of_ocaml [options] program.bc -o program.c\n\
  \       c_of_ocaml --runtime-files DIR"

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

let compile ~profile ~effects ~debug_info ~input_file ~output_file =
  let ic = open_in_bin input_file in
  (match Parse_bytecode.from_channel ic with
  | `Exe -> ()
  | `Cmo _ | `Cma _ -> failwith "c_of_ocaml: only bytecode executables are supported");
  let t = Timer.make () in
  let one =
    Parse_bytecode.from_exe
      ~includes:[]
      ~include_cmis:false
      ~link_info:false
      ~linkall:false
      ~debug:debug_info
      ic
  in
  close_in ic;
  if times () then Format.eprintf "  parsing: %a@." Timer.print t;
  Config.set_effects_backend
    (match effects with
    | Some e -> e
    | None ->
        if
          Ocaml_version.compare Ocaml_version.current [ 5; 0 ] >= 0
          && uses_effects one.code
        then `Double_translation
        else `Disabled);
  let optimized, global_flow_data =
    Driver.optimize_for_wasm ~profile ~shapes:false one.code
  in
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
      closures
  in
  if times () then Format.eprintf "  code generation: %a@." Timer.print t;
  let oc = open_out_bin output_file in
  output_string oc c;
  close_out oc

let () =
  let input_file = ref None in
  let runtime_dir = ref None in
  let output_file = ref None in
  let profile = ref Profile.O1 in
  let debug_info = ref false in
  let effects = ref None in
  let spec =
    [ "-o", Arg.String (fun s -> output_file := Some s), "FILE Write the C code to FILE"
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
  match !input_file, !output_file, !runtime_dir with
  | None, None, Some dir -> write_runtime_files dir
  | Some input_file, Some output_file, None -> (
      Config.set_target `C;
      register_primitives ();
      if !debug_info then Config.Flag.enable "debuginfo";
      try
        compile
          ~profile:!profile
          ~effects:!effects
          ~debug_info:!debug_info
          ~input_file
          ~output_file
      with Failure msg ->
        prerr_endline msg;
        exit 1)
  | _ ->
      Arg.usage spec usage;
      exit 2
