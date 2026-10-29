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

let usage = "Usage: c_of_ocaml [options] program.bc -o program.c"

(* The JavaScript runtime tells which primitives are pure *)
let load_primitive_kinds () =
  List.iter Js_of_ocaml_compiler_runtime_files.runtime ~f:(fun t ->
      let filename = Builtins.File.name t in
      let fragments = Linker.Fragment.parse_builtin t in
      Linker.load_fragments ~target_env:Target_env.Isomorphic ~filename fragments);
  Linker.check_deps ();
  List.iter
    ~f:(fun name -> Primitive.register name `Pure None None)
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
    ]

let compile ~profile ~input_file ~output_file =
  let ic = open_in_bin input_file in
  (match Parse_bytecode.from_channel ic with
  | `Exe -> ()
  | `Cmo _ | `Cma _ ->
      failwith "c_of_ocaml: only bytecode executables are supported");
  let t = Timer.make () in
  let one =
    Parse_bytecode.from_exe
      ~includes:[]
      ~include_cmis:false
      ~link_info:false
      ~linkall:false
      ~debug:false
      ic
  in
  close_in ic;
  if times () then Format.eprintf "  parsing: %a@." Timer.print t;
  let optimized, _ = Driver.optimize_for_wasm ~profile ~shapes:false one.code in
  let t = Timer.make () in
  let p, closures = Closure_conversion.f optimized.program in
  let c = Generate.f p closures in
  if times () then Format.eprintf "  code generation: %a@." Timer.print t;
  let oc = open_out_bin output_file in
  output_string oc c;
  close_out oc

let () =
  let input_file = ref None in
  let output_file = ref None in
  let profile = ref Profile.O1 in
  let spec =
    [ "-o", Arg.String (fun s -> output_file := Some s), "FILE Write the C code to FILE"
    ; ( "--opt"
      , Arg.Int
          (fun n ->
            match Profile.of_int n with
            | Some p -> profile := p
            | None -> raise (Arg.Bad "--opt: expected 1, 2 or 3"))
      , "N Optimization profile (1, 2 or 3)" )
    ; "--debug", Arg.String Debug.enable, "NAME Enable the debug output NAME"
    ; "--enable", Arg.String Config.Flag.enable, "OPT Enable the option OPT"
    ; "--disable", Arg.String Config.Flag.disable, "OPT Disable the option OPT"
    ]
  in
  Arg.parse spec (fun s -> input_file := Some s) usage;
  match !input_file, !output_file with
  | Some input_file, Some output_file -> (
      Config.set_target `C;
      Config.set_effects_backend `Disabled;
      load_primitive_kinds ();
      try compile ~profile:!profile ~input_file ~output_file
      with Failure msg ->
        prerr_endline msg;
        exit 1)
  | _ ->
      Arg.usage spec usage;
      exit 2
