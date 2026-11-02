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

(* The program (compiled with --dynlink), which loads the plugins given on
   its command line *)

external loadfile : string -> unit = "test_loadfile"

let load f =
  match loadfile f with
  | () -> Printf.printf "loaded %s\n%!" f
  | exception Failure msg -> Printf.printf "%s: Failure %S\n%!" f msg
  | exception Shared.Plugin_error msg -> Printf.printf "%s: Plugin_error %S\n%!" f msg

let () =
  print_endline "main";
  List.iter load (List.tl (Array.to_list Sys.argv));
  Printf.printf
    "counter = %d, hooks: %s\n"
    !Shared.counter
    (String.concat " " (List.map string_of_int (Shared.run 5)))
