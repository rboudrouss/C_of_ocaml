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

(* Prints the primitives on JavaScript values (and caml_assume_no_perform)
   called by a C file, with their number of arguments, and the primitives
   whose name starts with '%' left in it *)

let () =
  let ic = open_in_bin Sys.argv.(1) in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let calls =
    Str.regexp "camlc_ccall\\([0-9]+\\)(\\(caml_[a-z_]*\\(js\\|perform\\)[a-z_]*\\)"
  in
  let percent = Str.regexp "%[a-z]+_[a-z_]+" in
  let found = Hashtbl.create 16 in
  let rec scan re f pos =
    match Str.search_forward re s pos with
    | pos ->
        Hashtbl.replace found (f ()) ();
        scan re f (pos + 1)
    | exception Not_found -> ()
  in
  scan calls (fun () -> Str.matched_group 2 s ^ "/" ^ Str.matched_group 1 s) 0;
  scan percent (fun () -> "unexpected primitive " ^ Str.matched_string s) 0;
  Hashtbl.fold (fun k () l -> k :: l) found []
  |> List.sort compare
  |> List.iter print_endline
