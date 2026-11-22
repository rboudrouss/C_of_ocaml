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

(* Prints the places where a C file reads the sentinel of the dead code
   elimination as a boxed number (Double_val(dummy), ...) *)

let () =
  let ic = open_in_bin Sys.argv.(1) in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let re = Str.regexp "[A-Za-z0-9]+_val(camlc_g_[0-9a-f]+_dummy)" in
  let rec scan pos n =
    match Str.search_forward re s pos with
    | _ ->
        print_endline (Str.matched_string s);
        scan (Str.match_end ()) (n + 1)
    | exception Not_found -> n
  in
  Printf.printf "%d sentinel(s) read as boxed numbers\n" (scan 0 0)
