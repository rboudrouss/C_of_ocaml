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

(* A plugin using the program and the standard library, with globals and
   static data used after GCs *)

let add a b c = a + b + c

let () =
  Printf.printf
    "plugin: %d %s\n%!"
    (List.length [ 1; 2; 3 ])
    (String.uppercase_ascii "abc");
  Shared.counter := !Shared.counter + 10;
  Shared.register (add 1 2);
  (try raise Not_found with Not_found -> print_endline "plugin: Not_found caught");
  let h = Hashtbl.create 3 in
  Hashtbl.add h "x" 1.5;
  Printf.printf "plugin: %g\n" (Hashtbl.find h "x" *. 2.)

let table = Array.init 1000 (fun i -> string_of_int i)

let lookup i = table.(i)

let consts = [ "static"; "data" ]

let () =
  Shared.register (fun i ->
      for _ = 1 to 3 do
        Gc.compact ();
        ignore (Sys.opaque_identity (Array.make 10000 0))
      done;
      assert (compare (Some consts) (Some [ String.make 1 's' ^ "tatic"; "data" ]) = 0);
      assert (Hashtbl.hash consts = Hashtbl.hash [ "static"; String.sub "xdata" 1 4 ]);
      int_of_string (lookup (i * 100)))
