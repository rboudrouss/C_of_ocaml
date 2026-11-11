(* Js_of_ocaml compiler
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

(* The jumps of the CPS code to a block without parameter (the loop below,
   left by an exception), calls of a closure of arity 0: without the tail
   calls of WebAssembly, the backward ones bounce on the trampoline, the
   others may be left pending (see [CAMLC_PENDING]), and both are then made
   with no argument. With --dynlink (TAIL_CALLS=no COOC_FLAGS='--effects=
   double-translation --cps-stack --dynlink' tools/c_of_ocaml_tests.sh),
   where the unknown calls may perform effects, the deep recursion runs in
   CPS. *)

let rec deep d f =
  if d = 0
  then 0
  else
    let x =
      let s = Sys.opaque_identity (ref 0) and i = Sys.opaque_identity (ref 300) in
      (try
         while true do
           if !i = 0 then raise Exit;
           s := !s + f !i;
           decr i
         done
       with Exit -> ());
      !s
    in
    x + deep (d - 1) f

let () = Printf.printf "%d\n" (deep 100_000 (Sys.opaque_identity (fun x -> x land 1)))
