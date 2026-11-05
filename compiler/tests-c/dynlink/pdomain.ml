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

(* A plugin starting a domain in a program which starts none: its
   collections stop the world, the loop of the program waiting for it
   (Shared.wait) included, which polls, as the program links Domain *)

let () =
  let flag = Atomic.make false in
  let d =
    Domain.spawn (fun () ->
        let l = ref [] in
        for i = 1 to 100_000 do
          l := [ i ]
        done;
        Gc.full_major ();
        Atomic.set flag true;
        List.length !l)
  in
  Shared.wait flag;
  Printf.printf "pdomain: %d\n" (Domain.join d)
