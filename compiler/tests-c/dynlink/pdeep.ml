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

(* A plugin with deep recursions, compiled with --cps-stack as the program:
   its own, one going through a function of the program, and one of the
   program *)

let n = 1_000_000

let rec count n = if n = 0 then 0 else 1 + count (n - 1)

let rec down n = Shared.down down n

let () = Printf.printf "pdeep: %d %d %d\n" (count n) (down n) (Shared.count n)
