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

(* A unit of the program, used by the plugins *)

let counter = ref 0

let hooks : (int -> int) list ref = ref []

let register f = hooks := f :: !hooks

let run x = List.map (fun f -> f x) (List.rev !hooks)

exception Plugin_error of string

(* Recursions which the plugins make deep (c_of_ocaml --cps-stack) *)
let rec count n = if n = 0 then 0 else 1 + count (n - 1)

let down f n = if n = 0 then 0 else 1 + f (n - 1)
