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

(* The primitives on JavaScript values, with literal arrays of arguments and
   constant strings: with C, they remain calls to the generic primitives *)

open Js_of_ocaml

let () =
  let o =
    Js.Unsafe.obj [| "a", Js.Unsafe.inject 1; "b", Js.Unsafe.inject (Js.string "x") |]
  in
  let r : int = Js.Unsafe.meth_call o "m" [| Js.Unsafe.inject 2; Js.Unsafe.inject 3 |] in
  let f = Js.Unsafe.js_expr "(function (x) { return x + 1; })" in
  let s : int = Js.Unsafe.fun_call f [| Js.Unsafe.inject 4 |] in
  let d : int = Js.Unsafe.call f o [| Js.Unsafe.inject 5 |] in
  let c =
    Js.Unsafe.new_obj (Js.Unsafe.get Js.Unsafe.global "Date") [| Js.Unsafe.inject 0 |]
  in
  Js.Unsafe.set o "c" (Js.string "y");
  Js.Unsafe.delete o "a";
  let e : int = Js.Unsafe.get o "b" in
  let v = Js.Unsafe.pure_js_expr "globalThis" in
  let w = Js.Unsafe.js_expr "42" in
  let n = Js_of_ocaml.Effect_js.assume_no_perform (fun () -> r + s) in
  let a = Js.array [| d; e; n |] in
  Js.Unsafe.set
    Js.Unsafe.global
    "result"
    (Js.Unsafe.obj [| "c", Js.Unsafe.inject c; "v", v; "w", w |]);
  ignore
    (Js.Unsafe.meth_call
       v
       "f"
       [| Js.Unsafe.inject a; Js.Unsafe.inject (Js.bytestring "z") |]
      : int)
