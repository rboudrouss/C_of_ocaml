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

(* Representations of the values in C, and small helpers of the
   generated C *)

open! Stdlib

(* 4 on wasm32 (31-bit integers), 8 on wasm64 (63-bit integers) *)
let word_size () = if Targetint.num_bits () > 32 then 8 else 4

(* Effect handlers: functions which may perform an effect have a CPS
   version, run on fibers by the runtime (see camlc.h) *)
let double_translation () =
  match Config.effects () with
  | `Double_translation -> true
  | `Disabled | `Cps | `Jspi | `Native -> false

(****)

let sanitize s =
  String.map s ~f:(fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> c
      | _ -> '_')

let c_string s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter s ~f:(fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | ' ' .. '~' -> Buffer.add_char b c
      | _ -> Buffer.add_string b (Printf.sprintf "\\%03o" (Char.code c)));
  Buffer.add_char b '"';
  Buffer.contents b

let tagged_int i =
  (* The tagged representation fits in a word *)
  let i = Targetint.to_int64 i in
  let t = Int64.add (Int64.shift_left i 1) 1L in
  if word_size () = 8
  then Printf.sprintf "((value)0x%LxULL)" t
  else Printf.sprintf "((value)0x%lxu)" (Int64.to_int32 t)

(* The representation of a variable in C: OCaml values, or unboxed numbers
   (see {!Typing}) *)
type repr =
  | Value
  | Double
  | Unboxed_int64
  | Unboxed_int32
  | Unboxed_nativeint

let c_type r =
  match r with
  | Value -> "value"
  | Double -> "double"
  | Unboxed_int64 -> "int64_t"
  | Unboxed_int32 -> "int32_t"
  | Unboxed_nativeint -> "intnat"

(* The field of [union camlc_word] holding a number in representation [r] *)
let repr_letter r =
  match r with
  | Value -> 'v'
  | Double -> 'd'
  | Unboxed_int64 -> 'l'
  | Unboxed_int32 -> 'i'
  | Unboxed_nativeint -> 'n'

let repr_of_type (t : Typing.typ) =
  match t with
  | Number (Float, Unboxed) -> Double
  | Number (Int64, Unboxed) -> Unboxed_int64
  | Number (Int32, Unboxed) -> Unboxed_int32
  | Number (Nativeint, Unboxed) -> Unboxed_nativeint
  | _ -> Value

let box r e =
  match r with
  | Value -> e
  | Double -> Printf.sprintf "camlc_box_double(%s)" e
  | Unboxed_int64 -> Printf.sprintf "camlc_box_int64(%s)" e
  | Unboxed_int32 -> Printf.sprintf "camlc_box_int32(%s)" e
  | Unboxed_nativeint -> Printf.sprintf "camlc_box_nativeint(%s)" e

let unbox r e =
  match r with
  | Value -> e
  | Double -> Printf.sprintf "Double_val(%s)" e
  | Unboxed_int64 -> Printf.sprintf "Int64_val(%s)" e
  | Unboxed_int32 -> Printf.sprintf "Int32_val(%s)" e
  | Unboxed_nativeint -> Printf.sprintf "Nativeint_val(%s)" e

let repr_equal (r : repr) r' = Poly.equal r r'
