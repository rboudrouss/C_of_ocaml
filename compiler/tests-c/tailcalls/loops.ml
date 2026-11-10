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

(* Long loops of tail calls through closures, in constant space also
   without the tail calls of WebAssembly (TAIL_CALLS=no
   tools/c_of_ocaml_tests.sh): their calls left pending return to the first
   call which is not a tail call (see [CAMLC_PENDING]) *)

let n = 3_000_000

(* An unknown call *)
let ping f k = if k = 0 then 0 else f (k - 1)

let rec go k = ping (Sys.opaque_identity go) k

(* A chain of continuations, each calling the next *)
let rec cps k kont = if k = 0 then kont 0 else cps (k - 1) (fun x -> kont (x + 1))

(* A state machine *)
let states = Array.make 3 (fun _ -> 0)

let () =
  states.(0) <- (fun k -> if k = 0 then 0 else states.(1) (k - 1));
  states.(1) <- (fun k -> if k = 0 then 1 else states.(2) (k - 1));
  states.(2) <- (fun k -> if k = 0 then 2 else states.(0) (k - 1))

(* Partial applications and applications to more arguments *)
let rec over k = if k = 0 then fun x -> x else fun x -> over (k - 1) (x + 1)

let rec h k = if k = 0 then 0 else (Sys.opaque_identity two) (k - 1) 0

and two k _ = h k

(* An exception raised at the end of the loop, caught below it, and the
   loops in a non-tail call (the result is used) *)
let rec raising k = if k = 0 then raise Exit else (Sys.opaque_identity raising) (k - 1)

let caught k = try raising k with Exit -> -1

(* Non-tail calls of loops, under a deep recursion *)
let rec deep d = if d = 0 then 0 else states.(0) 31 + deep (d - 1)

let () =
  Printf.printf "go %d\n" (go n);
  Printf.printf "cps %d\n" (cps 1_000_000 (fun x -> x));
  Printf.printf "states %d\n" (states.(0) n);
  Printf.printf "over %d\n" (over 1_000_000 0);
  Printf.printf "two %d\n" (h n);
  Printf.printf "caught %d\n" (caught n);
  Printf.printf "sum %d\n" (List.fold_left (fun acc k -> acc + states.(0) k) 0 [ n; n + 1; n + 2 ]);
  Printf.printf "deep %d\n" (deep 10_000)
