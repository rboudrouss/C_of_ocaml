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

(* A plugin performing effects (OCaml 5, --effects double-translation) *)

type _ Effect.t += Get : int Effect.t

let () =
  let open Effect.Deep in
  let r =
    try_with
      (fun () -> Effect.perform Get + 1)
      ()
      { effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | Get -> Some (fun (k : (a, _) continuation) -> continue k 41)
            | _ -> None)
      }
  in
  Printf.printf "peffect: %d\n" r
