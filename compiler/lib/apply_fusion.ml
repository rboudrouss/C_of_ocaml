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

(* An application whose result is only applied, right after:
   [p = f a; r = p b] becomes [r = f a b], an application to more
   arguments (not exact), which applies [f] to [a] and its result to [b],
   or [f] to [a b] at once if its arity is that of both: the partial
   application [f a], a closure, is no longer allocated then. The
   arguments [b] are defined before [f a] or are constants (nothing runs
   in between but debug events and constants), [p] has no other use, and [f a] is not an exact call
   (of a known function, which a generic application would replace). *)

open! Stdlib
open Code

let debug = Debug.find "apply-fusion"

let f p =
  let uses = Var.Hashtbl.create 1024 in
  Addr.Map.iter
    (fun _ b ->
      Freevars.iter_block_free_vars
        (fun x ->
          Var.Hashtbl.replace
            uses
            x
            (1 + Option.value ~default:0 (Var.Hashtbl.find_opt uses x)))
        b)
    p.blocks;
  let count = ref 0 in
  let rec fuse l =
    match l with
    | Let (p, Apply { f; args = a; exact = false }) :: rem -> (
        let rec skip_events acc l =
          match l with
          | ((Event _ | Let (_, Constant _)) as e) :: r -> skip_events (e :: acc) r
          | _ -> List.rev acc, l
        in
        let events, rem' = skip_events [] rem in
        match rem' with
        | Let (r, Apply { f = p'; args = b; exact = _ }) :: rem''
          when Var.equal p p'
               && Poly.equal (Var.Hashtbl.find_opt uses p) (Some 1)
               && not (List.mem ~eq:Var.equal p b) ->
            incr count;
            events @ fuse (Let (r, Apply { f; args = a @ b; exact = false }) :: rem'')
        | _ -> List.hd l :: fuse rem)
    | i :: rem -> i :: fuse rem
    | [] -> []
  in
  let blocks =
    Addr.Map.map
      (fun b ->
        if
          List.exists b.body ~f:(fun i ->
              match i with
              | Let (_, Apply _) -> true
              | _ -> false)
        then { b with body = fuse b.body }
        else b)
      p.blocks
  in
  if debug () then Format.eprintf "apply-fusion: %d applications@." !count;
  { p with blocks }
