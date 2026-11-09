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

open! Stdlib
open Code
open! C_primitives

(* Program-wide facts about variables *)

type facts =
  { is_int : BitSet.t  (** The variable holds an integer *)
  ; is_static : BitSet.t  (** The variable holds static data, outside the heap *)
  }

let compute_facts (p : program) closures =
  let is_int = BitSet.create' (Var.count ()) in
  let is_static = BitSet.create' (Var.count ()) in
  let set_int x = BitSet.set is_int (Var.idx x) in
  let int x = BitSet.mem is_int (Var.idx x) in
  (* Definitions which imply an integer. Not the uses: a variable switched
     on or compared as an integer may be a block on other paths, after the
     test [IsInt] of a pattern matching *)
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, Constant (Int _)) -> set_int x
          | Let (x, Constant (Tuple (_, [||], _, _))) -> BitSet.set is_static (Var.idx x)
          | Let (x, Constant _) -> BitSet.set is_static (Var.idx x)
          | Let (x, Closure _) -> (
              match Var.Map.find_opt x closures with
              | Some { Closure_conversion.free_variables = []; _ } ->
                  BitSet.set is_static (Var.idx x)
              | Some _ | None -> ())
          | Let (x, Block (_, [||], _, _, _)) -> BitSet.set is_static (Var.idx x)
          | Let (x, Field (_, _, Immediate)) -> set_int x
          | Let (x, Prim (Array_get Immediate, _)) -> set_int x
          | Let
              (x, Prim ((Vectlength _ | Not | IsInt _ | Eq _ | Neq _ | Lt | Le | Ult), _))
            -> set_int x
          | Let (x, Prim (Extern (name, _), _)) ->
              if StringSet.mem name int_primitives then set_int x
          | Offset_ref _ | Let _ | Assign _ | Set_field _ | Array_set _ | Event _ -> ()))
    p.blocks;
  (* Block parameters and assigned variables all of whose values are
     integers *)
  let sources = Var.Hashtbl.create 1024 in
  let add_source x y =
    Var.Hashtbl.replace
      sources
      x
      (y :: (Var.Hashtbl.find_opt sources x |> Option.value ~default:[]))
  in
  let add_cont (pc, args) =
    let params = (Addr.Map.find pc p.blocks).params in
    List.iter2 params args ~f:add_source
  in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Assign (x, y) -> add_source x y
          | Let (_, Closure (params, cont, _)) ->
              (* Function parameters can be anything *)
              List.iter params ~f:(fun x -> add_source x x);
              add_cont cont
          | _ -> ());
      match block.branch with
      | Branch cont | Poptrap cont -> add_cont cont
      | Cond (_, c1, c2) ->
          add_cont c1;
          add_cont c2
      | Switch (_, conts) -> Array.iter conts ~f:add_cont
      | Pushtrap (c1, x, c2) ->
          add_source x x;
          add_cont c1;
          add_cont c2
      | Return _ | Raise _ | Stop -> ())
    p.blocks;
  let changed = ref true in
  while !changed do
    changed := false;
    Var.Hashtbl.iter
      (fun x l ->
        if (not (int x)) && List.for_all l ~f:(fun y -> (not (Var.equal x y)) && int y)
        then (
          set_int x;
          changed := true))
      sources
  done;
  (* An assigned variable or a block parameter is static only if it is
     always the same static value: we do not track that *)
  Var.Hashtbl.iter (fun x _ -> BitSet.unset is_static (Var.idx x)) sources;
  { is_int; is_static }
