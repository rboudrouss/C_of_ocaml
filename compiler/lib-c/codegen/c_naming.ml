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

(* The tables of the program, and the stable C names of its globals and
   functions *)

open! Stdlib
open Code
open! C_repr
open! C_output

(* The size of the runtime's table of global data, where the predefined
   exceptions are registered *)
let global_data_size (p : program) =
  Addr.Map.fold
    (fun _ block n ->
      List.fold_left block.body ~init:n ~f:(fun n i ->
          match i with
          | Let (_, Prim (Extern ("caml_register_global_by_index", _), [ _; Pc (Int i) ]))
            -> max n (Targetint.to_int_exn i + 1)
          | _ -> n))
    p.blocks
    16

let collect_functions p =
  (* All the closures, with their parameters, entry point and location *)
  fold_closures
    p
    (fun name_opt params cont loc acc ->
      match name_opt with
      | Some f -> (f, params, cont, loc) :: acc
      | None -> acc)
    []

let source_file (pi : Parse_info.t) =
  match pi.name, pi.src with
  | Some n, _ | None, Some n -> n
  | None, None -> ""

(* The C name of each global, after the last debug event before its
   definition in the program, its name and its rank among the globals with
   the same event and name (see [name_functions]) *)
let name_globals (p : program) global_list names =
  let set = Var.Set.of_list global_list in
  let ranks = String.Hashtbl.create 1024 in
  let last = ref "" in
  let name x =
    if Var.Set.mem x set && not (Var.Hashtbl.mem names x)
    then (
      let base =
        match Var.get_name x with
        | Some s -> sanitize s
        | None -> ""
      in
      let key = !last ^ ":" ^ base in
      let rank =
        match String.Hashtbl.find_opt ranks key with
        | Some n -> n
        | None -> 0
      in
      String.Hashtbl.replace ranks key (rank + 1);
      Var.Hashtbl.add
        names
        x
        (Printf.sprintf
           "%s%s%s"
           (digest_name "camlc_g_" key)
           (if rank = 0 then "" else Printf.sprintf "_%d" rank)
           (if String.equal base "" then "" else "_" ^ base)))
  in
  Addr.Map.iter
    (fun _ (block : block) ->
      List.iter block.params ~f:name;
      List.iter block.body ~f:(fun i ->
          match i with
          | Event pi -> last := Printf.sprintf "%s:%d:%d" (source_file pi) pi.line pi.col
          | Let (x, _) -> name x
          | Assign _ | Set_field _ | Offset_ref _ | Array_set _ -> ()))
    p.blocks;
  (* Not defined in a block, which should not happen *)
  List.iter global_list ~f:(fun x ->
      if not (Var.Hashtbl.mem names x)
      then Var.Hashtbl.add names x (Printf.sprintf "camlc_g_v%d" (Var.idx x)))

(* The C names of the functions do not depend on the numbering of the
   variables, which an edit anywhere in the program shifts: they are named
   after their location in the source and their name (and their rank among
   the functions with the same location and name, the copies made by
   inlining for instance), so that the code of the rest of the program is
   left unchanged *)
(* The first debug event of the code starting at [pc] *)
let first_event blocks (pc, _) =
  let rec search visited todo =
    match todo with
    | [] -> None
    | pc :: r when Addr.Set.mem pc visited || Addr.Set.cardinal visited > 32 ->
        search visited r
    | pc :: r -> (
        let block = Addr.Map.find pc blocks in
        match
          List.find_map block.body ~f:(fun i ->
              match i with
              | Event pi -> Some pi
              | _ -> None)
        with
        | Some _ as loc -> loc
        | None ->
            let next = ref [] in
            Code.fold_children blocks pc (fun pc' () -> next := pc' :: !next) ();
            search (Addr.Set.add pc visited) (r @ List.rev !next))
  in
  search Addr.Set.empty [ pc ]

let name_functions out blocks functions =
  let ranks = String.Hashtbl.create 1024 in
  (* Without a location, the first debug event of the body *)
  let first_event = first_event blocks in
  List.iter functions ~f:(fun (f, _, cont, (loc : Parse_info.t option)) ->
      let loc =
        match loc with
        | Some _ -> loc
        | None -> first_event cont
      in
      let base =
        match Var.get_name f with
        | Some s -> sanitize s
        | None -> ""
      in
      let key =
        match loc with
        | Some pi -> Printf.sprintf "%s:%d:%d:%s" (source_file pi) pi.line pi.col base
        | None -> ":" ^ base
      in
      let rank =
        match String.Hashtbl.find_opt ranks key with
        | Some n -> n
        | None -> 0
      in
      String.Hashtbl.replace ranks key (rank + 1);
      let hash = String.sub (Digest.to_hex (Digest.string key)) ~pos:0 ~len:12 in
      Var.Hashtbl.replace
        out.fun_names
        f
        (Printf.sprintf
           "camlc_f%s%s%s"
           hash
           (if rank = 0 then "" else Printf.sprintf "_%d" rank)
           (if String.equal base "" then "" else "_" ^ base)))
