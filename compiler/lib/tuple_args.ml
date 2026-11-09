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

(* Tupled parameters passed as several parameters.

   The bytecode allocates the tuple of a function written [let rec f (x, y)
   = ...] at each call, which ocamlopt avoids. A parameter of a function
   [f] is passed as the fields of the tuple when:
   - [f] does not escape: all its occurrences are the function of an
     application to as many arguments as it has parameters;
   - the parameter is only read by [Field], its values never seen whole;
     the parameter of the entry block of [f] it is passed to, if any, the
     same, the other edges to this block being branches (the self tail
     calls of [f], see [Tailcall]);
   - at each call, and each branch to the entry block, the argument is a
     block allocated in the same block of code, with no [Assign] in
     between, whose only use is this argument, or a constant tuple; all of
     the same size, larger than the fields read.
   Nobody else sees the block, which is not modified then: the fields read
   by [f] are those given at the allocation. The block is no longer
   allocated, and [f] gets a parameter for each of its fields. *)

open! Stdlib
open Code

let debug = Debug.find "tuple-args"

(* At most this many fields *)
let max_fields = 8

(* An application of a function or a branch to an entry block: its block of
   code, its position in the body (that of the branch: after the body), and
   its arguments *)
type site =
  { site_pc : Addr.t
  ; index : int
  ; args : Var.t list
  }

let f p =
  let closures = Var.Hashtbl.create 64 in
  let uses = Var.Hashtbl.create 1024 in
  let callee_uses = Var.Hashtbl.create 64 in
  let field_uses = Var.Hashtbl.create 1024 in
  let calls = Var.Hashtbl.create 64 in
  let branches = Addr.Hashtbl.create 64 in
  let preds = Addr.Hashtbl.create 1024 in
  let defs = Var.Hashtbl.create 64 in
  let max_field = Var.Hashtbl.create 1024 in
  let not_plain = Var.Hashtbl.create 16 in
  let incr tbl x =
    Var.Hashtbl.replace tbl x (1 + Option.value ~default:0 (Var.Hashtbl.find_opt tbl x))
  in
  let count tbl x = Option.value ~default:0 (Var.Hashtbl.find_opt tbl x) in
  let add_to tbl x v =
    Var.Hashtbl.replace tbl x (v :: Option.value ~default:[] (Var.Hashtbl.find_opt tbl x))
  in
  let pred (pc, _) =
    Addr.Hashtbl.replace
      preds
      pc
      (1 + Option.value ~default:0 (Addr.Hashtbl.find_opt preds pc))
  in
  Addr.Map.iter
    (fun pc block ->
      List.iteri block.body ~f:(fun index i ->
          Freevars.iter_instr_free_vars (fun x -> incr uses x) i;
          match i with
          | Let (x, Closure (params, cont, _)) ->
              Var.Hashtbl.replace closures x (params, cont);
              pred cont
          | Let (_, Apply { f; args; _ }) ->
              incr callee_uses f;
              add_to calls f { site_pc = pc; index; args }
          | Let (x, Field (y, n, kind)) ->
              incr field_uses y;
              (match kind with
              | Non_float | Immediate -> ()
              | Float -> Var.Hashtbl.replace not_plain y ());
              (match Var.Hashtbl.find_opt max_field y with
              | Some m when m >= n -> ()
              | _ -> Var.Hashtbl.replace max_field y n);
              ignore x
          | Let (x, ((Block _ | Constant (Tuple _)) as e)) ->
              Var.Hashtbl.replace defs x (pc, index, e)
          | _ -> ());
      Freevars.iter_last_free_var (fun x -> incr uses x) block.branch;
      match block.branch with
      | Return _ | Raise _ | Stop -> ()
      | Branch ((pc', args) as c) ->
          pred c;
          Addr.Hashtbl.replace
            branches
            pc'
            ({ site_pc = pc; index = List.length block.body; args }
            :: Option.value ~default:[] (Addr.Hashtbl.find_opt branches pc'))
      | Poptrap c -> pred c
      | Cond (_, c1, c2) | Pushtrap (c1, _, c2) ->
          pred c1;
          pred c2
      | Switch (_, a) -> Array.iter a ~f:pred)
    p.blocks;
  let size e =
    match e with
    | Block (tag, fields, _, _, _) when tag <> Obj.double_array_tag ->
        Some (Array.length fields)
    | Constant (Tuple (tag, fields, _, _)) when tag <> Obj.double_array_tag ->
        Some (Array.length fields)
    | _ -> None
  in
  (* The size of the argument [y] of [site], if it can be replaced by its
     fields *)
  let argument_size site y =
    match Var.Hashtbl.find_opt defs y with
    | Some (pc, index, (Block _ as e))
      when pc = site.site_pc && index < site.index && count uses y = 1 ->
        let body = (Addr.Map.find pc p.blocks).body in
        if
          List.filteri body ~f:(fun j _ -> j > index && j < site.index)
          |> List.exists ~f:(fun i ->
              match i with
              | Assign _ -> true
              | _ -> false)
        then None
        else size e
    | Some (_, _, (Constant _ as e)) -> size e
    | _ -> None
  in
  (* The variable a closure parameter is read from, and its position among
     the parameters of the entry block, if it is passed to it *)
  let entry_param (params, (pc, args)) k =
    let x = List.nth params k in
    match
      List.filter (List.mapi args ~f:(fun j y -> j, y)) ~f:(fun (_, y) -> Var.equal x y)
    with
    | [] -> Some (x, None)
    | [ (j, _) ] ->
        let others = Option.value ~default:[] (Addr.Hashtbl.find_opt branches pc) in
        if
          Option.value ~default:0 (Addr.Hashtbl.find_opt preds pc)
          = 1 + List.length others
        then Some (List.nth (Addr.Map.find pc p.blocks).params j, Some j)
        else None
    | _ -> None
  in
  (* The parameters passed as fields: function -> positions and sizes *)
  let changed = Var.Hashtbl.create 16 in
  Var.Hashtbl.iter
    (fun f ((params, (pc, _)) as closure) ->
      let n_params = List.length params in
      let fcalls = Option.value ~default:[] (Var.Hashtbl.find_opt calls f) in
      if
        count uses f > 0
        && count uses f = count callee_uses f
        && List.for_all fcalls ~f:(fun s -> List.length s.args = n_params)
      then
        let positions =
          List.filter_map
            (List.mapi params ~f:(fun k _ -> k))
            ~f:(fun k ->
              match entry_param closure k with
              | Some (y, j)
                when count uses y > 0
                     && count uses y = count field_uses y
                     && not (Var.Hashtbl.mem not_plain y) -> (
                  let read = Var.Hashtbl.find max_field y in
                  let sizes =
                    List.map fcalls ~f:(fun s -> argument_size s (List.nth s.args k))
                    @
                    match j with
                    | None -> []
                    | Some j ->
                        List.map
                          (Option.value ~default:[] (Addr.Hashtbl.find_opt branches pc))
                          ~f:(fun s -> argument_size s (List.nth s.args j))
                  in
                  match sizes with
                  | Some n :: rem
                    when n > read
                         && n <= max_fields
                         && List.for_all rem ~f:(fun s ->
                             match s with
                             | Some n' -> n' = n
                             | None -> false) -> Some (k, n)
                  | _ -> None)
              | _ -> None)
        in
        if not (List.is_empty positions) then Var.Hashtbl.replace changed f positions)
    closures;
  if Var.Hashtbl.length changed = 0
  then p
  else
    (* The new parameters of the closures and of their entry blocks, the
       positions of the arguments to expand in the branches to these
       blocks, and the variables read from the old parameters *)
    let new_params = Var.Hashtbl.create 16 in
    let entry_positions = Addr.Hashtbl.create 16 in
    Var.Hashtbl.iter
      (fun f positions ->
        let ((params, (pc, _)) as closure) = Var.Hashtbl.find closures f in
        if debug ()
        then
          Format.eprintf
            "tuple-args: %a: %s@."
            Var.print
            f
            (String.concat
               ~sep:", "
               (List.map positions ~f:(fun (k, n) ->
                    Printf.sprintf "parameter %d (%d fields)" k n)));
        List.iter positions ~f:(fun (k, n) ->
            let x = List.nth params k in
            Var.Hashtbl.replace new_params x (Array.init n ~f:(fun _ -> Var.fork x));
            match entry_param closure k with
            | Some (_, None) -> ()
            | Some (y, Some j) ->
                Var.Hashtbl.replace new_params y (Array.init n ~f:(fun _ -> Var.fork y));
                Addr.Hashtbl.replace
                  entry_positions
                  pc
                  (j
                  :: Option.value ~default:[] (Addr.Hashtbl.find_opt entry_positions pc))
            | None -> assert false))
      changed;
    let expand l =
      List.concat_map l ~f:(fun x ->
          match Var.Hashtbl.find_opt new_params x with
          | Some a -> Array.to_list a
          | None -> [ x ])
    in
    (* The fields of the arguments, with the definitions of the fields of
       constant tuples; the blocks are no longer allocated *)
    let dropped_blocks = Var.Hashtbl.create 64 in
    let fields y =
      match Var.Hashtbl.find defs y with
      | _, _, Block (_, fields, _, _, _) ->
          Var.Hashtbl.replace dropped_blocks y ();
          [], Array.to_list fields
      | _, _, Constant (Tuple (_, fields, _, _)) ->
          let vars = Array.map fields ~f:(fun _ -> Var.fresh ()) in
          ( Array.to_list (Array.map2 vars fields ~f:(fun v c -> Let (v, Constant c)))
          , Array.to_list vars )
      | _ -> assert false
    in
    let expand_args positions args =
      let pre, args =
        List.split
          (List.mapi args ~f:(fun k y ->
               if List.mem ~eq:Int.equal k positions then fields y else [], [ y ]))
      in
      List.concat pre, List.concat args
    in
    let subst = ref Var.Map.empty in
    let blocks =
      Addr.Map.map
        (fun block ->
          let body =
            List.concat_map block.body ~f:(fun i ->
                match i with
                | Let (x, Closure (params, (pc, args), hint))
                  when Var.Hashtbl.mem changed x ->
                    [ Let (x, Closure (expand params, (pc, expand args), hint)) ]
                | Let (x, Field (y, n, _)) when Var.Hashtbl.mem new_params y ->
                    subst := Var.Map.add x (Var.Hashtbl.find new_params y).(n) !subst;
                    []
                | Let (x, Apply { f; args; exact }) when Var.Hashtbl.mem changed f ->
                    let pre, args =
                      expand_args (List.map (Var.Hashtbl.find changed f) ~f:fst) args
                    in
                    pre @ [ Let (x, Apply { f; args; exact }) ]
                | _ -> [ i ])
          in
          match block.branch with
          | Branch (pc, args) when Addr.Hashtbl.mem entry_positions pc ->
              let pre, args = expand_args (Addr.Hashtbl.find entry_positions pc) args in
              { block with body = body @ pre; branch = Branch (pc, args) }
          | _ -> { block with body })
        p.blocks
    in
    let blocks =
      Addr.Map.mapi
        (fun pc block ->
          { block with
            params =
              (if Addr.Hashtbl.mem entry_positions pc
               then expand block.params
               else block.params)
          ; body =
              List.filter block.body ~f:(fun i ->
                  match i with
                  | Let (y, Block _) -> not (Var.Hashtbl.mem dropped_blocks y)
                  | _ -> true)
          })
        blocks
    in
    Subst.Excluding_Binders.program (Subst.from_map !subst) { p with blocks }
