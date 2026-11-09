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

(* The loops of code which runs once, run by functions called again and
   again.

   V8 compiles a function first with its baseline compiler (Liftoff), then,
   once it has run long enough, with its optimizing compiler, which the
   next calls use: a loop of a function called once (the toplevel code, a
   function called once from it out of its loops) runs to its end in the
   baseline code, about twice as slow, as there is no on-stack replacement
   for WebAssembly.

   Such an outermost loop becomes a closure [g] run from the loop header
   by a driver block. [g] runs at most [limit] iterations (a counter
   decremented by the back edges, the loop header then returning) and
   returns an integer saying what to do next, the values going through a
   mailbox, a block allocated at the start of the program: 0, call [g]
   again with the parameters of the loop header ([limit] doubled, from
   [first_limit] to [max_limit]); [j + 1], take the [j]-th edge out of the
   loop, with its arguments and the variables of the loop used after it
   (renamed out of [g]); [n + 1], return the value. [g] is closed (its
   free variables are parameters): neither it nor the values it returns
   are allocated. The blocks of the loop move to [g] unchanged, but for its
   edges. Loops are left as they are when they contain assignments,
   exception handlers not wholly inside them, parameters holding unboxed
   numbers (boxed through [g]), little code (which the C compiler may
   remove), or variables used after them with several edges out. *)

open! Stdlib
open Code

let debug = Debug.find "loop-reentry"

let print = Debug.find "loop-reentry-print"

(* The number of iterations before the first return to the driver, then
   doubled up to [max_limit] *)
let first_limit = 64

let max_limit = 1 lsl 20

(* At most this many free variables, passed as parameters to [g] (closed,
   it is not allocated) *)
let max_free = 32

let int n = Constant (Int (Targetint.of_int_exn n))

let block tag args =
  Block (tag, Array.of_list args, NotArray, Maybe_mutable, Block_desc.none)

let successors (b : block) =
  match b.branch with
  | Return _ | Raise _ | Stop -> []
  | Branch c | Poptrap c -> [ c ]
  | Cond (_, c1, c2) -> [ c1; c2 ]
  | Pushtrap (c1, _, c2) -> [ c1; c2 ]
  | Switch (_, a) -> Array.to_list a

let map_conts f (b : block) =
  let branch =
    match b.branch with
    | (Return _ | Raise _ | Stop) as l -> l
    | Branch c -> Branch (f c)
    | Poptrap c -> Poptrap (f c)
    | Cond (x, c1, c2) -> Cond (x, f c1, f c2)
    | Pushtrap (c1, x, c2) -> Pushtrap (f c1, x, f c2)
    | Switch (x, a) -> Switch (x, Array.map a ~f)
  in
  { b with branch }

let f p =
  let blocks = ref p.blocks in
  let free_pc = ref p.free_pc in
  let new_block b =
    let pc = !free_pc in
    incr free_pc;
    blocks := Addr.Map.add pc b !blocks;
    pc
  in
  (* The primitive defining each variable *)
  let prim_defs = Var.Hashtbl.create 1024 in
  Addr.Map.iter
    (fun _ b ->
      List.iter b.body ~f:(fun i ->
          match i with
          | Let (x, Prim (Extern (name, _), _)) -> Var.Hashtbl.replace prim_defs x name
          | _ -> ()))
    p.blocks;
  (* The blocks using each variable *)
  let uses = Var.Hashtbl.create 1024 in
  Addr.Map.iter
    (fun pc b ->
      Freevars.iter_block_free_vars
        (fun x ->
          Var.Hashtbl.replace
            uses
            x
            (pc :: Option.value ~default:[] (Var.Hashtbl.find_opt uses x)))
        b)
    p.blocks;
  (* The blocks of the closures defined in some blocks *)
  let rec closure_blocks pcs acc =
    Addr.Set.fold
      (fun pc acc ->
        List.fold_left (Addr.Map.find pc p.blocks).body ~init:acc ~f:(fun acc i ->
            match i with
            | Let (_, Closure (_, (pc', _), _)) ->
                let rec visit pc acc =
                  if Addr.Set.mem pc acc
                  then acc
                  else
                    let acc = Addr.Set.add pc acc in
                    let acc = closure_blocks (Addr.Set.singleton pc) acc in
                    List.fold_left
                      (successors (Addr.Map.find pc p.blocks))
                      ~init:acc
                      ~f:(fun acc (pc, _) -> visit pc acc)
                in
                visit pc' acc
            | _ -> acc))
      pcs
      acc
  in
  let in_g = ref Addr.Set.empty in
  let count = ref 0 in
  (* The functions called once from the toplevel code, out of its loops *)
  let called_once =
    let g = Structure.control_flow_graph p.blocks p.start in
    let nodes = Structure.get_nodes g in
    let idom = Structure.immediate_dominators g in
    let rec dominates a b =
      a = b
      ||
      match Addr.Hashtbl.find_opt idom b with
      | Some b' -> dominates a b'
      | None -> false
    in
    (* The blocks of the loops of the toplevel code *)
    let in_loops = ref Addr.Set.empty in
    Addr.Set.iter
      (fun pc ->
        List.iter
          (successors (Addr.Map.find pc p.blocks))
          ~f:(fun (h, _) ->
            if Addr.Set.mem h nodes && dominates h pc
            then (
              (* A back edge: the blocks reaching [pc] without [h] *)
              let rec add pc =
                if not (Addr.Set.mem pc !in_loops)
                then (
                  in_loops := Addr.Set.add pc !in_loops;
                  if pc <> h
                  then
                    Addr.Set.iter
                      (fun pc' ->
                        if
                          List.exists
                            (successors (Addr.Map.find pc' p.blocks))
                            ~f:(fun (pc'', _) -> pc'' = pc)
                        then add pc')
                      nodes)
              in
              add h;
              add pc)))
      nodes;
    let closures = Var.Hashtbl.create 64 in
    let calls = Var.Hashtbl.create 64 in
    let all_uses = Var.Hashtbl.create 1024 in
    Addr.Map.iter
      (fun pc b ->
        Freevars.iter_block_free_vars
          (fun x ->
            Var.Hashtbl.replace
              all_uses
              x
              (1 + Option.value ~default:0 (Var.Hashtbl.find_opt all_uses x)))
          b;
        List.iter b.body ~f:(fun i ->
            match i with
            | Let (x, Closure (_, (entry, _), _)) -> Var.Hashtbl.replace closures x entry
            | Let (_, Apply { f; _ }) ->
                Var.Hashtbl.replace
                  calls
                  f
                  (pc :: Option.value ~default:[] (Var.Hashtbl.find_opt calls f))
            | _ -> ()))
      p.blocks;
    Var.Hashtbl.fold
      (fun f entry acc ->
        match Var.Hashtbl.find_opt calls f with
        | Some [ pc ]
          when Poly.equal (Var.Hashtbl.find_opt all_uses f) (Some 1)
               && Addr.Set.mem pc nodes
               && not (Addr.Set.mem pc !in_loops) -> entry :: acc
        | _ -> acc)
      closures
      []
  in
  let process start =
    (* The blocks of the function, their predecessors *)
    let g = Structure.control_flow_graph !blocks start in
    let nodes = Structure.get_nodes g in
    let preds = Addr.Hashtbl.create 64 in
    Addr.Set.iter
      (fun pc ->
        List.iter
          (successors (Addr.Map.find pc !blocks))
          ~f:(fun (pc', _) ->
            Addr.Hashtbl.replace
              preds
              pc'
              (Addr.Set.add
                 pc
                 (Option.value ~default:Addr.Set.empty (Addr.Hashtbl.find_opt preds pc')))))
      nodes;
    let idom = Structure.immediate_dominators g in
    let rec dominates a b =
      a = b
      ||
      match Addr.Hashtbl.find_opt idom b with
      | Some b' -> dominates a b'
      | None -> false
    in
    let preds_of pc =
      Option.value ~default:Addr.Set.empty (Addr.Hashtbl.find_opt preds pc)
    in
    (* The natural loop of each header *)
    let loop h =
      let back = Addr.Set.filter (fun u -> dominates h u) (preds_of h) in
      if Addr.Set.is_empty back
      then None
      else
        let body = ref (Addr.Set.singleton h) in
        let rec add pc =
          if not (Addr.Set.mem pc !body)
          then (
            body := Addr.Set.add pc !body;
            Addr.Set.iter add (preds_of pc))
        in
        Addr.Set.iter add back;
        Some (back, !body)
    in
    let handled = ref Addr.Set.empty in
    List.iter (Structure.blocks_in_reverse_post_order g) ~f:(fun h ->
        if not (Addr.Set.mem h !handled)
        then
          match loop h with
          | None -> ()
          | Some (back, body) ->
              handled := Addr.Set.union body !handled;
              let get pc = Addr.Map.find pc !blocks in
              (* The variables defined in the loop *)
              let defined = ref Var.Map.empty in
              let ok = ref true in
              let size = ref 0 in
              Addr.Set.iter
                (fun pc ->
                  let b = get pc in
                  Freevars.iter_block_bound_vars
                    (fun x -> defined := Var.Map.add x pc !defined)
                    b;
                  List.iter b.body ~f:(fun i ->
                      match i with
                      | Assign _ -> ok := false
                      | Event _ -> ()
                      | Let (_, Apply _) -> size := !size + 8
                      | Let (_, Prim (Extern (name, _), _))
                        when String.length name > 0 && not (Char.equal name.[0] '%') ->
                          (* A call: not a loop which the C compiler may
                             remove *)
                          size := !size + 8
                      | _ -> incr size);
                  match b.branch with
                  | Pushtrap ((pc1, _), _, (pc2, _)) ->
                      (* A handler of the loop *)
                      if not (Addr.Set.mem pc1 body && Addr.Set.mem pc2 body)
                      then ok := false
                  | Poptrap (pc', _) ->
                      (* Not the end of a handler outside the loop *)
                      if not (Addr.Set.mem pc' body) then ok := false
                  | _ -> ())
                body;
              (* The parameters of the loop header holding numbers which the
                 code keeps unboxed: boxed when passed to [g] *)
              let boxed_number x =
                match Var.Hashtbl.find_opt prim_defs x with
                | Some name ->
                    List.exists
                      [ "caml_int64_"; "caml_int32_"; "caml_nativeint_" ]
                      ~f:(fun p ->
                        String.length name >= String.length p
                        && String.equal (String.sub name ~pos:0 ~len:(String.length p)) p)
                    ||
                    let n = String.length name in
                    n >= 6 && String.equal (String.sub name ~pos:(n - 6) ~len:6) "_float"
                | None -> false
              in
              Addr.Set.iter
                (fun pc ->
                  List.iter
                    (successors (get pc))
                    ~f:(fun (pc', args) ->
                      if pc' = h && List.exists args ~f:boxed_number then ok := false))
                body;
              (* The blocks which move to [g]; the variables of the loop used
                 out of them *)
              let gset = closure_blocks body body in
              let live_out =
                Var.Map.fold
                  (fun x _ acc ->
                    if
                      List.exists
                        (Option.value ~default:[] (Var.Hashtbl.find_opt uses x))
                        ~f:(fun pc -> not (Addr.Set.mem pc gset))
                    then x :: acc
                    else acc)
                  !defined
                  []
                |> List.rev
              in
              (* The edges out of the loop *)
              let exits =
                Addr.Set.fold
                  (fun pc acc ->
                    List.fold_left
                      (successors (get pc))
                      ~init:acc
                      ~f:(fun acc (pc', _) ->
                        if Addr.Set.mem pc' body || List.mem ~eq:Int.equal pc' acc
                        then acc
                        else pc' :: acc))
                  body
                  []
                |> List.rev
              in
              let returns =
                Addr.Set.exists
                  (fun pc ->
                    match (get pc).branch with
                    | Return _ -> true
                    | _ -> false)
                  body
              in
              let n_exits = List.length exits in
              (* The edges out of the loop: with the variables used after the
                 loop, a single one, from a block where they are defined *)
              let exit_edges =
                Addr.Set.fold
                  (fun pc acc ->
                    List.fold_left
                      (successors (get pc))
                      ~init:acc
                      ~f:(fun acc (pc', _) ->
                        if Addr.Set.mem pc' body then acc else (pc, pc') :: acc))
                  body
                  []
              in
              let live_out_ok =
                List.is_empty live_out
                ||
                match exit_edges with
                | [ (u, _) ] ->
                    (not returns)
                    && List.for_all live_out ~f:(fun x ->
                        dominates (Var.Map.find x !defined) u)
                | _ -> false
              in
              if debug ()
              then
                Format.eprintf
                  "loop %d: %d blocks, size %d, %d live out, %d exit edges, ok %b, live \
                   out ok %b@."
                  h
                  (Addr.Set.cardinal body)
                  !size
                  (List.length live_out)
                  (List.length exit_edges)
                  !ok
                  live_out_ok;
              (* The tags of the blocks returned: 0, n_exits + 1 at most *)
              (* The free variables of [g], its parameters *)
              let bound = ref Var.Set.empty and used = ref Var.Set.empty in
              Addr.Set.iter
                (fun pc ->
                  let b = get pc in
                  Freevars.iter_block_bound_vars
                    (fun x -> bound := Var.Set.add x !bound)
                    b;
                  Freevars.iter_block_free_vars (fun x -> used := Var.Set.add x !used) b;
                  List.iter b.body ~f:(fun i ->
                      match i with
                      | Let (_, Closure (params, _, _)) ->
                          List.iter params ~f:(fun x -> bound := Var.Set.add x !bound)
                      | _ -> ()))
                gset;
              let free = Var.Set.elements (Var.Set.diff !used !bound) in
              if
                !ok
                && live_out_ok
                && !size >= 8
                && n_exits + 1 < 200
                && h <> start
                && List.length free <= max_free
              then (
                if debug ()
                then
                  Format.eprintf
                    "transform %d (start %d): %d blocks, %d free variables@."
                    h
                    start
                    (Addr.Set.cardinal gset)
                    (List.length free);
                in_g := Addr.Set.union gset !in_g;
                incr count;
                let header = get h in
                let n_fields =
                  List.fold_left
                    exits
                    ~init:(max (List.length header.params) (if returns then 1 else 0))
                    ~f:(fun n pc' ->
                      max n (List.length (get pc').params + List.length live_out))
                in
                (* The mailbox, allocated at the start of the program *)
                let mailbox = Var.fresh_n "mailbox" in
                (let zero = Var.fresh () in
                 let b = get p.start in
                 blocks :=
                   Addr.Map.add
                     p.start
                     { b with
                       body =
                         Let (zero, int 0)
                         :: Let
                              ( mailbox
                              , block
                                  0
                                  (List.init ~len:(max 1 n_fields) ~f:(fun _ -> zero)) )
                         :: b.body
                     }
                     !blocks);
                (* The parameters of [g]: the loop parameters, its free
                   variables, the mailbox and the limit *)
                let gparams = List.map header.params ~f:Var.fork in
                let fparams = List.map free ~f:Var.fork in
                let mbox = Var.fork mailbox in
                let limit = Var.fresh_n "limit" in
                let store vars =
                  List.mapi vars ~f:(fun i x -> Set_field (mbox, i, Non_float, x))
                in
                let answer n body =
                  let c = Var.fresh () in
                  body @ [ Let (c, int n) ], Return c
                in
                (* In [g]: the loop header with its counter *)
                let counter = Var.fresh_n "counter" in
                let hc_params = List.map header.params ~f:Var.fork in
                let test = Var.fresh () in
                let reenter =
                  let params = List.map header.params ~f:Var.fork in
                  let body, branch = answer 0 (store params) in
                  new_block { params; body; branch }
                in
                let hc =
                  new_block
                    { params = hc_params @ [ counter ]
                    ; body =
                        [ Let (test, Prim (Lt, [ Pc (Int Targetint.zero); Pv counter ])) ]
                    ; branch = Cond (test, (h, hc_params), (reenter, hc_params))
                    }
                in
                let entry =
                  new_block
                    { params = []; body = []; branch = Branch (hc, gparams @ [ limit ]) }
                in
                (* The edges out of the loop, and its returns *)
                let exit_blocks =
                  List.mapi exits ~f:(fun j pc' ->
                      let params = List.map (get pc').params ~f:Var.fork in
                      let body, branch = answer (j + 1) (store (params @ live_out)) in
                      pc', new_block { params; body; branch })
                in
                (* The free variables read from the parameters *)
                let fsubst =
                  Subst.from_map
                    (List.fold_left2 free fparams ~init:Var.Map.empty ~f:(fun m x x' ->
                         Var.Map.add x x' m))
                in
                Addr.Set.iter
                  (fun pc ->
                    let b = get pc in
                    let b =
                      if Addr.Set.mem pc body
                      then
                        map_conts
                          (fun ((pc', args) as c) ->
                            if Addr.Set.mem pc' body
                            then if pc' = h then hc, args @ [ counter ] else c
                            else
                              ( snd
                                  (List.find exit_blocks ~f:(fun (pc'', _) -> pc'' = pc'))
                              , args ))
                          b
                      else b
                    in
                    (* The back edges decrement the counter *)
                    let b =
                      if Addr.Set.mem pc back
                      then
                        let c' = Var.fork counter in
                        { (map_conts
                             (fun ((pc', args) as c) ->
                               if pc' = hc
                               then
                                 ( pc'
                                 , List.filteri args ~f:(fun i _ ->
                                       i < List.length args - 1)
                                   @ [ c' ] )
                               else c)
                             b)
                          with
                          body =
                            b.body
                            @ [ Let
                                  ( c'
                                  , Prim
                                      ( Extern ("%int_sub", None)
                                      , [ Pv counter; Pc (Int Targetint.one) ] ) )
                              ]
                        }
                      else b
                    in
                    let b =
                      match b.branch with
                      | Return x when Addr.Set.mem pc body ->
                          let body, branch =
                            answer (n_exits + 1) (b.body @ store [ x ])
                          in
                          { b with body; branch }
                      | _ -> b
                    in
                    blocks :=
                      Addr.Map.add pc (Subst.Excluding_Binders.block fsubst b) !blocks)
                  gset;
                (* The driver, in place of the loop *)
                let dparams = List.map header.params ~f:Var.fork in
                let dlimit = Var.fork limit in
                let gvar = Var.fresh_n "loop" in
                let r = Var.fresh () in
                let fields n = List.init ~len:n ~f:(fun _ -> Var.fresh ()) in
                let get_fields vars =
                  List.mapi vars ~f:(fun i x -> Let (x, Field (mailbox, i, Non_float)))
                in
                let driver = !free_pc in
                incr free_pc;
                let again =
                  let vars = fields (List.length gparams) in
                  let l2 = Var.fresh () in
                  let small = Var.fresh () in
                  let max = Var.fresh () in
                  let doubled = Var.fresh () in
                  let to_driver l = Branch (driver, vars @ [ l ]) in
                  let bmax =
                    new_block
                      { params = []
                      ; body = [ Let (max, int max_limit) ]
                      ; branch = to_driver max
                      }
                  in
                  let bdouble =
                    new_block { params = []; body = []; branch = to_driver doubled }
                  in
                  new_block
                    { params = []
                    ; body =
                        get_fields vars
                        @ [ Let
                              ( doubled
                              , Prim (Extern ("%int_add", None), [ Pv dlimit; Pv dlimit ])
                              )
                          ; Let (l2, int max_limit)
                          ; Let (small, Prim (Lt, [ Pv doubled; Pv l2 ]))
                          ]
                    ; branch = Cond (small, (bdouble, []), (bmax, []))
                    }
                in
                let loop_subst = ref Var.Map.empty in
                let leave =
                  List.map exits ~f:(fun pc' ->
                      let vars = fields (List.length (get pc').params) in
                      let outs = List.map live_out ~f:Var.fork in
                      List.iter2 live_out outs ~f:(fun x x' ->
                          loop_subst := Var.Map.add x x' !loop_subst);
                      new_block
                        { params = []
                        ; body = get_fields (vars @ outs)
                        ; branch = Branch (pc', vars)
                        })
                in
                let ret =
                  if returns
                  then
                    let x = Var.fresh () in
                    [ new_block
                        { params = []; body = get_fields [ x ]; branch = Return x }
                    ]
                  else []
                in
                blocks :=
                  Addr.Map.add
                    driver
                    { params = dparams @ [ dlimit ]
                    ; body =
                        [ Let
                            ( gvar
                            , Closure
                                ( gparams @ fparams @ [ mbox; limit ]
                                , (entry, [])
                                , (None, None) ) )
                        ; Let
                            ( r
                            , Apply
                                { f = gvar
                                ; args = dparams @ free @ [ mailbox; dlimit ]
                                ; exact = true
                                } )
                        ]
                    ; branch =
                        Switch
                          ( r
                          , Array.of_list
                              (List.map ((again :: leave) @ ret) ~f:(fun pc -> pc, [])) )
                    }
                    !blocks;
                let g_blocks = [ hc; entry; reenter ] @ List.map exit_blocks ~f:snd in
                in_g := Addr.Set.union !in_g (Addr.Set.of_list g_blocks);
                (* The variables of the loop used after it, read from the
                   mailbox: renamed out of [g] *)
                (if not (Var.Map.is_empty !loop_subst)
                 then
                   let s = Subst.from_map !loop_subst in
                   blocks :=
                     Addr.Map.mapi
                       (fun pc b ->
                         if Addr.Set.mem pc gset || List.mem ~eq:Int.equal pc g_blocks
                         then b
                         else Subst.Excluding_Binders.block s b)
                       !blocks);
                (* The edges to the loop go to the driver: from the blocks
                   of the function, including those added for the loops
                   before (which may leave to this one) *)
                Addr.Set.iter
                  (fun pc ->
                    if not (Addr.Set.mem pc body || Addr.Set.mem pc !in_g)
                    then
                      let b = get pc in
                      if List.exists (successors b) ~f:(fun (pc', _) -> pc' = h)
                      then
                        let first = Var.fresh () in
                        blocks :=
                          Addr.Map.add
                            pc
                            { (map_conts
                                 (fun ((pc', args) as c) ->
                                   if pc' = h then driver, args @ [ first ] else c)
                                 b)
                              with
                              body = b.body @ [ Let (first, int first_limit) ]
                            }
                            !blocks)
                  (Addr.Map.fold
                     (fun pc _ acc -> Addr.Set.add pc acc)
                     !blocks
                     Addr.Set.empty)))
  in
  process p.start;
  List.iter called_once ~f:process;
  if debug () then Format.eprintf "loop-reentry: %d loops@." !count;
  let blocks = !blocks in
  let p = { p with blocks; free_pc = !free_pc } in
  if print () then Code.Print.program Format.err_formatter (fun _ _ -> "") p;
  p
