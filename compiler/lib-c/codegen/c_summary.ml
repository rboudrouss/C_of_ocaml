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
open! C_repr
open! C_output
open! C_primitives
open! C_translate
open! C_frame

(* The functions which may raise an exception, that is return an
   exception result: the calls of the other ones are not followed by a
   check. A function may raise if, outside any handler, it raises, calls a
   primitive which may raise, an unknown function or a function which may
   raise. CPS versions and CPS calls are assumed to raise. An overflow of
   the shadow stack in a function which cannot raise is fatal.

   Similarly, the functions which may trigger a garbage collection: the
   calls of the other ones are not GC points. A call of a function which
   may trigger one only when it raises, outside any handler, only roots the
   variables live in the handler of the caller. *)
type summary =
  { fn : Var.t
  ; raises : bool  (** Raises by itself *)
  ; outer : Var.t list  (** The functions called outside any handler *)
  ; gcs : bool  (** May trigger a GC by itself and return *)
  ; any_gcs : bool  (** May trigger a GC by itself *)
  ; inner : Var.t list  (** The functions called inside a handler *)
  ; tail : Var.t list  (** The known functions called in tail position *)
  ; unknown_tail : bool  (** Makes an unknown tail call *)
  }

let non_tail_calls blocks pc =
  let block = Addr.Map.find pc blocks in
  let rec calls l =
    match l with
    | [] -> []
    | Let (x, Apply { f; exact; _ }) :: r ->
        let tail =
          List.for_all r ~f:(function
            | Event _ -> true
            | _ -> false)
          &&
          match block.branch with
          | Return y -> Var.equal x y
          | _ -> false
        in
        if tail then calls r else (x, f, exact) :: calls r
    | _ :: r -> calls r
  in
  calls block.body

(* The largest number of calls which may be recursive on a path through
   the function starting at [pc] (back edges ignored) *)
let recursive_calls out global_flow_info closures blocks ~fn pc =
  let memo = Addr.Hashtbl.create 16 in
  let rec count pc =
    match Addr.Hashtbl.find_opt memo pc with
    | Some n -> n
    | None ->
        Addr.Hashtbl.replace memo pc 0;
        let block = Addr.Map.find pc blocks in
        let here =
          List.length
            (List.filter
               (non_tail_calls blocks pc)
               ~f:(recursive_call out global_flow_info closures ~fn))
        in
        let next =
          match block.branch with
          | Return _ | Raise _ | Stop -> 0
          | Branch (pc', _) | Poptrap (pc', _) -> count pc'
          | Cond (_, (pc1, _), (pc2, _)) | Pushtrap ((pc1, _), _, (pc2, _)) ->
              max (count pc1) (count pc2)
          | Switch (_, conts) ->
              Array.fold_left conts ~init:0 ~f:(fun n (pc', _) -> max n (count pc'))
        in
        let n = here + next in
        Addr.Hashtbl.replace memo pc n;
        n
  in
  count pc

(* A context for the analyses of the code, out of any function *)
let analysis_ctx out facts global_flow_info closures blocks =
  { out
  ; facts
  ; global_flow_info
  ; blocks
  ; closures
  ; env_var = Var.fresh ()
  ; fn = None
  ; arity = None
  ; static_env = true
  ; locations = Var.Hashtbl.create 1
  ; slots = Var.Hashtbl.create 1
  ; traps = Addr.Hashtbl.create 1
  ; handler_ids = Addr.Hashtbl.create 1
  ; local_names = Var.Hashtbl.create 1
  ; labels = Addr.Hashtbl.create 1
  ; buf = Buffer.create 1
  ; position = None
  ; source_name = None
  ; raises = true
  ; ret = Value
  ; framed = false
  ; framed_blocks = None
  ; frame_push = ""
  }

let analyze_functions out facts global_flow_info closures blocks functions =
  let ctx = analysis_ctx out facts global_flow_info closures blocks in
  let typed a =
    match a with
    | Pv x ->
        { a_typ = Typing.var_type out.types x; a_const = None; get = (fun _ -> "_") }
    | Pc c -> { a_typ = Typing.constant_type c; a_const = Some c; get = (fun _ -> "_") }
  in
  (* Whether a function raises by itself, the functions it calls outside
     any handler, whether it triggers a GC by itself (and then returns, or
     in any case), and the functions it calls inside handlers *)
  let summary pc =
    let visited = Addr.Hashtbl.create 16 in
    let raises = ref false in
    let outer = ref [] in
    let gcs = ref false in
    let any_gcs = ref false in
    let inner = ref [] in
    let tail = ref [] in
    let unknown_tail = ref false in
    let gc_point depth i =
      match gc_point ctx i with
      | `No -> ()
      | `Raise -> if depth = 0 then any_gcs := true else gcs := true
      | `Call | `Alloc -> gcs := true
    in
    let rec visit pc depth =
      if not (Addr.Hashtbl.mem visited pc)
      then (
        Addr.Hashtbl.add visited pc ();
        let block = Addr.Map.find pc blocks in
        (* A tail call (see [translate_function]) *)
        (if depth = 0
         then
           match
             ( List.find_opt (List.rev block.body) ~f:(fun i ->
                   match i with
                   | Event _ -> false
                   | _ -> true)
             , block.branch )
           with
           | Some (Let (x, Apply { f; exact; _ })), Return y when Var.equal x y -> (
               match resolve_callee global_flow_info closures x ~f ~exact with
               | Known g -> tail := g :: !tail
               | Known_direct _ | Unknown -> unknown_tail := true)
           | _ -> ());
        List.iter block.body ~f:(fun i ->
            match i with
            | Let (x, Apply { f; args; exact }) -> (
                if double_translation () && Var.Set.mem x out.in_cps
                then (
                  raises := true;
                  gcs := true)
                else
                  match resolve_callee global_flow_info closures x ~f ~exact with
                  | Known g ->
                      if depth = 0 then outer := g :: !outer else inner := g :: !inner;
                      if List.exists (boxed_arguments ctx x ~f ~args ~exact) ~f:Fun.id
                      then gcs := true
                  | Known_direct _ | Unknown ->
                      if depth = 0 then raises := true;
                      gcs := true)
            | Let (_, Prim (Extern ("caml_cps_closure", _), _)) -> ()
            | Let (_, Prim (Extern (name, _), args)) ->
                (match primitive out name (List.map args ~f:typed) with
                | Plain _ -> ()
                | Checked _ | Guarded _ | Ccallv -> if depth = 0 then raises := true);
                gc_point depth i
            | Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ ->
                gc_point depth i);
        match block.branch with
        | Raise _ -> if depth = 0 then raises := true
        | Return _ | Stop -> ()
        | Branch (pc', _) -> visit pc' depth
        | Poptrap (pc', _) -> visit pc' (depth - 1)
        | Cond (_, (pc1, _), (pc2, _)) ->
            visit pc1 depth;
            visit pc2 depth
        | Switch (_, conts) -> Array.iter conts ~f:(fun (pc', _) -> visit pc' depth)
        | Pushtrap ((pc1, _), _, (pc2, _)) ->
            visit pc1 (depth + 1);
            visit pc2 depth)
    in
    visit pc 0;
    (* A poll may raise and collect *)
    let entry_poll, loop_polls = polls out blocks pc in
    if entry_poll || not (Addr.Set.is_empty loop_polls)
    then (
      raises := true;
      gcs := true);
    !raises, !outer, !gcs, !gcs || !any_gcs, !inner, !tail, !unknown_tail
  in
  (* The functions with the property, and the callers of each function *)
  let propagate roots callers =
    let set = Var.Hashtbl.create 1024 in
    let rec mark f =
      if not (Var.Hashtbl.mem set f)
      then (
        Var.Hashtbl.add set f ();
        List.iter (Var.Hashtbl.find_all callers f) ~f:mark)
    in
    List.iter roots ~f:mark;
    Var.Hashtbl.fold (fun f () acc -> Var.Set.add f acc) set Var.Set.empty
  in
  let summaries =
    List.map functions ~f:(fun (f, _, (pc, _)) ->
        let raises, outer, gcs, any_gcs, inner, tail, unknown_tail = summary pc in
        let side = Var.Set.mem f out.cps_sides in
        { fn = f
        ; raises = raises || side
        ; outer
        ; gcs = gcs || side
        ; any_gcs = any_gcs || side
        ; inner
        ; tail
        ; unknown_tail
        })
  in
  let callers l =
    let h = Var.Hashtbl.create 1024 in
    List.iter summaries ~f:(fun s ->
        List.iter (l s) ~f:(fun g -> Var.Hashtbl.add h g s.fn));
    h
  in
  let roots p = List.filter_map summaries ~f:(fun s -> if p s then Some s.fn else None) in
  out.raising <- propagate (roots (fun s -> s.raises)) (callers (fun s -> s.outer));
  out.unknown_tail_calls <-
    propagate (roots (fun s -> s.unknown_tail)) (callers (fun s -> s.tail));
  (* The functions which may trigger a GC in any way, then those which may
     return after one: a call inside a handler is in the second case if
     the function called may trigger a GC in any way *)
  let set_collecting fallback =
    let any =
      propagate
        (roots (fun s -> s.any_gcs || Var.Set.mem s.fn fallback))
        (callers (fun s -> s.outer @ s.inner))
    in
    out.collecting <-
      propagate
        (roots (fun s ->
             s.gcs
             || Var.Set.mem s.fn fallback
             || List.exists s.inner ~f:(fun g -> Var.Set.mem g any)))
        (callers (fun s -> s.outer));
    out.collecting_on_raise <- Var.Set.diff any out.collecting
  in
  set_collecting Var.Set.empty;
  (* The strongly connected components of the graph of known calls
     (Tarjan), those with a cycle *)
  let callees = Var.Hashtbl.create 1024 in
  List.iter summaries ~f:(fun s -> Var.Hashtbl.replace callees s.fn (s.outer @ s.inner));
  let index = Var.Hashtbl.create 1024 in
  let low = Var.Hashtbl.create 1024 in
  let on_stack = Var.Hashtbl.create 1024 in
  let stack = ref [] in
  let next = ref 0 in
  let components = Var.Hashtbl.create 1024 in
  let rec visit f =
    Var.Hashtbl.replace index f !next;
    Var.Hashtbl.replace low f !next;
    incr next;
    stack := f :: !stack;
    Var.Hashtbl.replace on_stack f ();
    List.iter
      (Option.value ~default:[] (Var.Hashtbl.find_opt callees f))
      ~f:(fun g ->
        if not (Var.Hashtbl.mem index g)
        then (
          visit g;
          Var.Hashtbl.replace
            low
            f
            (min (Var.Hashtbl.find low f) (Var.Hashtbl.find low g)))
        else if Var.Hashtbl.mem on_stack g
        then
          Var.Hashtbl.replace
            low
            f
            (min (Var.Hashtbl.find low f) (Var.Hashtbl.find index g)));
    if Var.Hashtbl.find low f = Var.Hashtbl.find index f
    then
      let rec pop acc =
        match !stack with
        | g :: r ->
            stack := r;
            Var.Hashtbl.remove on_stack g;
            if Var.equal g f then g :: acc else pop (g :: acc)
        | [] -> assert false
      in
      let component = pop [] in
      let cyclic =
        match component with
        | [ g ] ->
            List.exists
              (Option.value ~default:[] (Var.Hashtbl.find_opt callees g))
              ~f:(Var.equal g)
        | _ -> true
      in
      if cyclic
      then
        List.iter component ~f:(fun g ->
            Var.Hashtbl.replace components g (Var.Hashtbl.find index f))
  in
  List.iter summaries ~f:(fun s -> if not (Var.Hashtbl.mem index s.fn) then visit s.fn);
  out.components <- components;
  (* With --cps-stack, the recursive functions with a CPS version may go on
     in it, which allocates (see [camlc_cps_stack]): the linear recursions
     (one call which may be recursive on each path), and the others which
     may trigger a GC anyway, which then have a frame measuring their depth
     (that of a recursion without frame, such as [fib], is not measured) *)
  if Config.Flag.cps_recursions ()
  then (
    out.cps_fallback <-
      List.fold_left functions ~init:Var.Set.empty ~f:(fun acc (f, _, (pc, _)) ->
          if Var.Map.mem f out.cps_pairs
          then
            match
              recursive_calls out global_flow_info closures blocks ~fn:(Some f) pc
            with
            | 0 -> acc
            | 1 -> Var.Set.add f acc
            | _ -> if Var.Set.mem f out.collecting then Var.Set.add f acc else acc
          else acc);
    set_collecting out.cps_fallback)
