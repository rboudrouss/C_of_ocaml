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

(* We compute which functions and which call points needs to be in CPS. *)

open! Stdlib

let times = Debug.find "times"

let double_translate () =
  match Config.effects () with
  | `Disabled | `Jspi | `Native -> assert false
  | `Cps -> false
  | `Double_translation -> true

(* We turn mutually recursive functions into CPS so that calls between
   them are tail calls. This is not needed when targeting Wasm, which
   has proper tail calls. *)
let cps_for_tail_calls () =
  match Config.target () with
  | `JavaScript -> true
  | `Wasm | `C -> false

open Code

let add_var = Var.ISet.add

(* x depends on y *)
let add_dep deps x y =
  let idx = Var.idx y in
  deps.(idx) <- Var.Set.add x deps.(idx)

let add_tail_dep deps x y =
  if not (Var.Map.mem x !deps) then deps := Var.Map.add x Var.Set.empty !deps;
  deps :=
    Var.Map.update
      y
      (fun s -> Some (Var.Set.add x (Option.value ~default:Var.Set.empty s)))
      !deps

let rec block_iter_last ~f l =
  match l with
  | [] -> ()
  | [ i ] -> f true i
  | [ i; Event _ ] -> f true i
  | i :: l ->
      f false i;
      block_iter_last ~f l

let block_deps ~info ~vars ~tail_deps ~deps ~blocks ~fun_name pc =
  let block = Addr.Map.find pc blocks in
  block_iter_last block.body ~f:(fun is_last i ->
      match i with
      | Let (x, Apply { f; _ }) -> (
          add_var vars x;
          (match fun_name with
          | None -> ()
          | Some g ->
              add_var vars g;
              (* If a call point is in CPS, then the englobing
                 function should be in CPS *)
              add_dep deps g x);
          match Var.Tbl.get info.Global_flow.info_approximation f with
          | Top -> ()
          | Values { known; others } ->
              let known_tail_call =
                cps_for_tail_calls ()
                && (not others)
                && is_last
                &&
                match block.branch with
                | Return x' -> Var.equal x x'
                | _ -> false
              in
              Var.Set.iter
                (fun g ->
                  add_var vars g;
                  (if known_tail_call
                   then
                     match fun_name with
                     | None -> ()
                     | Some f -> add_tail_dep tail_deps f g);
                  (* If a called function is in CPS, then the call
                     point is in CPS *)
                  add_dep deps x g;
                  (* Conversally, if a call point is in CPS then all
                     called functions must be in CPS *)
                  if not (double_translate ()) then add_dep deps g x)
                known)
      | Let
          ( x
          , Prim
              ( Extern
                  ( ( "%perform"
                    | "%reperform"
                    | "%resume"
                    | "%with_stack"
                    | "%with_stack_bind" )
                  , _ )
              , _ ) ) -> (
          add_var vars x;
          match fun_name with
          | None -> ()
          | Some f ->
              add_var vars f;
              (* If a function contains effect primitives, it must be
                 in CPS *)
              add_dep deps f x)
      | Let (x, Closure _) -> add_var vars x
      | Let (_, (Prim _ | Block _ | Constant _ | Field _ | Special _))
      | Event _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ -> ())

let program_deps ~info ~vars ~tail_deps ~deps p =
  fold_closures
    p
    (fun fun_name _ (pc, _) _ () ->
      traverse
        { fold = Code.fold_children }
        (fun pc () ->
          block_deps ~info ~vars ~tail_deps ~deps ~blocks:p.blocks ~fun_name pc)
        pc
        p.blocks
        ())
    ()

module Domain = struct
  type t = bool

  let equal = Bool.equal

  let bot = false
end

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)
module Solver = G.Solver (Domain)

let fold_children g f x acc =
  let acc = ref acc in
  g.G.iter_children (fun y -> acc := f y !acc) x;
  !acc

(* Whether [x] may run below an effect handler, that is, whether its
   stack frame may be captured by a continuation. A function may run
   below an effect handler if it escapes (fiber bodies and effect
   handlers are passed to the [%with_stack] and [%resume] primitives,
   so they escape), or if it is called from a call point that may
   itself run below an effect handler, that is, a call point in a
   function that may. Toplevel code never runs below an effect handler.

   A function that never runs below an effect handler does not need a
   CPS version. This is only used with double translation, where a
   direct-style version of every function is available, so that a CPS
   call point can always fall back to calling the direct-style version
   of a callee that has no CPS version.

   The dependencies are the same as for [cps_needed], but the
   information flows in the opposite direction: from callers to
   callees. So this is solved on the reversed graph, reading [deps]. *)
let below_handler ~info ~deps st x =
  let from_callers () =
    fold_children deps (fun y acc -> acc || Var.Tbl.get st y) x false
  in
  match info.Global_flow.info_defs.(Var.idx x) with
  | Expr (Closure _) -> Var.ISet.mem info.Global_flow.info_may_escape x || from_callers ()
  | Expr (Apply _ | Prim _ | Block _ | Constant _ | Field _ | Special _) | Phi _ ->
      from_callers ()

let cps_needed ~info ~in_mutual_recursion ~rev_deps ~below st x =
  (match below with
    | None -> true
    | Some below -> Var.Tbl.get below x)
  &&
  (* Mutually recursive functions are turned into CPS for tail
     optimization (JavaScript only, see [cps_for_tail_calls]) *)
  (Var.Set.mem x in_mutual_recursion
  ||
  let idx = Var.idx x in
  fold_children rev_deps (fun y acc -> acc || Var.Tbl.get st y) x false
  ||
  match info.Global_flow.info_defs.(idx) with
  | Expr (Apply { f; args; exact; _ }) -> (
      (* If we don't know all possible functions at a call point, it
         must be in CPS *)
      match Var.Tbl.get info.Global_flow.info_approximation f with
      | Top -> true
      | Values { known; others } ->
          others
          || (not exact)
             &&
             (* Likewise if a function may be applied to too many
                   arguments: the closure it returns is then applied
                   to the remaining arguments *)
             let nargs = List.length args in
             Var.Set.exists
               (fun g ->
                 match info.Global_flow.info_defs.(Var.idx g) with
                 | Expr (Closure (params, _, _)) -> List.length params < nargs
                 | _ -> true)
               known)
  | Expr (Closure _) ->
      (not (double_translate ()))
      &&
      (* If a function escapes, it must be in CPS *)
      Var.ISet.mem info.Global_flow.info_may_escape x
  | Expr
      (Prim
         ( Extern
             ( ("%perform" | "%reperform" | "%resume" | "%with_stack" | "%with_stack_bind")
             , _ )
         , _ )) ->
      (* Effects primitives are in CPS *)
      true
  | Expr (Prim _ | Block _ | Constant _ | Field _ | Special _) | Phi _ -> false)

module SCC = Strongly_connected_components.Make (Var)

let find_mutually_recursive_calls tail_deps =
  let scc = SCC.component_graph !tail_deps in
  Array.fold_left
    ~f:(fun s (c, _) ->
      match c with
      | SCC.No_loop _ -> s
      | Has_loop l -> List.fold_left ~f:(fun s x -> Var.Set.add x s) l ~init:s)
    ~init:Var.Set.empty
    scc

(* With double translation and [cps_recursions] (c_of_ocaml --cps-stack),
   the functions which may recurse, and their calls which may be
   recursive, are in CPS too, so that a deep recursion can go on in CPS,
   bouncing on a trampoline rather than growing the stack (see
   [camlc_cps_stack]): the functions in a cycle of known calls or making
   unknown calls, and their calls to unknown functions or to such
   functions. Their callers are not: the switch to CPS happens in the
   recursion. *)
let recursive_functions p info =
  let calls = ref Var.Map.empty in
  fold_closures
    p
    (fun fun_name _ (pc, _) _ () ->
      match fun_name with
      | None -> ()
      | Some g ->
          traverse
            { fold = Code.fold_children }
            (fun pc () ->
              List.iter (Addr.Map.find pc p.blocks).body ~f:(fun i ->
                  match i with
                  | Let (x, Apply { f; _ }) ->
                      let callees =
                        match Var.Tbl.get info.Global_flow.info_approximation f with
                        | Top -> None
                        | Values { known; others } -> if others then None else Some known
                      in
                      calls :=
                        Var.Map.update
                          g
                          (fun l -> Some ((x, callees) :: Option.value ~default:[] l))
                          !calls
                  | _ -> ()))
            pc
            p.blocks
            ())
    ();
  let graph =
    Var.Map.fold
      (fun g l graph ->
        let callees =
          List.fold_left l ~init:Var.Set.empty ~f:(fun s (_, callees) ->
              match callees with
              | Some known -> Var.Set.union s known
              | None -> s)
        in
        let graph =
          Var.Set.fold
            (fun h graph ->
              if Var.Map.mem h graph then graph else Var.Map.add h Var.Set.empty graph)
            callees
            graph
        in
        Var.Map.add g callees graph)
      !calls
      Var.Map.empty
  in
  let recursive =
    Array.fold_left (SCC.component_graph graph) ~init:Var.Set.empty ~f:(fun s (c, _) ->
        match c with
        | SCC.No_loop _ -> s
        | Has_loop l -> List.fold_left l ~init:s ~f:(fun s x -> Var.Set.add x s))
  in
  let recursive =
    Var.Map.fold
      (fun g l s ->
        if List.exists l ~f:(fun (_, callees) -> Option.is_none callees)
        then Var.Set.add g s
        else s)
      !calls
      recursive
  in
  Var.Map.fold
    (fun g l s ->
      if Var.Set.mem g recursive
      then
        List.fold_left l ~init:s ~f:(fun s (x, callees) ->
            match callees with
            | None -> Var.Set.add x s
            | Some known ->
                if Var.Set.exists (fun h -> Var.Set.mem h recursive) known
                then Var.Set.add x s
                else s)
      else s)
    !calls
    recursive

let annot st xi =
  match (xi : Print.xinstr) with
  | Instr (Let (x, _)) when Var.Set.mem x st -> "*"
  | _ -> " "

let f p info =
  let t = Timer.make () in
  let t1 = Timer.make () in
  let nv = Var.count () in
  let vars = Var.ISet.empty () in
  let deps = Array.make nv Var.Set.empty in
  let tail_deps = ref Var.Map.empty in
  program_deps ~info ~vars ~tail_deps ~deps p;
  if times () then Format.eprintf "      fun analysis (initialize): %a@." Timer.print t1;
  let t2 = Timer.make () in
  let in_mutual_recursion =
    if cps_for_tail_calls ()
    then find_mutually_recursive_calls tail_deps
    else Var.Set.empty
  in
  if times () then Format.eprintf "      fun analysis (tail calls): %a@." Timer.print t2;
  let t3 = Timer.make () in
  let g =
    { G.domain = vars; iter_children = (fun f x -> Var.Set.iter f deps.(Var.idx x)) }
  in
  let rev_deps = G.invert () g in
  let below =
    if double_translate ()
    then Some (Solver.f () rev_deps (below_handler ~info ~deps:g))
    else None
  in
  let res = Solver.f () g (cps_needed ~info ~in_mutual_recursion ~rev_deps ~below) in
  if times () then Format.eprintf "      fun analysis (solve): %a@." Timer.print t3;
  let s = ref Var.Set.empty in
  Var.Tbl.iter (fun x v -> if v then s := Var.Set.add x !s) res;
  if double_translate () && Config.Flag.cps_recursions ()
  then s := Var.Set.union !s (recursive_functions p info);
  if times () then Format.eprintf "    fun analysis: %a@." Timer.print t;
  !s
