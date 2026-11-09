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
open! C_output
open! C_translate
open! C_frame
open! C_summary
open! C_naming

(* The toplevel code of a large program is split into functions of about
   [toplevel_chunk_size] instructions (option --toplevel-chunk-size): the
   time C compilers take to optimize a function grows faster than its size. Since the values which
   live long are in globals (see [Globalize]), there are many points where
   no local variable is live. A chunk runs from such a point, a block [c]
   outside any handler, to the next one: the code reached from [c] must be
   dominated by [c] (not in a loop entered before it), and so must be the
   ends of the program, so that each chunk, unless it raises, ends by
   jumping to the start of the next one. The toplevel function calls the
   chunks in turn. The chunks start at points chosen by the hash of their
   name, with a probability proportional to the size of the code since the
   previous candidate, once a chunk has half the size: an edit leaves the
   other chunks unchanged. *)
let toplevel_chunks
    out
    facts
    global_flow_info
    closures
    blocks
    ~toplevel_chunk_size
    ~free_pc
    pc =
  let ctx = analysis_ctx out facts global_flow_info closures blocks in
  let ctx = { ctx with traps = Addr.Hashtbl.create 1024 } in
  let pcs = function_blocks ctx pc in
  let size =
    List.fold_left pcs ~init:0 ~f:(fun n pc ->
        n + List.length (Addr.Map.find pc blocks).body)
  in
  if toplevel_chunk_size = 0 || size < 2 * toplevel_chunk_size
  then []
  else
    let add_local x =
      if not (Var.Hashtbl.mem ctx.locations x || Var.Hashtbl.mem out.globals x)
      then Var.Hashtbl.add ctx.locations x Local
    in
    List.iter pcs ~f:(fun pc ->
        let block = Addr.Map.find pc blocks in
        List.iter block.params ~f:add_local;
        List.iter block.body ~f:(fun i ->
            match instr_defs i with
            | Some x -> add_local x
            | None -> ());
        match block.branch with
        | Pushtrap (_, x, _) -> add_local x
        | Return _ | Raise _ | Stop | Branch _ | Cond _ | Switch _ | Poptrap _ -> ());
    let _, live_in = rooted_variables ctx pcs ~params:[] in
    (* The blocks are split where no local variable is live: at the first
       such point of a block which starts with some (or with parameters),
       and every [toplevel_chunk_size / 16] instructions at least *)
    let free_pc = ref free_pc in
    let spacing = toplevel_chunk_size / 16 in
    let blocks =
      List.fold_left pcs ~init:blocks ~f:(fun blocks pc ->
          let block = Addr.Map.find pc blocks in
          let body = Array.of_list block.body in
          let n = Array.length body in
          let clean_entry =
            List.is_empty block.params && Var.Set.is_empty (Addr.Hashtbl.find live_in pc)
          in
          if n < 2 || (clean_entry && n < 2 * spacing)
          then blocks
          else
            let live = ref (live_out ctx live_in pc) in
            let empty = Array.make n false in
            for k = n - 1 downto 0 do
              empty.(k) <- Var.Set.is_empty !live;
              let i = body.(k) in
              live :=
                instr_uses
                  ctx
                  i
                  (match instr_defs i with
                  | Some x -> Var.Set.remove x !live
                  | None -> !live)
            done;
            let first =
              if clean_entry
              then 0
              else
                let rec find k = if k >= n || empty.(k - 1) then k else find (k + 1) in
                find 1
            in
            (* The pieces, from the last one *)
            let traps = Addr.Hashtbl.find ctx.traps pc in
            let rec split blocks k last branch =
              (* [k]: the start of the piece; [last]: its end *)
              if k = 0
              then
                Addr.Map.add
                  pc
                  { block with
                    body = Array.to_list (Array.sub body ~pos:0 ~len:last)
                  ; branch
                  }
                  blocks
              else if
                empty.(k - 1)
                && (k = first || (last - k >= spacing && k >= first + spacing))
              then (
                let pc' = !free_pc in
                incr free_pc;
                Addr.Hashtbl.replace ctx.traps pc' traps;
                Addr.Hashtbl.replace live_in pc' Var.Set.empty;
                let blocks =
                  Addr.Map.add
                    pc'
                    { params = []
                    ; body = Array.to_list (Array.sub body ~pos:k ~len:(last - k))
                    ; branch
                    }
                    blocks
                in
                split blocks (k - 1) k (Branch (pc', [])))
              else split blocks (k - 1) last branch
            in
            split blocks (n - 1) n block.branch)
    in
    let free_pc = !free_pc in
    let g = Structure.control_flow_graph blocks pc in
    let idom = Structure.immediate_dominators g in
    let rpo = Structure.blocks_in_reverse_post_order g in
    let depth = Addr.Hashtbl.create 1024 in
    List.iter rpo ~f:(fun pc' ->
        Addr.Hashtbl.replace
          depth
          pc'
          (match Addr.Hashtbl.find_opt idom pc' with
          | Some d -> Addr.Hashtbl.find depth d + 1
          | None -> 0));
    let rec common_dominator a b =
      if a = b
      then a
      else if Addr.Hashtbl.find depth a > Addr.Hashtbl.find depth b
      then common_dominator (Addr.Hashtbl.find idom a) b
      else common_dominator a (Addr.Hashtbl.find idom b)
    in
    let ends =
      List.filter rpo ~f:(fun pc ->
          match (Addr.Map.find pc blocks).branch with
          | Return _ | Stop -> true
          | Raise _ | Branch _ | Cond _ | Switch _ | Pushtrap _ | Poptrap _ -> false)
    in
    match ends with
    | [] -> []
    | e :: r ->
        (* The blocks dominating the ends of the program, in order *)
        let last = List.fold_left r ~init:e ~f:common_dominator in
        let rec dominators pc acc =
          match Addr.Hashtbl.find_opt idom pc with
          | Some d -> dominators d (pc :: acc)
          | None -> pc :: acc
        in
        let chain = Array.of_list (dominators last []) in
        let m = Array.length chain in
        let index = Addr.Hashtbl.create 1024 in
        Array.iteri chain ~f:(fun i pc -> Addr.Hashtbl.add index pc i);
        (* The last block of the chain dominating each block *)
        let level = Addr.Hashtbl.create 1024 in
        List.iter rpo ~f:(fun pc' ->
            Addr.Hashtbl.replace
              level
              pc'
              (match Addr.Hashtbl.find_opt index pc' with
              | Some i -> i
              | None -> Addr.Hashtbl.find level (Addr.Hashtbl.find idom pc')));
        (* A block [chain.(i)] cannot start a chunk if some code it dominates
           jumps to code it does not dominate *)
        let excluded = Array.make (m + 1) 0 in
        let sizes = Array.make m 0 in
        List.iter rpo ~f:(fun pc' ->
            let l = Addr.Hashtbl.find level pc' in
            sizes.(l) <- sizes.(l) + List.length (Addr.Map.find pc' blocks).body;
            Code.fold_children
              blocks
              pc'
              (fun pc'' () ->
                let l' = Addr.Hashtbl.find level pc'' in
                if l' < l
                then (
                  excluded.(l' + 1) <- excluded.(l' + 1) + 1;
                  excluded.(l + 1) <- excluded.(l + 1) - 1))
              ());
        let candidate =
          let n = ref 0 in
          Array.init m ~f:(fun i ->
              n := !n + excluded.(i);
              let pc = chain.(i) in
              let block = Addr.Map.find pc blocks in
              i > 0
              && !n = 0
              && List.is_empty block.params
              && List.is_empty (Addr.Hashtbl.find ctx.traps pc)
              && Var.Set.is_empty (Addr.Hashtbl.find live_in pc))
        in
        (* The chunks are named after the first debug event and the first
           global they define *)
        let next_global = Addr.Hashtbl.create 1024 in
        let next = ref "" in
        List.iter (List.rev rpo) ~f:(fun pc' ->
            List.iter (List.rev (Addr.Map.find pc' blocks).body) ~f:(fun i ->
                match instr_defs i with
                | Some x -> (
                    match Var.Hashtbl.find_opt out.globals x with
                    | Some g -> next := g
                    | None -> ())
                | None -> ());
            Addr.Hashtbl.replace next_global pc' !next);
        let ranks = String.Hashtbl.create 64 in
        let name pc =
          let key =
            (match first_event blocks (pc, []) with
              | Some pi -> Printf.sprintf "%s:%d:%d" (source_file pi) pi.line pi.col
              | None -> "")
            ^ ":"
            ^ Addr.Hashtbl.find next_global pc
          in
          let rank =
            match String.Hashtbl.find_opt ranks key with
            | Some n -> n
            | None -> 0
          in
          String.Hashtbl.replace ranks key (rank + 1);
          Printf.sprintf
            "%s%s"
            (digest_name "camlc_tl" key)
            (if rank = 0 then "" else Printf.sprintf "_%d" rank)
        in
        let starts = ref [ 0, name pc ] in
        let n = ref 0 in
        let since = ref 0 in
        for i = 1 to m - 1 do
          n := !n + sizes.(i - 1);
          since := !since + sizes.(i - 1);
          if candidate.(i)
          then (
            let name = name chain.(i) in
            if
              !n >= 2 * toplevel_chunk_size
              || !n >= toplevel_chunk_size / 2
                 && Hashtbl.hash name land 0xfffff
                    < !since * 0x100000 / max 1 (toplevel_chunk_size / 2)
            then (
              starts := (i, name) :: !starts;
              n := 0);
            since := 0)
        done;
        let starts = Array.of_list (List.rev !starts) in
        if Array.length starts < 2
        then []
        else
          (* The code of each chunk: the jumps to the start of the next one
             go to a block which returns, and its first block starts with the
             last debug event before it *)
          let preds = Addr.Hashtbl.create 64 in
          Array.iter starts ~f:(fun (i, _) -> Addr.Hashtbl.replace preds chain.(i) []);
          List.iter rpo ~f:(fun pc' ->
              Code.fold_children
                blocks
                pc'
                (fun pc'' () ->
                  match Addr.Hashtbl.find_opt preds pc'' with
                  | Some l -> Addr.Hashtbl.replace preds pc'' (pc' :: l)
                  | None -> ())
                ());
          let events = Addr.Hashtbl.create 64 in
          let last_event = ref None in
          List.iter rpo ~f:(fun pc' ->
              if Addr.Hashtbl.mem preds pc'
              then Addr.Hashtbl.replace events pc' !last_event;
              List.iter (Addr.Map.find pc' blocks).body ~f:(fun i ->
                  match i with
                  | Event pi -> last_event := Some pi
                  | _ -> ()));
          let exit_pc = free_pc in
          Array.to_list
            (Array.mapi starts ~f:(fun k (i, name) ->
                 let start = chain.(i) in
                 let blocks =
                   match Addr.Hashtbl.find events start with
                   | Some pi when k > 0 ->
                       let block = Addr.Map.find start blocks in
                       Addr.Map.add
                         start
                         { block with body = Event pi :: block.body }
                         blocks
                   | Some _ | None -> blocks
                 in
                 let blocks =
                   if k + 1 = Array.length starts
                   then blocks
                   else
                     let next = chain.(fst starts.(k + 1)) in
                     let redirect ((pc', args) as cont) =
                       if pc' = next
                       then (
                         assert (List.is_empty args);
                         exit_pc, [])
                       else cont
                     in
                     List.fold_left
                       (Addr.Hashtbl.find preds next)
                       ~init:
                         (Addr.Map.add
                            exit_pc
                            { params = []; body = []; branch = Stop }
                            blocks)
                       ~f:(fun blocks pc' ->
                         let block = Addr.Map.find pc' blocks in
                         let branch =
                           match block.branch with
                           | Branch c -> Branch (redirect c)
                           | Poptrap c -> Poptrap (redirect c)
                           | Cond (x, c1, c2) -> Cond (x, redirect c1, redirect c2)
                           | Switch (x, conts) -> Switch (x, Array.map conts ~f:redirect)
                           | Pushtrap (c, x, h) -> Pushtrap (redirect c, x, redirect h)
                           | (Return _ | Raise _ | Stop) as b -> b
                         in
                         Addr.Map.add pc' { block with branch } blocks)
                 in
                 name, start, blocks))
