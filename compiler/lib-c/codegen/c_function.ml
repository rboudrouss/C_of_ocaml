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

(* The translation of a function *)

open! Stdlib
open Code
open! C_repr
open! C_output
open! C_translate
open! C_frame
open! C_summary

let translate_function
    out
    facts
    global_flow_info
    blocks
    closures
    ~fn
    ~name
    ~params
    ~param_reprs
    ~env
    ~static_env
    ~source_name
    ~raises
    (pc, args) =
  out.refs <- Var.Set.empty;
  let ctx =
    { out
    ; facts
    ; global_flow_info
    ; blocks
    ; closures
    ; env_var = Var.fresh ()
    ; fn
    ; arity = None
    ; static_env
    ; locations = Var.Hashtbl.create 64
    ; slots = Var.Hashtbl.create 16
    ; traps = Addr.Hashtbl.create 16
    ; handler_ids = Addr.Hashtbl.create 16
    ; local_names = Var.Hashtbl.create 64
    ; labels = Addr.Hashtbl.create 16
    ; buf = Buffer.create 4096
    ; position = None
    ; source_name
    ; raises
    ; ret =
        (match fn with
        | Some f -> Var.Hashtbl.find out.fun_returns f
        | None -> Value)
    ; framed = false
    ; framed_blocks = None
    ; frame_push = ""
    }
  in
  List.iter env ~f:(fun (x, loc) -> Var.Hashtbl.replace ctx.locations x loc);
  let ctx = if List.is_empty env then ctx else { ctx with arity = Some param_reprs } in
  let locals = ref [] in
  let add_local x =
    if not (Var.Hashtbl.mem ctx.locations x || Var.Hashtbl.mem out.globals x)
    then (
      Var.Hashtbl.add ctx.locations x Local;
      locals := x :: !locals)
  in
  List.iter params ~f:add_local;
  let pcs = function_blocks ctx pc in
  List.iter pcs ~f:(fun pc ->
      let block = Addr.Map.find pc blocks in
      List.iter block.params ~f:add_local;
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, _) | Assign (x, _) -> add_local x
          | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ());
      match block.branch with
      | Pushtrap (_, x, _) -> add_local x
      | Return _ | Raise _ | Stop | Branch _ | Cond _ | Switch _ | Poptrap _ -> ());
  let rooted, live_in = rooted_variables ctx pcs ~params in
  let entry_poll, loop_polls = polls out blocks pc in
  assign_slots ctx pcs ~params ~entry:(pc, args) rooted live_in;
  let frame_size = Var.Hashtbl.fold (fun _ i n -> max n (i + 1)) ctx.slots 0 in
  (* The shadow stack measures the depth of the stack of the engine (see
     [camlc_segment_limit]) in the recursions: only the functions making
     calls which may be recursive (to their strongly connected component
     of known calls, or unknown) can go on a new segment, the others
     appearing at most once on the stack. The frame of such a function has
     at least a word for 100 bytes of its frame on the stack of the engine,
     which takes about 80 bytes and 2 per C variable (V8's baseline
     compiler). Without frame, it gets one if it makes one such call on
     each path: with more, the depth of the recursion is that of a tree
     (fib), costly to measure *)
  let non_tail_calls pc = non_tail_calls blocks pc in
  let recursive_call = recursive_call out global_flow_info closures ~fn in
  let calls pc = List.exists (non_tail_calls pc) ~f:recursive_call in
  let recursive_calls = recursive_calls out global_flow_info closures blocks ~fn pc in
  let depth_frame = frame_size = 0 && Option.is_some fn && recursive_calls = 1 in
  let depth_words =
    if Option.is_some fn && recursive_calls > 0 && (depth_frame || frame_size > 0)
    then 1 + (List.length !locals / 50)
    else 0
  in
  let frame_size = max frame_size depth_words in
  (* The first slots, written at the start of the function: those of the
     parameters, allocated first *)
  let initialized =
    let param_slots =
      List.filter_map (ctx.env_var :: params) ~f:(fun x ->
          Var.Hashtbl.find_opt ctx.slots x)
    in
    let rec count i = if List.mem ~eq:Int.equal i param_slots then count (i + 1) else i in
    count 0
  in
  (* Shrink-wrapping: the frame is allocated when entering the blocks which
     use it, and those reachable from them, if the start of the function
     does not need it. Not with exception handlers, reached from several
     blocks. *)
  let slot_var x = Var.Hashtbl.mem ctx.slots x in
  let uses_slots s = Var.Set.exists slot_var s in
  let cont_needs (pc', args) =
    uses_slots (use_list ctx args Var.Set.empty)
    || List.exists (Addr.Map.find pc' blocks).params ~f:slot_var
  in
  let needs pc =
    let block = Addr.Map.find pc blocks in
    (depth_frame && calls pc)
    || List.exists block.params ~f:slot_var
    || List.exists block.body ~f:(fun i ->
        (match instr_defs i with
          | Some x -> slot_var x
          | None -> false)
        || uses_slots (instr_uses ctx i Var.Set.empty)
        ||
        match i with
        | Let (x, Closure _) -> (
            match Var.Map.find_opt x closures with
            | Some { functions; _ } -> List.exists functions ~f:(fun (g, _) -> slot_var g)
            | None -> false)
        | _ -> false)
    ||
    match block.branch with
    | Return x | Raise (x, _) -> uses_slots (use ctx x Var.Set.empty)
    | Stop -> false
    | Branch c | Poptrap c -> cont_needs c
    | Cond (x, c1, c2) ->
        uses_slots (use ctx x Var.Set.empty) || cont_needs c1 || cont_needs c2
    | Switch (x, conts) ->
        uses_slots (use ctx x Var.Set.empty) || Array.exists conts ~f:cont_needs
    | Pushtrap _ -> true
  in
  let overflow_return =
    if ctx.raises
    then "return camlc_stack_overflow();"
    else "camlc_fatal_stack_overflow();"
  in
  let segment_label = ref false in
  let segment_stmt () =
    match fn with
    | Some f
      when Var.Set.mem f out.cps_fallback
           && repr_equal ctx.ret Value
           && List.for_all (Var.Hashtbl.find out.fun_reprs f) ~f:(fun r ->
               repr_equal r Value) ->
        (* With --cps-stack, the call goes on in the CPS version of the
           function (see [camlc_cps_stack]) *)
        let args =
          List.mapi params ~f:(fun i x ->
              if Var.Hashtbl.mem ctx.slots x
              then Printf.sprintf "a%d" i
              else local_name ctx x)
        in
        let helper = Printf.sprintf "camlc_c_%d" (List.length args) in
        String.Hashtbl.replace out.segment_thunks helper ();
        Printf.sprintf
          "if (fp < camlc_stack_limit) %s %s"
          overflow_return
          (let call =
             Printf.sprintf
               "%s(%s)"
               helper
               (String.concat ~sep:", " (as_value ctx f :: args))
           in
           if ctx.raises
           then Printf.sprintf "return %s;" call
           else Printf.sprintf "return camlc_cps_stack_noexc(%s);" call)
    | Some f ->
        let reprs = Var.Hashtbl.find out.fun_reprs f in
        let args =
          List.mapi params ~f:(fun i x ->
              let r = List.nth reprs i in
              ( r
              , if Var.Hashtbl.mem ctx.slots x
                then Printf.sprintf "a%d" i
                else local_name ctx x ))
          @ if List.is_empty env then [] else [ Value, "env" ]
        in
        let thunk =
          Printf.sprintf
            "camlc_s_%c_%s"
            (repr_letter ctx.ret)
            (String.concat
               ~sep:""
               (List.map args ~f:(fun (r, _) -> String.make 1 (repr_letter r))))
        in
        String.Hashtbl.replace out.segment_thunks thunk ();
        Printf.sprintf
          "if (fp < camlc_stack_limit) %s return %s(%s);"
          overflow_return
          thunk
          (String.concat
             ~sep:", "
             (Printf.sprintf "(void *)%s" (fun_name out f)
             :: "FRAME_SIZE"
             :: List.map args ~f:snd))
    | None -> overflow_return
  in
  (* When the frame would go below [camlc_segment_limit] but not below
     [camlc_stack_limit], the function is called again on a new segment
     of the stack of the wasm engine (see [camlc_segment]), through the
     thunk of its signature ([camlc_s_...], defined by [declare_symbols]),
     if nothing was done since its start (out of line, at label [S]);
     else it goes on on the current segment *)
  let frame_check ~restartable =
    if restartable && Option.is_some fn && recursive_calls > 0
    then (
      segment_label := true;
      "if (CAMLC_UNLIKELY(fp < camlc_segment_limit)) goto S;")
    else Printf.sprintf "if (CAMLC_UNLIKELY(fp < camlc_stack_limit)) %s" overflow_return
  in
  if
    frame_size > 0
    && initialized = 0
    && (not (cont_needs (pc, args)))
    && not
         (List.exists pcs ~f:(fun pc ->
              match (Addr.Map.find pc blocks).branch with
              | Pushtrap _ -> true
              | _ -> false))
  then (
    let framed = ref Addr.Set.empty in
    let rec mark pc =
      if not (Addr.Set.mem pc !framed)
      then (
        framed := Addr.Set.add pc !framed;
        match (Addr.Map.find pc blocks).branch with
        | Return _ | Raise _ | Stop -> ()
        | Branch (pc', _) | Poptrap (pc', _) -> mark pc'
        | Cond (_, (pc1, _), (pc2, _)) ->
            mark pc1;
            mark pc2
        | Switch (_, conts) -> Array.iter conts ~f:(fun (pc', _) -> mark pc')
        | Pushtrap _ -> assert false)
    in
    List.iter pcs ~f:(fun pc -> if needs pc then mark pc);
    if not (Addr.Set.mem pc !framed)
    then (
      (* The function can be restarted where it allocates its frame if
         what it may do before has no effect: the blocks from which the
         frame is allocated (a fixpoint) *)
      let reaches = ref !framed in
      let changed = ref true in
      while !changed do
        changed := false;
        List.iter pcs ~f:(fun pc ->
            if not (Addr.Set.mem pc !reaches)
            then
              let succ pc' = Addr.Set.mem pc' !reaches in
              let r =
                match (Addr.Map.find pc blocks).branch with
                | Return _ | Raise _ | Stop -> false
                | Branch (pc', _) | Poptrap (pc', _) -> succ pc'
                | Cond (_, (pc1, _), (pc2, _)) | Pushtrap ((pc1, _), _, (pc2, _)) ->
                    succ pc1 || succ pc2
                | Switch (_, conts) -> Array.exists conts ~f:(fun (pc', _) -> succ pc')
              in
              if r
              then (
                reaches := Addr.Set.add pc !reaches;
                changed := true))
      done;
      let restartable =
        List.for_all pcs ~f:(fun pc ->
            Addr.Set.mem pc !framed
            || (not (Addr.Set.mem pc !reaches))
            || List.for_all (Addr.Map.find pc blocks).body ~f:(fun i ->
                match i with
                | Let (_, e) -> Pure_fun.pure_expr Pure_fun.empty e
                | Event _ -> true
                | Assign _ | Set_field _ | Offset_ref _ | Array_set _ -> false))
      in
      ctx.framed_blocks <- Some !framed;
      ctx.frame_push <-
        String.concat
          ~sep:" "
          [ "fp = camlc_sp - FRAME_SIZE;"
          ; frame_check ~restartable
          ; "camlc_sp = fp;"
          ; "for (int i = 0; i < FRAME_SIZE; i++) fp[i] = Val_unit;"
          ]));
  ctx.framed <- frame_size > 0 && Option.is_none ctx.framed_blocks;
  (* The slots of the frame holding a value no longer live are cleared
     before the points where the GC may run, which would otherwise keep the
     value alive (as ocamlopt's frame descriptors do not): the slots
     written since they were last cleared, a forward analysis *)
  let env_slot = Var.Hashtbl.find_opt ctx.slots ctx.env_var in
  let slot_of x =
    match Var.Hashtbl.find_opt ctx.slots x with
    | Some i when not (Poly.equal (Some i) env_slot) -> Some i
    | _ -> None
  in
  let add_slot x d =
    match slot_of x with
    | Some i -> IntSet.add i d
    | None -> d
  in
  let live_slots live =
    Var.Set.fold
      (fun x acc ->
        match Var.Hashtbl.find_opt ctx.slots x with
        | Some i -> IntSet.add i acc
        | None -> acc)
      live
      IntSet.empty
  in
  let all_slots =
    Var.Hashtbl.fold (fun x _ acc -> add_slot x acc) ctx.slots IntSet.empty
  in
  let is_framed pc =
    frame_size > 0
    &&
    match ctx.framed_blocks with
    | Some s -> Addr.Set.mem pc s
    | None -> true
  in
  (* The live variables after each instruction of a block *)
  let live_after_body ~live_end body =
    let n = Array.length body in
    let live_after = Array.make n live_end in
    for k = n - 1 downto 1 do
      let i = body.(k) in
      let live =
        match instr_defs i with
        | Some x -> Var.Set.remove x live_after.(k)
        | None -> live_after.(k)
      in
      live_after.(k - 1) <- instr_uses ctx i live
    done;
    live_after
  in
  (* The variables live through the point where the GC may run of the
     instruction [i], the [k]-th of the block: those live after it, and the
     fields of the block it allocates, rooted during the allocation *)
  let gc_live_at live_after g k i =
    let live =
      match instr_defs i with
      | Some x -> Var.Set.remove x live_after.(k)
      | None -> live_after.(k)
    in
    match g with
    | `Alloc -> instr_uses ctx i live
    | `Call -> live
  in
  (* Through a point where the GC may run: the slots to clear, and the
     slots written then *)
  let through_gc_point d live =
    let l = live_slots live in
    IntSet.diff d l, IntSet.inter d l
  in
  let dirty_in = Addr.Hashtbl.create 16 in
  let handlers = ref Addr.Set.empty in
  List.iter pcs ~f:(fun pc ->
      match (Addr.Map.find pc blocks).branch with
      | Pushtrap (_, _, (h, _)) -> handlers := Addr.Set.add h !handlers
      | _ -> ());
  let block_dirty_in pc =
    if not (is_framed pc)
    then IntSet.empty
    else if Addr.Set.mem pc !handlers
    then all_slots
    else Option.value ~default:IntSet.empty (Addr.Hashtbl.find_opt dirty_in pc)
  in
  (let entry = List.fold_left params ~init:IntSet.empty ~f:(fun d x -> add_slot x d) in
   Addr.Hashtbl.replace dirty_in pc entry;
   let changed = ref true in
   while !changed do
     changed := false;
     List.iter pcs ~f:(fun pc ->
         if is_framed pc
         then (
           let block = Addr.Map.find pc blocks in
           let d = ref (block_dirty_in pc) in
           if Addr.Set.mem pc loop_polls
           then d := snd (through_gc_point !d (Addr.Hashtbl.find live_in pc));
           let body = Array.of_list block.body in
           let live_after = live_after_body ~live_end:(live_out ctx live_in pc) body in
           let gc_live = gc_live_at live_after in
           Array.iteri body ~f:(fun k i ->
               (match gc_point ctx i with
               | (`Call | `Alloc) as g -> d := snd (through_gc_point !d (gc_live g k i))
               | `No | `Raise -> ());
               match instr_defs i with
               | Some x -> d := add_slot x !d
               | None -> ());
           let succ (pc', _) =
             let d' =
               List.fold_left (Addr.Map.find pc' blocks).params ~init:!d ~f:(fun d x ->
                   add_slot x d)
             in
             let old =
               Option.value ~default:IntSet.empty (Addr.Hashtbl.find_opt dirty_in pc')
             in
             if not (IntSet.subset d' old)
             then (
               Addr.Hashtbl.replace dirty_in pc' (IntSet.union old d');
               changed := true)
           in
           match block.branch with
           | Return _ | Raise _ | Stop -> ()
           | Branch c | Poptrap c | Pushtrap (c, _, _) -> succ c
           | Cond (_, c1, c2) ->
               succ c1;
               succ c2
           | Switch (_, a) -> Array.iter a ~f:succ))
   done);
  let clear_slots d =
    if not (IntSet.is_empty d)
    then
      line
        ctx
        "%s"
        (String.concat
           ~sep:" "
           (List.map (IntSet.elements d) ~f:(fun i ->
                Printf.sprintf "fp[%d] = Val_unit;" i)))
  in
  (* Body *)
  List.iteri params ~f:(fun i x ->
      if Var.Hashtbl.mem ctx.slots x then line ctx "%s = a%d;" (slot ctx x) i);
  (match Var.Hashtbl.find_opt ctx.slots ctx.env_var with
  | Some i -> line ctx "fp[%d] = env;" i
  | None -> ());
  line ctx "%s" (branch ctx (pc, args));
  List.iter pcs ~f:(fun pc ->
      let block = Addr.Map.find pc blocks in
      let traps = Addr.Hashtbl.find ctx.traps pc in
      (match ctx.framed_blocks with
      | Some s -> ctx.framed <- Addr.Set.mem pc s
      | None -> ());
      Printf.bprintf ctx.buf " %s:;\n" (label ctx pc);
      let dirty = ref (block_dirty_in pc) in
      if Addr.Set.mem pc loop_polls
      then (
        if ctx.framed
        then (
          let clear, d = through_gc_point !dirty (Addr.Hashtbl.find live_in pc) in
          clear_slots clear;
          dirty := d);
        line
          ctx
          "%s"
          (poll_stmt
             (Addr.Hashtbl.find live_in pc
             |> Var.Set.elements
             |> List.filter ~f:(fun x -> gc_value ctx x)
             |> List.map ~f:(fun x -> local ctx x))
             ~raise:(raise_stmt ctx traps "r")));
      (* After a point where the GC may run, the variables of the frame
         live after it are reloaded (those live in a handler are reloaded
         at its start) *)
      let translate_body ?(live_end = live_out ctx live_in pc) body =
        let body = Array.of_list body in
        let live_after = live_after_body ~live_end body in
        Array.iteri body ~f:(fun k i ->
            (match gc_point ctx i with
            | (`Call | `Alloc) as g when ctx.framed ->
                let clear, d = through_gc_point !dirty (gc_live_at live_after g k i) in
                clear_slots clear;
                dirty := d
            | `Call | `Alloc | `No | `Raise -> ());
            (match instr_defs i with
            | Some x -> dirty := add_slot x !dirty
            | None -> ());
            translate_instr ctx traps i;
            match gc_point ctx i with
            | `Call | `Alloc ->
                let live =
                  match instr_defs i with
                  | Some x -> Var.Set.remove x live_after.(k)
                  | None -> live_after.(k)
                in
                let s = reload ctx (Var.Set.elements live) in
                if not (String.equal s "") then line ctx "%s" s
            | `No | `Raise -> ())
      in
      (* An application whose result is returned, outside any handler *)
      let tail_call =
        let rec strip_events l =
          match l with
          | Event _ :: r -> strip_events r
          | _ -> l
        in
        match strip_events (List.rev block.body), block.branch, traps with
        | Let (x, Apply { f; args; exact }) :: rem, Return y, [] when Var.equal x y ->
            Some (List.rev rem, x, f, args, exact)
        | _ -> None
      in
      match tail_call with
      | Some (body, x, f, args, exact) ->
          translate_body
            ~live_end:(instr_uses ctx (Let (x, Apply { f; args; exact })) Var.Set.empty)
            body;
          translate_tail_call ctx x ~f ~args ~exact
      | None -> (
          translate_body block.body;
          translate_last ctx traps block.branch;
          match block.branch with
          | Pushtrap (_, _, handler) ->
              Printf.bprintf ctx.buf " H%d:;\n" (Addr.Hashtbl.find ctx.handler_ids pc);
              line
                ctx
                "%s"
                (reload
                   ctx
                   (Var.Set.elements
                      (Var.Set.union
                         (Addr.Hashtbl.find_opt live_in (fst handler)
                         |> Option.value ~default:Var.Set.empty)
                         (use_list ctx (snd handler) Var.Set.empty))));
              line ctx "%s" (branch ctx handler)
          | Return _ | Raise _ | Stop | Branch _ | Cond _ | Switch _ | Poptrap _ -> ()));
  let param_set = Var.Set.of_list params in
  let decls =
    List.filter_map (List.rev !locals) ~f:(fun x ->
        if Var.Set.mem x param_set && not (Var.Hashtbl.mem ctx.slots x)
        then None
        else Some (var_repr ctx x, local_name ctx x))
  in
  let params_c =
    String.concat
      ~sep:", "
      (List.mapi params ~f:(fun i x ->
           if Var.Hashtbl.mem ctx.slots x
           then Printf.sprintf "value a%d" i
           else Printf.sprintf "%s %s" (c_type (var_repr ctx x)) (local_name ctx x))
      @ if List.is_empty env then [] else [ "value env" ])
  in
  let fb = Buffer.create (Buffer.length ctx.buf + 1024) in
  Printf.bprintf
    fb
    "%s(%s) {\n"
    name
    (if String.equal params_c "" then "void" else params_c);
  List.iter
    [ Value; Double; Unboxed_int64; Unboxed_int32; Unboxed_nativeint ]
    ~f:(fun r ->
      let l =
        List.filter_map decls ~f:(fun (r', d) -> if repr_equal r r' then Some d else None)
      in
      List.iteri l ~f:(fun i d ->
          if i mod 16 = 0
          then Printf.bprintf fb (if i = 0 then "  %s " else ";\n  %s ") (c_type r);
          if i mod 16 <> 0 then Buffer.add_string fb ", ";
          Buffer.add_string fb d);
      if not (List.is_empty l) then Buffer.add_string fb ";\n");
  Buffer.add_string
    fb
    "  value r, b, *argv, *bx;\n  (void)r; (void)b; (void)argv; (void)bx;\n";
  if entry_poll
  then
    Printf.bprintf
      fb
      "  %s\n"
      (poll_stmt
         (List.concat
            (List.mapi params ~f:(fun i x ->
                 if not (gc_value ctx x)
                 then []
                 else if Var.Hashtbl.mem ctx.slots x
                 then [ Printf.sprintf "a%d" i ]
                 else [ local_name ctx x ]))
         @ if List.is_empty env || static_env then [] else [ "env" ])
         ~raise:"return r;");
  if frame_size > 0 && Option.is_some ctx.framed_blocks
  then Printf.bprintf fb "#define FRAME_SIZE %d\n  value *fp;\n" frame_size
  else if frame_size > 0
  then
    Printf.bprintf
      fb
      "#define FRAME_SIZE %d\n\
      \  value *fp = camlc_sp - FRAME_SIZE;\n\
      \  %s\n\
      \  camlc_sp = fp;\n\
      \  for (int i = %d; i < FRAME_SIZE; i++) fp[i] = Val_unit;\n"
      frame_size
      (frame_check ~restartable:true)
      initialized;
  Buffer.add_buffer fb ctx.buf;
  if !segment_label then Printf.bprintf fb " S:;\n  %s\n" (segment_stmt ());
  Buffer.add_string fb "}\n";
  if frame_size > 0 then Buffer.add_string fb "#undef FRAME_SIZE\n";
  Buffer.add_char fb '\n';
  out.definitions <-
    { def_fn = fn
    ; def_name =
        (match String.rindex_opt name ' ' with
        | Some i -> String.sub name ~pos:(i + 1) ~len:(String.length name - i - 1)
        | None -> name)
    ; def_text = Buffer.contents fb
    ; def_size = Buffer.length fb
    ; def_refs = out.refs
    }
    :: out.definitions
