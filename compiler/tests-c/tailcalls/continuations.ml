(* Tail calls only, through closures of different arities and continuations
   [next], in the shape of Constr_matching.sub_match of Rocq: a loop of tail
   calls which goes through unknown calls *)
type t = Leaf of int | Node of t * t

let flag = ref true

let authorized c =
  match c with
  | Leaf n when n = -1 -> fun next -> 1 + next ()
  | _ -> if !flag then fun next -> next () else fun next -> next ()

let rec aux c mk_ctx next =
  let here = authorized c in
  let next () =
    match c with
    | Leaf _ -> next ()
    | Node (a, b) ->
        let next_mk_ctx = function [ a; b ] -> mk_ctx (Node (a, b)) | _ -> assert false in
        try_aux [ a; b ] next_mk_ctx next
  in
  here next

and try_aux lc mk_ctx next =
  let rec try_sub_match_rec lacc lc =
    match lc with
    | [] -> next ()
    | c :: tl ->
        let mk_ctx ce = mk_ctx (List.rev_append lacc (ce :: tl)) in
        let next () = try_sub_match_rec (c :: lacc) tl in
        aux c mk_ctx next
  in
  try_sub_match_rec [] lc

let () =
  let rec build n acc = if n = 0 then acc else build (n - 1) (Node (acc, Leaf n)) in
  let t = build 1_000_000 (Leaf 0) in
  Printf.printf "%d\n" (aux t (fun x -> x) (fun () -> 42))
