(* The value matched against a type with constant and non-constant
   constructors is an integer on some paths and a block on others: it must
   be a root of the GC where it is a block *)

type t =
  | A
  | B
  | C
  | D of string
  | E of int list

let garbage n = ignore (Sys.opaque_identity (List.init n string_of_int))

let[@inline never] f x =
  let n =
    match x with
    | A -> 0
    | B -> 1
    | C -> 2
    | D _ | E _ ->
        (* The memory [x] was in is reused after the collection *)
        Gc.minor ();
        garbage 100;
        3
  in
  (* [x] is used after the collection *)
  match x with
  | A | B | C -> n
  | D s -> n + String.length s
  | E l -> n + List.length l

let[@inline never] g x =
  (* A range of constant constructors *)
  let k = match x with A | B -> 10 | C -> 20 | D _ -> 30 | E _ -> 40 in
  Gc.minor ();
  garbage 50;
  match x with D s -> k + String.length s | E l -> k + List.length l | _ -> k

(* Young values, which the minor collections move *)
let[@inline never] make i =
  match i mod 5 with
  | 0 -> A
  | 1 -> B
  | 2 -> C
  | 3 -> D (String.make (i land 7) 'x')
  | _ -> E (List.init (i land 3) Fun.id)

let () =
  let total = ref 0 in
  for i = 1 to 1000 do
    total := !total + f (make i)
  done;
  for i = 1 to 1000 do
    total := !total + g (make i)
  done;
  Printf.printf "%d\n" !total
