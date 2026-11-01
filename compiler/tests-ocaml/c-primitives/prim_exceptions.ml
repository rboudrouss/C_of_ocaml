(* C primitives which raise, and the comparisons and conversions with a
   fast path in c_of_ocaml: the exception is caught by the right handler,
   the shadow stack restored *)

type v =
  | A of int
  | B of int
  | C
  | D of string * float

type r = { x : float; y : float }

type t = Leaf | Node of t * int * t

let rec tree d k = if d = 0 then Leaf else Node (tree (d - 1) k, (if d = 1 then k else d), tree (d - 1) 0)

let values =
  let l = List.init 5000 (fun i -> i) in
  let lz = lazy (List.length [ 1; 2 ]) in
  ignore (Lazy.force lz);
  let o = object end in
  [| Obj.repr 0; Obj.repr 1; Obj.repr (-1); Obj.repr max_int; Obj.repr min_int
   ; Obj.repr ""; Obj.repr "a"; Obj.repr "ab"; Obj.repr "b"; Obj.repr "a\000"
   ; Obj.repr 0.; Obj.repr (-0.); Obj.repr nan; Obj.repr infinity; Obj.repr neg_infinity; Obj.repr 1.5
   ; Obj.repr (1, "a"); Obj.repr (1, "b"); Obj.repr (2, nan); Obj.repr (2, 1.)
   ; Obj.repr (Some 1); Obj.repr None; Obj.repr (Some nan); Obj.repr (Some (Some ""))
   ; Obj.repr [ 1; 2; 3 ]; Obj.repr [ 1; 2 ]; Obj.repr l; Obj.repr (List.map (fun x -> x) l)
   ; Obj.repr (l @ [ 1 ]); Obj.repr (tree 12 1); Obj.repr (tree 12 1); Obj.repr (tree 12 2)
   ; Obj.repr [| 1.; 2. |]; Obj.repr [| nan |]; Obj.repr 1L; Obj.repr 2L; Obj.repr 3l
   ; Obj.repr (A 1); Obj.repr (B 1); Obj.repr (A 2); Obj.repr C; Obj.repr (D ("x", nan)); Obj.repr (D ("x", 1.))
   ; Obj.repr { x = 1.; y = nan }; Obj.repr { x = 1.; y = 2. }
   ; Obj.repr lz; Obj.repr (lazy (List.length [])); Obj.repr o; Obj.repr (object end)
   ; Obj.repr (fun x -> x + 1); Obj.repr (Some (fun x -> x)); Obj.repr (ref 1); Obj.repr (ref 1)
  |]

let ops : (Obj.t -> Obj.t -> char) list =
  let b x = if x then '1' else '0' in
  [ (fun a b -> match compare a b with 0 -> '=' | n when n < 0 -> '<' | _ -> '>')
  ; (fun x y -> b (x = y)); (fun x y -> b (x <> y)); (fun x y -> b (x < y))
  ; (fun x y -> b (x <= y)); (fun x y -> b (x > y)); (fun x y -> b (x >= y))
  ]

let messages = ref []

let () =
  Array.iteri
    (fun i a ->
      let s = Buffer.create 512 in
      Array.iter
        (fun b ->
          List.iter
            (fun op ->
              Buffer.add_char s
                (try op a b with Invalid_argument m ->
                  if not (List.mem m !messages) then messages := m :: !messages;
                  'X'))
            ops;
          Buffer.add_char s ' ')
        values;
      Printf.printf "%2d %s\n" i (Buffer.contents s))
    values;
  List.iter print_endline (List.sort compare !messages)

(* Non tail recursion: frames on the shadow stack *)
let rec deep n f = if n = 0 then f () else 1 + deep (n - 1) f

let () =
  List.iter
    (fun s ->
      Printf.printf "int_of_string %S: %s\n" s
        (try string_of_int (int_of_string s) with Failure m -> "Failure " ^ m))
    [ "0"; "-0"; "7"; "-7"; "123456789"; "-123456789"; "0001234567"; ""; "-"; "+5"; "12a"
    ; "0x1F"; "-0b101"; "1_000"; "99999999999999999999"; " 1"; "--1" ];
  List.iter
    (fun (s, n) ->
      Printf.printf "Bytes.create %s: %s\n" s
        (try string_of_int (Bytes.length (Bytes.create n)) with Invalid_argument m -> m))
    [ "0", 0; "3", 3; "-1", -1; "-5", -5; "max + 1", Sys.max_string_length + 1 ];
  (* Values live in the frames across the handlers, and a GC *)
  let kept = ref [] in
  for i = 1 to 200 do
    let s = String.make 3 (Char.chr (65 + (i mod 26))) in
    (match deep 30 (fun () -> int_of_string ("x" ^ s)) with
    | _ -> ()
    | exception Failure _ -> kept := s :: !kept);
    (match deep 30 (fun () -> compare (fun x -> x) (fun x -> x)) with
    | _ -> ()
    | exception Invalid_argument _ -> kept := s :: !kept);
    if i mod 50 = 0 then Gc.full_major ()
  done;
  Printf.printf "%d %s\n" (List.length !kept) (String.concat "" (List.filteri (fun i _ -> i < 6) !kept));
  (* The innermost handler which matches *)
  let r =
    try
      try deep 10 (fun () -> Bytes.length (Bytes.create (-1))) with Not_found -> -1
    with Invalid_argument m -> deep 10 (fun () -> String.length m)
  in
  Printf.printf "outer handler: %d\n" r;
  let r = try deep 10 (fun () -> try int_of_string "z" with Failure _ -> 42) with _ -> -1 in
  Printf.printf "inner handler: %d\n" r;
  Printf.printf "after: %d\n" (deep 1000 (fun () -> int_of_string "5"))
