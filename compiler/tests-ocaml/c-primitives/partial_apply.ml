(* Partial applications of unknown functions: c_of_ocaml allocates them
   with the number of arguments given, and those of the functions of arity
   at most 8 call them directly once applied to the arguments left; the
   others, and those of plugins, go through the generic application *)

let opaque = Sys.opaque_identity

let f2 = opaque (fun a b -> (a * 10) + b)

let f3 = opaque (fun a b c -> (((a * 10) + b) * 10) + c)

let f5 = opaque (fun a b c d e -> Printf.sprintf "%d%d%d%d%d" a b c d e)

let f8 = opaque (fun a b c d e f g h -> a + b + c + d + e + f + g + (h * 100))

let f9 = opaque (fun a b c d e f g h i -> [ a; b; c; d; e; f; g; h; i ])

let f10 = opaque (fun a b c d e f g h i j -> a + b + c + d + e + f + g + h + i + (j * 1000))

exception E of int

let raising = opaque (fun a b -> if b = 0 then raise (E a) else a / b)

(* A function returning a function: over-application *)
let returning = opaque (fun a -> if a > 0 then fun b c -> a + b + c else fun b _ -> b)

let () =
  let p = opaque (f2 4) in
  Printf.printf "%d\n" (p 2);
  let p = opaque (f3 1) in
  let q = opaque (p 2) in
  Printf.printf "%d %d %d\n" (q 3) (p 4 5) ((opaque (f3 7 8)) 9);
  Printf.printf "%s %s\n" ((opaque (f5 1 2)) 3 4 5) ((opaque ((opaque (f5 9)) 8 7)) 6 5);
  Printf.printf "%d %d\n" ((opaque (f8 1)) 2 3 4 5 6 7 8) ((opaque (f8 1 1 1 1 1 1 1)) 2);
  Printf.printf
    "%s\n"
    (String.concat " " (List.map string_of_int ((opaque (f9 1 2 3)) 4 5 6 7 8 9)));
  let p = opaque (f10 1 2) in
  let q = opaque (p 3 4 5 6 7) in
  Printf.printf "%d %d\n" (q 8 9 10) ((opaque (q 1)) 1 1);
  Printf.printf "%d\n" ((opaque (raising 7)) 2);
  (match (opaque (raising 7)) 0 with
  | _ -> print_endline "no exception"
  | exception E n -> Printf.printf "E %d\n" n);
  Printf.printf "%d %d\n" (returning 1 2 3) (returning 0 2 3);
  (* Partial applications as arguments, and called back by the runtime *)
  Printf.printf
    "%s\n"
    (String.concat " " (List.map string_of_int (List.map (f3 1 2) [ 3; 4; 5 ])));
  let l = Array.to_list (Array.map (f2 3) [| 1; 2 |]) in
  Printf.printf "%d %d\n" (List.nth l 0) (List.nth l 1);
  let r = ref 0 in
  let finalise = opaque (fun n -> Gc.finalise ((opaque (fun a _ -> r := !r + a)) 2) (ref n)) in
  for i = 1 to 100 do
    finalise i
  done;
  Gc.full_major ();
  Printf.printf "%b\n" (!r >= 190)

(* Many partial applications, with collections in between: their
   arguments must survive *)
let () =
  let fs = Array.init 16 (fun k -> opaque (fun a b c -> (a * k) + b + c)) in
  let keep = Array.make 64 (fun _ -> 0) in
  let s = ref 0 in
  for i = 0 to 200_000 do
    let p = fs.(i land 15) (string_of_int i |> int_of_string) in
    let q = p (List.length [ i; i ]) in
    keep.(i land 63) <- q;
    if i land 1023 = 0 then Gc.minor ();
    s := (!s + keep.((i * 7) land 63) i) land 0xFFFFFF
  done;
  Printf.printf "%d\n" !s
