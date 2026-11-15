(* The loops of code run once, run by closures called again and again
   (compiler/lib/loop_reentry.ml): the same results *)
let opaque = Sys.opaque_identity

let h = Hashtbl.create 16

(* A toplevel loop with accumulators used after it, many iterations *)
let () =
  let s = ref 0 and p = ref 1 in
  for i = 1 to 1_000_000 do
    s := !s + (i land opaque 7);
    p := !p * 3 land 0xffff
  done;
  Printf.printf "accumulators %d %d\n" !s !p

(* An exception handler inside, an exception out of the loop *)
let () =
  let n = ref 0 in
  (try
     for i = 1 to 100_000 do
       (try if i mod 7 = 0 then raise Not_found else incr n
        with Not_found -> n := !n + 2);
       if i = 99_999 then raise Exit
     done
   with Exit -> n := - !n);
  Printf.printf "handlers %d\n" !n

(* Closures created in the loop, capturing its variables *)
let () =
  let fs = ref [] in
  for i = 1 to 1000 do
    Hashtbl.replace h (i mod 10) i;
    fs := (fun () -> i * 2) :: !fs
  done;
  Printf.printf
    "closures %d %d\n"
    (List.fold_left (fun a f -> a + f ()) 0 !fs)
    (Hashtbl.find h 3)

(* Nested loops, a while loop with several exits *)
let () =
  let a = Array.make 100 0 in
  let total = ref 0 in
  for i = 0 to 9_999 do
    for j = 0 to 99 do
      a.(j) <- a.(j) + (i lxor j)
    done;
    total := (!total + a.(i mod 100)) land 0xffffff
  done;
  let k = ref 0 in
  while
    !k < 1_000_000
    &&
    (incr k;
     a.(!k mod 100) <> opaque (-1))
  do
    if !k = 777_777 then k := 2_000_000
  done;
  Printf.printf "nested %d %d\n" !total !k

(* A function called once, whose loop returns a value from inside *)
let find_first l =
  let rec go i = function
    | [] -> -1
    | x :: r ->
        if x * x > 1_000_000
        then i
        else (
          Hashtbl.replace h i x;
          go (i + 1) r)
  in
  go 0 l

let () = Printf.printf "return %d\n" (find_first (List.init 5000 (fun i -> i)))

(* Several edges out of the loop, each with some of its variables used
   after it *)
let exits n m =
  let rec go i =
    let x = (i * 3) + opaque 1 in
    Hashtbl.replace h 0 x;
    if x > n
    then Printf.printf "exits x %d i %d\n" x i
    else if i land 1023 = m
    then Printf.printf "exits i %d\n" i
    else go (i + 1)
  in
  go 0

let () = exits 3_000_000 1000

(* (the same, called once too, leaving by the other edge) *)
let exits' n m =
  let rec go i =
    let x = (i * 3) + opaque 1 in
    Hashtbl.replace h 0 x;
    if x > n
    then Printf.printf "exits' x %d i %d\n" x i
    else if i land 1023 = m
    then Printf.printf "exits' i %d\n" i
    else go (i + 1)
  in
  go 0

let () = exits' 2_000 5_000

(* An exception raised inside a handler of the loop by a block which does
   not go back to the loop header, the loop left by an exception only *)
exception Done of int

let handlers_inside n =
  let s = Array.make 8 3 in
  let count = Array.make 1 0 in
  while true do
    count.(0) <- count.(0) + 1;
    try
      for i = 2 to 7 do
        let sx = s.(i) in
        if sx <> 0
        then (
          s.(i) <- sx - 1;
          raise Exit);
        if count.(0) >= n then raise (Done (count.(0) + i));
        s.(i) <- i
      done
    with Exit -> ()
  done

let () =
  try handlers_inside 100_000 with Done n -> Printf.printf "handlers inside %d\n" n

(* (the same, with no variable of the loop used after it) *)
let handlers_inside' n =
  let s = Array.make 8 3 in
  let k = Array.make 1 0 in
  let count = Array.make 1 0 in
  while true do
    count.(0) <- count.(0) + 1;
    try
      for i = 2 to 7 do
        k.(0) <- i;
        if s.(i) <> 0 then raise Exit;
        if count.(0) >= n then raise Exit;
        s.(i) <- i
      done
    with _ ->
      if count.(0) >= n
      then raise (Done (-count.(0)))
      else
        let i = k.(0) in
        s.(i) <- s.(i) - 1
  done

let () =
  try handlers_inside' 100_000 with Done n -> Printf.printf "handlers inside' %d\n" n
