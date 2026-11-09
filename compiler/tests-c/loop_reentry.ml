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
