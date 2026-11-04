(* Bound errors and divisions by zero raised in loops, whose length reads the
   C compiler moves out of the loops: the exception leaves the loop at the
   right iteration, after the stores of the previous ones *)

let opaque = Sys.opaque_identity

let check name f =
  match f () with
  | v -> Printf.printf "%s: %d\n" name v
  | exception Invalid_argument s -> Printf.printf "%s: Invalid_argument %S\n" name s
  | exception Division_by_zero -> Printf.printf "%s: Division_by_zero\n" name

(* A partition, as quicksort's: loops reading and writing the array *)
let[@inline never] partition (a : int array) lo hi =
  let i = ref lo and j = ref hi in
  let pivot = a.(hi) in
  while !i < !j do
    while !i < hi && a.(!i) <= pivot do
      incr i
    done;
    while !j > lo && a.(!j) >= pivot do
      decr j
    done;
    if !i < !j then begin
      let t = a.(!i) in
      a.(!i) <- a.(!j);
      a.(!j) <- t
    end
  done;
  !i

(* The array read changes in the loop *)
let[@inline never] walk (arrays : int array array) =
  let t = ref 0 in
  for k = 0 to Array.length arrays - 1 do
    let a = arrays.(k) in
    for i = 0 to opaque 4 do
      t := !t + a.(i)
    done
  done;
  !t

let () =
  let a = Array.init 50 (fun i -> (i * 37) mod 50) in
  check "partition" (fun () -> partition a 0 49);
  check "partitioned" (fun () -> Array.fold_left (fun acc x -> (acc * 3) + x) 0 a land 0xffffff);
  check "partition past the end" (fun () -> partition a 0 (opaque 50));
  let b = Array.make 10 0 in
  check "fill past the end" (fun () ->
      for i = 0 to opaque 20 do
        b.(i) <- i + 1
      done;
      0);
  check "filled" (fun () -> Array.fold_left ( + ) 0 b);
  check "walk" (fun () -> walk [| Array.make 5 1; Array.make 8 2; Array.make 6 3 |]);
  check "walk a shorter array" (fun () -> walk [| Array.make 5 1; Array.make 4 2; Array.make 6 3 |]);
  let n = ref 0 in
  check "divide" (fun () ->
      let t = ref 0 in
      for i = 10 downto opaque (-3) do
        n := i;
        t := !t + (1000 / i) + (1000 mod i)
      done;
      !t);
  check "divided until" (fun () -> !n);
  check "float array past the end" (fun () ->
      let f = Array.init 7 float_of_int in
      let t = ref 0. in
      for i = 0 to opaque 7 do
        t := !t +. f.(i);
        f.(i) <- 0.
      done;
      int_of_float !t);
  check "string past the end" (fun () ->
      let s = opaque "abcdef" and t = ref 0 in
      for i = 0 to opaque 6 do
        t := !t + Char.code s.[i]
      done;
      !t)
