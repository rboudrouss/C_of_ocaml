(* Numbers which c_of_ocaml keeps unboxed and boxes where they are passed
   to a call, rather than at their definition: accumulators of loops given
   to known and unknown functions, partial applications, Printf, stored,
   captured by closures, carried by exceptions *)

exception E of int64

let store = ref []

let[@inline never] keep (x : int64) = store := Obj.repr x :: !store

let[@inline never] twice f x = f (f x)

let unknown = ref (fun (x : int64) -> Int64.add x 1L)

let () =
  let acc = ref 0L and acc32 = ref 0l and accn = ref 0n and accf = ref 0. in
  for i = 1 to 100_000 do
    acc := Int64.add !acc (Int64.of_int (i * 3));
    acc32 := Int32.logxor !acc32 (Int32.of_int i);
    accn := Nativeint.add !accn (Nativeint.of_int (i land 7));
    accf := !accf +. (float_of_int i /. 7.)
  done;
  Printf.printf "%Ld %ld %nd %.3f\n" !acc !acc32 !accn !accf;
  keep !acc;
  print_endline (Int64.to_string (!unknown !acc));
  print_endline (Int64.to_string (twice (Int64.mul 3L) !acc));
  let f = Int64.add !acc in
  print_endline (Int64.to_string (f 5L));
  let g () = Int64.sub !acc 1L in
  Gc.full_major ();
  print_endline (Int64.to_string (g ()));
  match raise (E !acc) with
  | () -> ()
  | exception E x -> print_endline (Int64.to_string x)

(* A number defined outside a loop and passed to calls in the loop *)
let () =
  let x = Int64.of_string "123456789012" in
  let s = ref 0L in
  for _ = 1 to 1000 do
    s := !unknown (Int64.add !s x)
  done;
  keep x;
  Printf.printf "%Ld\n" !s

(* Several calls (each one may box the number: physical equality is not
   specified on boxed numbers, ocamlopt boxes at each call too) *)
let () =
  let a = ref 1L in
  for i = 1 to 50 do
    a := Int64.mul !a (Int64.of_int ((i land 3) + 1))
  done;
  let x = !a in
  keep x;
  keep x;
  (match !store with
  | y :: z :: _ -> Printf.printf "%b %Ld\n" (Obj.obj y = (Obj.obj z : int64)) (Obj.obj y : int64)
  | _ -> ());
  Printf.printf "%d\n" (List.length !store)

(* Recursion: the accumulator is passed in a loop of calls *)
let rec sum n (acc : int64) = if n = 0 then acc else sum (n - 1) (Int64.add acc (Int64.of_int n))

let () = Printf.printf "%Ld\n" (sum 10000 0L)
