(* Numbers returned unboxed by the functions which cannot raise and are only
   called directly: in tail position or not, from recursive functions,
   functions allocating or handling exceptions, and numbers needed boxed by
   some callers *)

let[@inline never] ror32 a n = Int32.(logor (shift_right_logical a n) (shift_left a (32 - n)))

let[@inline never] e0 x = Int32.(logxor (ror32 x 2) (logxor (ror32 x 13) (ror32 x 22)))

let[@inline never] mix64 x = Int64.(logxor x (shift_right_logical x 33))

let[@inline never] nat n = Nativeint.(add (of_int n) 1n)

let[@inline never] hyp x y = sqrt ((x *. x) +. (y *. y))

(* Tail calls *)
let[@inline never] hyp_scaled k x y = hyp (k *. x) (k *. y)

let[@inline never] rec fsum n acc = if n = 0 then acc else fsum (n - 1) (acc +. float n)

(* Not a tail call *)
let[@inline never] rec fsum' n = if n = 0 then 0. else float n +. fsum' (n - 1)

(* A parameter or a computed number *)
let[@inline never] clamp x = if x > 100. then 100. else x *. 2.

(* Allocates, with collections *)
let[@inline never] list_sum n =
  let l = List.init n (fun i -> Int64.of_int i) in
  Gc.minor ();
  List.fold_left Int64.add 0L l

(* Handles an exception *)
let[@inline never] safe_div a b = try Int64.div a b with Division_by_zero -> -1L

(* Some of the results are needed boxed *)
let[@inline never] third x = x /. 3.

let boxed = ref []

(* Also used as a closure *)
let[@inline never] double x = x *. 2.

let apply = Sys.opaque_identity (fun f x -> f x)

let () =
  let s = ref 0l and t = ref 0L and u = ref 0n and f = ref 0. in
  for i = 1 to 1000 do
    s := Int32.add !s (e0 (Int32.of_int i));
    t := Int64.add !t (mix64 (Int64.of_int (i * 1_000_003)));
    u := Nativeint.add !u (nat i);
    f := !f +. hyp (float i) 2. +. hyp_scaled 0.5 (float i) 1.
  done;
  Printf.printf "%ld %Ld %nd %.3f\n" !s !t !u !f;
  Printf.printf "%.1f %.1f\n" (fsum 1000 0.) (fsum' 1000);
  Printf.printf "%.1f %.1f\n" (clamp 30.) (clamp 300.);
  Printf.printf "%Ld\n" (list_sum 10_000);
  Printf.printf "%Ld %Ld\n" (safe_div 10L 3L) (safe_div 10L 0L);
  let x = third 9. in
  boxed := third 6. :: !boxed;
  Gc.full_major ();
  Printf.printf "%.1f %.1f\n" x (List.hd !boxed);
  Printf.printf "%.1f %.1f\n" (double 4.) (apply double 5.)
