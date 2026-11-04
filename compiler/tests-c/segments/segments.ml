(* Deep recursions, on a small stack of the wasm engine: run with JSPI,
   they use many segments of it (see tools/c_of_ocaml_segments_test.sh) *)

let n = 1_000_000

(* A list function, its frame allocated after the pattern matching *)
let rec map f = function
  | [] -> []
  | x :: r -> f x :: map f r

(* Without values to keep on the shadow stack *)
let rec count n = if n = 0 then 0 else 1 + count (n - 1)

(* An exception handler in each frame *)
let rec handlers n = if n = 0 then 0 else 1 + try handlers (n - 1) with Not_found -> 0

(* An exception raised at the bottom, caught at the top and in the middle *)
exception Bottom of int

let rec raise_at_bottom n =
  if n = 0 then raise (Bottom 42) else 1 + raise_at_bottom (n - 1)

let rec catch_in_middle n =
  if n = 0
  then raise (Bottom 0)
  else if n = 500_000
  then try catch_in_middle (n - 1) with Bottom k -> k + 1
  else 1 + catch_in_middle (n - 1)

(* Calls to unknown functions *)
let next = ref (fun _ -> 0)

let unknown n = if n = 0 then 0 else 1 + !next (n - 1)

let () = next := unknown

(* An unboxed float result *)
let rec sum = function
  | [] -> 0.
  | x :: r -> x +. sum r

(* Mutual recursion, with an allocation in each frame *)
let rec even n = if n = 0 then [] else n :: odd (n - 1)

and odd n = if n = 0 then [] else -n :: even (n - 1)

(* Many arguments *)
let rec args a b c d e f g h n =
  if n = 0 then a + b + c + d + e + f + g + h else 1 + args b c d e f g h a (n - 1)

(* Deep, then shallow again, many times: the segments are released *)
let rec up_down k = if k = 0 then 0 else count 10_000 + up_down (k - 1)

let () =
  let l = List.init n Fun.id in
  Printf.printf "map %d\n" (List.length (map succ l));
  Printf.printf "count %d\n" (count n);
  Printf.printf "handlers %d\n" (handlers n);
  (try ignore (raise_at_bottom n) with Bottom k -> Printf.printf "bottom %d\n" k);
  Printf.printf "middle %d\n" (catch_in_middle n);
  Printf.printf "unknown %d\n" (unknown n);
  Printf.printf "sum %.0f\n" (sum (List.map float l));
  Printf.printf "even %d\n" (List.length (even n));
  Printf.printf "args %d\n" (args 1 2 3 4 5 6 7 8 n);
  Printf.printf "up_down %d\n" (up_down 1000);
  Gc.compact ();
  (* Exiting at the bottom of a deep recursion *)
  let rec exit_at_bottom n =
    if n = 0
    then (
      print_endline "exit";
      exit 3)
    else 1 + exit_at_bottom (n - 1)
  in
  ignore (exit_at_bottom n)
