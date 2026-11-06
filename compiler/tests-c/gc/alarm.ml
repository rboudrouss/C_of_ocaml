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

(* Gc.create_alarm, inlined: its record is not stored into a global (see
   globalize.ml), which would keep it alive, and its finaliser would never
   run *)

let alarms = ref 0

let finalised = ref 0

let () =
  let _ = Gc.create_alarm (fun () -> incr alarms) in
  for _ = 1 to 100 do
    Gc.finalise (fun _ -> incr finalised) (ref 0)
  done;
  let l = ref [] in
  for i = 1 to 2_000_000 do
    l := i :: !l;
    if i mod 100_000 = 0 then l := []
  done;
  Gc.full_major ();
  Gc.full_major ();
  Printf.printf "alarms %b, finalised %d\n" (!alarms > 0) !finalised
