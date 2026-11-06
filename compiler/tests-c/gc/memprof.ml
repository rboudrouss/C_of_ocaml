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

(* Gc.Memprof samples the allocations of the generated code, and runs its
   callbacks while the program runs (OCaml 5.3 or later) *)

let a = Array.make 1024 (ref 0)

let samples = ref 0

let () =
  let tracker =
    { Gc.Memprof.null_tracker with
      alloc_minor =
        (fun _ ->
          incr samples;
          None)
    }
  in
  let _ = Gc.Memprof.start ~sampling_rate:1e-2 tracker in
  for i = 1 to 1_000_000 do
    a.(i land 1023) <- ref i
  done;
  Gc.Memprof.stop ();
  (* About 20000 *)
  Printf.printf
    "memprof %b %d\n"
    (!samples > 15_000 && !samples < 25_000)
    (Array.fold_left (fun s r -> s + !r) 0 a)

(* A callback which raises, as memprof-limits does, interrupts a loop which
   allocates: at the next poll *)
exception Interrupt

let () =
  let n = ref 0 in
  let tracker =
    { Gc.Memprof.null_tracker with
      alloc_minor =
        (fun _ ->
          incr n;
          if !n >= 10 then raise Interrupt;
          None)
    }
  in
  let _ = Gc.Memprof.start ~sampling_rate:1e-3 tracker in
  let i = ref 0 in
  (try
     while true do
       a.(!i land 1023) <- ref !i;
       incr i
     done
   with Interrupt -> print_endline "interrupted");
  Gc.Memprof.stop ();
  Printf.printf "samples %d\n" !n
