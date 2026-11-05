(* Gc.ramp_up (OCaml 5.4): its function, called back by the runtime's C,
   recurses deeper than the stack of the wasm engine, on new segments as if
   called by the code *)
let rec count n = if n = 0 then 0 else 1 + count (n - 1)

let rec map f = function [] -> [] | x :: r -> f x :: map f r

let () =
  let n = 1_000_000 in
  let c, _ = Gc.ramp_up (fun () -> count n) in
  Printf.printf "ramp_up %d\n" c;
  let l, _ = Gc.ramp_up (fun () -> map succ (List.init n Fun.id)) in
  Printf.printf "ramp_up map %d\n" (List.length l);
  (* Under ramp_up, through another callback from C: the restriction applies again *)
  (try
     ignore (Gc.ramp_up (fun () -> if count 10 = 10 then raise Exit));
     print_endline "no exception"
   with Exit -> print_endline "exception through ramp_up")
