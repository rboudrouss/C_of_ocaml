(* Marshal.Closures with effect handlers (double translation): closures
   with and without CPS versions. The effect is performed by a function
   given as argument, which is not marshaled (the bytecode would copy the
   constructor of the effect). *)

open Effect
open Effect.Deep

type _ Effect.t += Get : int Effect.t

let copy (v : 'a) : 'a = Marshal.from_string (Marshal.to_string v [ Marshal.Closures ]) 0

let run f =
  match_with
    f
    ()
    { retc = (fun x -> x)
    ; exnc = raise
    ; effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Get -> Some (fun (k : (a, _) continuation) -> continue k 42)
          | _ -> None)
    }

let get () = perform Get

let () =
  let k = Sys.opaque_identity 3 in
  let pure = copy (fun x -> x + k) in
  let effectful = copy (fun p x -> x + p () + k) in
  let effectful2 = copy (fun x p y -> x + y + p ()) in
  let pap = copy (effectful2 1) in
  Printf.printf "%d\n" (run (fun () -> pure 1));
  Printf.printf "%d\n" (run (fun () -> effectful get 1));
  Printf.printf
    "%d\n"
    (run (fun () -> List.fold_left (fun a f -> a + f 1) 0 [ pure; effectful get; pap get ]));
  let rec even p n = if n = 0 then p () else odd p (n - 1)
  and odd p n = if n = 0 then -p () else even p (n - 1) in
  let even', odd' = copy (even, odd) in
  Printf.printf "%d %d\n" (run (fun () -> even' get 4)) (run (fun () -> odd' get 4))
