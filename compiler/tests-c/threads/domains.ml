(* Domains (OCaml 5) compiled by c_of_ocaml *)

let rec deep d n =
  if n = 0
  then [ d ]
  else
    let s = string_of_int (n + d) in
    let l = deep d (n - 1) in
    if s <> string_of_int (n + d) then failwith "corrupted";
    (n * d) :: l

(* Computations in parallel, with collections *)
let () =
  let counter = Atomic.make 0 in
  let work d =
    let total = ref 0 in
    for k = 1 to 2000 do
      total := !total + List.length (deep d (k mod 100));
      Atomic.incr counter
    done;
    !total
  in
  let ds = List.init 3 (fun d -> Domain.spawn (fun () -> work (d + 1))) in
  let r = work 0 in
  let rs = List.map Domain.join ds in
  Gc.full_major ();
  Printf.printf
    "parallel: %s, %d\n"
    (String.concat " " (List.map string_of_int (r :: rs)))
    (Atomic.get counter)

(* Busy waiting, which does not allocate, while another domain
   allocates: the stop-the-world collections need the code to poll *)
let () =
  let flag = Atomic.make false in
  let d =
    Domain.spawn (fun () ->
        let l = List.init 1_000_000 Fun.id in
        Atomic.set flag true;
        List.length l)
  in
  while not (Atomic.get flag) do
    ()
  done;
  Printf.printf "busy waiting: %d\n" (Domain.join d)

(* Effect handlers, in several domains and threads *)
open Effect
open Effect.Deep

type _ Effect.t += Yield : int -> unit Effect.t

let sum_gen d n =
  let total = ref 0 in
  match_with
    (fun () ->
      for i = 1 to n do
        perform (Yield (i * d));
        if i mod 100 = 0 then Thread.yield ()
      done)
    ()
    { retc = (fun () -> !total)
    ; exnc = raise
    ; effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Yield v ->
              Some
                (fun (k : (a, _) continuation) ->
                  total := !total + v;
                  ignore (Array.make 10 v);
                  continue k ())
          | _ -> None)
    }

let in_domain d =
  let res = Array.make 3 0 in
  let ts =
    List.init 3 (fun i -> Thread.create (fun () -> res.(i) <- sum_gen (d + i) 2000) ())
  in
  List.iter Thread.join ts;
  Array.fold_left ( + ) 0 res

let () =
  let ds = List.init 3 (fun d -> Domain.spawn (fun () -> in_domain (d + 1))) in
  let r = in_domain 0 in
  let rs = List.map Domain.join ds in
  Printf.printf "effects: %s\n" (String.concat " " (List.map string_of_int (r :: rs)))
