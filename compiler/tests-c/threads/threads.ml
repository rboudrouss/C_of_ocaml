(* Systhreads compiled by c_of_ocaml: each thread has its own shadow
   stack, and the code polls *)

(* Switches in the middle of deep recursions, with allocations and
   collections: each thread finds its values intact *)
let rec deep i n =
  if n = 0
  then (
    Thread.yield ();
    ignore (Array.make 1000 (string_of_int i));
    [ i ])
  else
    let s = Printf.sprintf "%d-%d" i n in
    let l = deep i (n - 1) in
    if s <> Printf.sprintf "%d-%d" i n then failwith "corrupted";
    (n + i) :: l

let stress i =
  for k = 1 to 200 do
    let l = deep i (10 + (((k * 7) + i) mod 50)) in
    if List.length l < 11 then failwith "length";
    if k mod 50 = 0 then Gc.minor ()
  done

let () =
  List.iter Thread.join (List.init 6 (fun i -> Thread.create stress i));
  print_endline "stress: ok"

(* Busy waiting, in a loop and by tail calls, which does not allocate:
   the other thread runs as the code polls *)
let flag = ref false

let rec wait () = if not !flag then wait ()

let () =
  let t = Thread.create (fun () -> flag := true) () in
  while not !flag do
    ()
  done;
  Thread.join t;
  flag := false;
  let t = Thread.create (fun () -> flag := true) () in
  wait ();
  Thread.join t;
  print_endline "busy waiting: ok"

(* Mutexes and conditions *)
let () =
  let m = Mutex.create () in
  let c = Condition.create () in
  let queue = Queue.create () in
  let producer () =
    for i = 1 to 1000 do
      Mutex.lock m;
      Queue.push i queue;
      Condition.signal c;
      Mutex.unlock m
    done
  in
  let sum = ref 0 in
  let consumer () =
    for _ = 1 to 1000 do
      Mutex.lock m;
      while Queue.is_empty queue do
        Condition.wait c m
      done;
      sum := !sum + Queue.pop queue;
      Mutex.unlock m
    done
  in
  let t1 = Thread.create consumer () in
  let t2 = Thread.create producer () in
  Thread.join t1;
  Thread.join t2;
  Printf.printf "producer and consumer: %d\n" !sum

(* Exceptions, in each thread *)
let () =
  let results = Array.make 4 "" in
  let ts =
    List.init 4 (fun i ->
        Thread.create
          (fun () ->
            results.(i) <-
              (try
                 for j = 1 to 100 do
                   if j = 50 + i then raise (Failure (string_of_int i));
                   Thread.yield ()
                 done;
                 "none"
               with Failure s -> s))
          ())
  in
  List.iter Thread.join ts;
  Printf.printf "exceptions: %s\n" (String.concat " " (Array.to_list results))
