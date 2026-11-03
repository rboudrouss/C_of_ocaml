(* The variables live where the GC may run are in the frame of the
   function, and read from C variables reloaded after each collection: after
   calls and allocations, in handlers, in the fields of the blocks and
   closures allocated, after the boxing of arguments, and the closure of the
   function itself *)

let opaque = Sys.opaque_identity

let garbage n = ignore (opaque (List.init n (fun i -> string_of_int i)))

(* Calls and allocations *)
let () =
  let a = Array.init 100 (fun i -> string_of_int i) in
  let l = ref [] and s = ref "" in
  for i = 0 to 9999 do
    let x = a.(i mod 100) in
    let p = (x, i) in
    garbage 3;
    l := p :: !l;
    if i mod 1000 = 0 then Gc.minor ();
    if i mod 5000 = 0 then Gc.compact ();
    s := x
  done;
  let t = List.fold_left (fun acc (x, i) -> acc + String.length x + i) 0 !l in
  Printf.printf "%d %s %s\n" t !s a.(42)

(* Handlers *)
let () =
  let a = opaque (Array.make 10 "x") in
  let n = ref 0 in
  for i = 0 to 999 do
    let v = string_of_int i in
    match
      garbage 5;
      if i mod 3 = 0 then raise Not_found;
      Gc.minor ();
      a.(i)
    with
    | w -> n := !n + String.length w + String.length v
    | exception (Not_found | Invalid_argument _) -> n := !n + String.length v + Array.length a
  done;
  Printf.printf "%d\n" !n

(* Blocks, float arrays and closures allocated with the variables, and the
   closure of the function *)
let make k =
  let base = opaque (String.make k 'b') and f = opaque (float_of_int k) in
  fun i ->
    garbage 4;
    let r = (base, i, string_of_int i) in
    let c = [| f; float_of_int i |] in
    let g () = String.length base + i in
    Gc.minor ();
    let (s, j, t), d = r, c.(0) +. c.(1) in
    String.length s + j + String.length t + int_of_float d + g ()

let () =
  let fs = List.init 20 make in
  let t = ref 0 in
  for i = 0 to 499 do
    t := !t + (List.nth fs (i mod 20)) i
  done;
  Printf.printf "%d\n" !t

(* The boxing of an argument before a call *)
let unknown = opaque (fun (x : float) (s : string) (y : int64) -> x +. float (String.length s) +. Int64.to_float y)

let () =
  let acc = ref 0. and s = opaque (String.make 7 's') in
  for i = 1 to 2000 do
    let x = float i *. 0.5 and y = Int64.of_int i in
    acc := !acc +. unknown x s y
  done;
  Printf.printf "%.1f\n" !acc
