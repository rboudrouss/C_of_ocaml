(* The results of the integer primitives are integers for the type
   analysis: the polymorphic comparisons of such integers, accumulated in
   loops, are integer comparisons, and they are not roots of the GC *)

let opaque = Sys.opaque_identity

let () =
  let best = ref 0 and worst = ref max_int and cmp = ref 0 and l = ref [] in
  for i = -1000 to 1000 do
    let x = ((i * 7919) + opaque 13) mod 1009 in
    let y = String.length (string_of_int i) + int_of_string (opaque "-3") in
    best := max !best (x * y);
    worst := min !worst (x - y);
    cmp := !cmp + compare x y + (if x >= y then 1 else 0) + if x = y then 10 else 0;
    (* Allocations meanwhile *)
    if i land 63 = 0 then l := (x, y) :: !l
  done;
  Gc.minor ();
  Printf.printf "%d %d %d %d\n" !best !worst !cmp (List.length !l);
  let a = opaque min_int and b = opaque max_int in
  let a = a lor 0 and b = b land max_int in
  Printf.printf
    "%b %b %d %d %d %d\n"
    (a < b)
    (a >= b)
    (compare a b)
    (compare b a)
    (max a b - b)
    (min a b - a);
  let h = Hashtbl.hash (opaque "abc") in
  Printf.printf "%b %d\n" (h >= 0) (compare (h land 0) 0)
