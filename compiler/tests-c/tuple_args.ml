(* Tupled parameters passed as several parameters (compiler/lib/tuple_args.ml),
   and the cases where they are not *)
let opaque = Sys.opaque_identity

(* Recursive, with a self tail call (a loop) and constant tuples *)
let rec tak (x, y, z) =
  if x > y then tak (tak (x - 1, y, z), tak (y - 1, z, x), tak (z - 1, x, y)) else z

(* Some fields only, read in a closure *)
let rec sum (a, _, c) acc =
  if a = 0 then acc () + c else sum (a - 1, opaque 0, c) (fun () -> acc () + a)

(* A record, mutable: fresh at each call *)
type r =
  { mutable u : int
  ; v : string
  }

let rec count ({ u; v } : r) =
  if u = 0 then String.length v else count { u = u - 1; v = v ^ "." }

(* The tuple also used by the caller: not passed as fields *)
let pair (a, b) = a + b

let shared n =
  let t = opaque (n, n + 1) in
  pair t + fst t

(* A function which escapes: not changed *)
let first (a, _) = a

let escaping l = List.map first l

(* A tuple modified between its allocation and the call: not passed as fields *)
let rec loop (r, n) = if n = 0 then !r else loop (r, n - 1)

let modified () =
  let r = ref 1 in
  let t = r, 3 in
  r := 7;
  loop t

(* Floats *)
let rec dist (x, y) n =
  if n = 0 then sqrt ((x *. x) +. (y *. y)) else dist (x +. 1., y *. 0.5) (n - 1)

(* An exception raised with the fields *)
exception E of int

let check (a, b) = if a > b then raise (E (a - b)) else b - a

let () =
  Printf.printf "tak %d\n" (tak (18, 12, 6));
  Printf.printf "sum %d\n" (sum (opaque 10, 0, 5) (fun () -> 1));
  Printf.printf "count %d\n" (count { u = 5; v = "ab" });
  Printf.printf "shared %d\n" (shared 4);
  Printf.printf
    "escaping %s\n"
    (String.concat "," (List.map string_of_int (escaping [ 1, 'a'; 2, 'b' ])));
  Printf.printf "modified %d\n" (modified ());
  Printf.printf "dist %.6f\n" (dist (3., 4.) 5);
  Printf.printf "check %d\n" (check (opaque 2, 9));
  match check (opaque 9, 2) with
  | n -> Printf.printf "check %d\n" n
  | exception E n -> Printf.printf "E %d\n" n
