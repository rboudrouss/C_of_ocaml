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

(* Tail calls between functions of different signatures, after calls which
   may raise: they loop in constant space *)

let n = 10_000_000

(* Opaque, may raise *)
let h = if Array.length Sys.argv > 5 then succ else fun x -> x

let t = Array.make 64 0

(* Known functions (large enough not to be inlined by the C compiler) *)
let rec f a b c d e =
  let r = h b in
  if a = 0
  then r + c + d + e
  else (
    for i = 0 to 7 do
      t.(i) <- (t.(i + 8) lxor (r * i)) + t.(i + 16)
    done;
    g (a - 1) (r + 1) c d)

and g a b c d =
  let r = h b in
  if a = 0
  then r + c + d
  else (
    for i = 0 to 7 do
      t.(i + 24) <- (t.(i + 32) lxor (r * i)) + t.(i + 40)
    done;
    f (a - 1) r c d (h 1))

(* Unknown functions *)
let k = ref (fun _ _ -> 0)

let u a b c = if a = 0 then h b + c else !k (a - 1) (h b + 1)

let () = k := fun a b -> u a b 0

(* Applications of an unknown function to more arguments than its arity
   (camlc_applyN_slow), whose last one is a tail call *)
let over = ref (fun _ x -> x)

let () =
  over :=
    fun n ->
      if n = 0
      then fun x -> x
      else
        let m = Sys.opaque_identity (n - 1) in
        fun x -> !over m (x + 1)

(* Partial applications of a function of more than 8 parameters (camlc_papN),
   whose application to the arguments left is a tail call *)
let pap = ref (fun _ -> 0)

let rec p9 a b c d e f g i n =
  if n = 0 then a + b + c + d + e + f + g + i + h n else !pap (n - 1)

(* (Obj.dup: an unknown function, whose partial application is not
   specialized, unlike through Sys.opaque_identity, which ocamlc drops) *)
let () = pap := (Obj.obj (Obj.dup (Obj.repr p9)) : _ -> _) 1 2 3 4 5 6 7 (h 8)

(* Unboxed floats *)
let rec x a (s : float) = if a = 0 then s else y (a - 1) (s +. 1.) (float_of_int (h a))

and y a s z = if a = 0 then s else x (a - 1) (s +. Float.min z 1.)

let () =
  Printf.printf
    "%d %d %d %.0f %d %d\n"
    (f n 0 0 0 0)
    (g n 0 0 0)
    (u n 0 0)
    (x n 0.)
    (!over n 0)
    (!pap n)
