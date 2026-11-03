(* Bound checks of arrays of values, of floats and of unknown kind, in
   loops, raising Invalid_argument caught by handlers or not *)

let opaque = Sys.opaque_identity

let check name f =
  match f () with
  | v -> Printf.printf "%s: %d\n" name v
  | exception Invalid_argument s -> Printf.printf "%s: Invalid_argument %S\n" name s

(* Of unknown kind: float or not *)
let[@inline never] get (a : 'a array) i = a.(i)

let[@inline never] set (a : 'a array) i v = a.(i) <- v

let () =
  let a = Array.init 10 (fun i -> i * i) in
  let f = Array.init 10 float_of_int in
  let s = Array.init 10 string_of_int in
  check "sum" (fun () ->
      let t = ref 0 in
      for i = 0 to Array.length a - 1 do
        t := !t + a.(i)
      done;
      !t);
  check "get past the end" (fun () ->
      let t = ref 0 in
      for i = 0 to opaque 10 do
        t := !t + a.(i)
      done;
      !t);
  check "get negative" (fun () -> a.(opaque (-1)));
  check "set past the end" (fun () ->
      for i = 0 to opaque 10 do
        a.(i) <- i
      done;
      0);
  check "set kept" (fun () -> a.(9));
  check "float get" (fun () -> int_of_float (f.(opaque 3) +. f.(opaque 9)));
  check "float get past the end" (fun () -> int_of_float f.(opaque 10));
  check "float set past the end" (fun () ->
      f.(opaque 10) <- 1.;
      0);
  check "string get" (fun () -> String.length s.(opaque 9));
  check "string set past the end" (fun () ->
      s.(opaque 12) <- "x";
      0);
  check "generic" (fun () -> get a 2 + int_of_float (get f 4) + String.length (get s 5));
  check "generic get past the end" (fun () -> int_of_float (get f (opaque 10)));
  check "generic set past the end" (fun () ->
      set s (opaque 10) "y";
      0);
  check "generic set" (fun () ->
      set f 1 2.5;
      set a 1 7;
      int_of_float (f.(1) *. 2.) + a.(1));
  (* The exception goes through a function *)
  let[@inline never] nth a i = a.(i) in
  check "through a call" (fun () -> nth a 3 + nth a (opaque 20))
