(* The float functions of the C library, called on unboxed floats by the
   generated code: same results as the primitives of the runtime (the
   expected output is that of emscripten's libm, musl's, not glibc's) *)
let xs =
  [ 0.; -0.; 0.5; -0.75; 1.; 2.5; -3.25; 10.; 1e-300; 1e300; infinity; neg_infinity; nan ]

let opaque = Sys.opaque_identity

external next_after : float -> float -> float = "caml_nextafter_float" "caml_nextafter"
[@@unboxed] [@@noalloc]

let p name f = List.iter (fun x -> Printf.printf "%s %h = %h\n" name x (f (opaque x))) xs

let p2 name f =
  List.iter
    (fun x ->
      List.iter
        (fun y -> Printf.printf "%s %h %h = %h\n" name x y (f (opaque x) (opaque y)))
        xs)
    xs

let () =
  p "exp2" Float.exp2;
  p "expm1" Float.expm1;
  p "log10" Float.log10;
  p "log2" Float.log2;
  p "log1p" Float.log1p;
  p "cbrt" Float.cbrt;
  p "round" Float.round;
  p "sinh" Float.sinh;
  p "cosh" Float.cosh;
  p "tanh" Float.tanh;
  p "asinh" Float.asinh;
  p "acosh" Float.acosh;
  p "atanh" Float.atanh;
  p "erf" Float.erf;
  p "erfc" Float.erfc;
  p2 "atan2" Float.atan2;
  p2 "hypot" Float.hypot;
  p2 "pow" Float.pow;
  p2 "fmod" Float.rem;
  p2 "copysign" Float.copy_sign;
  p2 "nextafter" next_after;
  List.iter
    (fun x ->
      Printf.printf "fma %h = %h\n" x (Float.fma (opaque x) (opaque 3.) (opaque 0.25)))
    xs
