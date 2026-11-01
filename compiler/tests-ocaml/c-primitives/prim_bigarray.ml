(* Bigarray accesses, which c_of_ocaml inlines for the kinds of elements
   which are numbers: every kind, both layouts, 1 to 3 dimensions, errors *)

open Bigarray

let show name f =
  let r = try f () with Invalid_argument m -> "Invalid_argument " ^ m in
  print_endline (name ^ ": " ^ r)

let test1 (type a b) name (kind : (a, b) kind) (of_int : int -> a) (to_string : a -> string) =
  let go : type l. string -> l layout -> unit =
   fun lname layout ->
    let a = Array1.create kind layout 5 in
    let first =
      match layout with
      | C_layout -> 0
      | Fortran_layout -> 1
    in
    for i = 0 to 4 do
      a.{first + i} <- of_int ((i * 77) - 150)
    done;
    for i = first - 2 to first + 6 do
      show (Printf.sprintf "%s %s get %d" name lname i) (fun () -> to_string a.{i});
      show
        (Printf.sprintf "%s %s set %d" name lname i)
        (fun () ->
          a.{i} <- of_int 1000;
          to_string a.{i})
    done
  in
  go "c" C_layout;
  go "fortran" Fortran_layout

let () =
  test1 "float32" float32 float_of_int string_of_float;
  test1 "float64" float64 (fun i -> float_of_int i /. 3.) string_of_float;
  test1 "int8s" int8_signed Fun.id string_of_int;
  test1 "int8u" int8_unsigned Fun.id string_of_int;
  test1 "int16s" int16_signed (fun i -> i * 300) string_of_int;
  test1 "int16u" int16_unsigned (fun i -> i * 300) string_of_int;
  test1 "int32" int32 (fun i -> Int32.mul (Int32.of_int i) 0x1000001l) Int32.to_string;
  test1 "int64" int64 (fun i -> Int64.mul (Int64.of_int i) 0x100000001L) Int64.to_string;
  test1 "int" int Fun.id string_of_int;
  test1 "nativeint" nativeint Nativeint.of_int Nativeint.to_string;
  test1 "char" char (fun i -> Char.chr (i land 255)) (fun c -> string_of_int (Char.code c));
  test1
    "complex64"
    complex64
    (fun i -> { Complex.re = float_of_int i; im = 1. })
    (fun c -> Printf.sprintf "%g+%gi" c.Complex.re c.Complex.im)

let () =
  let a = Array2.create int c_layout 3 4 in
  let f = Array2.create float64 fortran_layout 3 4 in
  for i = 0 to 2 do
    for j = 0 to 3 do
      a.{i, j} <- (10 * i) + j;
      f.{i + 1, j + 1} <- float_of_int ((10 * i) + j)
    done
  done;
  for i = -1 to 3 do
    for j = -1 to 4 do
      show (Printf.sprintf "2d c %d %d" i j) (fun () -> string_of_int a.{i, j});
      show (Printf.sprintf "2d fortran %d %d" i j) (fun () -> string_of_float f.{i, j})
    done
  done;
  let b = Array3.create int16_signed c_layout 2 3 4 in
  let g = Array3.create int32 fortran_layout 2 3 4 in
  for i = 0 to 1 do
    for j = 0 to 2 do
      for k = 0 to 3 do
        b.{i, j, k} <- -((100 * i) + (10 * j) + k);
        g.{i + 1, j + 1, k + 1} <- Int32.of_int ((100 * i) + (10 * j) + k)
      done
    done
  done;
  let s = ref 0 in
  for i = 0 to 1 do
    for j = 0 to 2 do
      for k = 0 to 3 do
        s := ((!s * 7) + b.{i, j, k} + (3 * Int32.to_int g.{i + 1, j + 1, k + 1})) land 0xFFFFFF
      done
    done
  done;
  Printf.printf "3d sum %d\n" !s;
  show "3d c 1 2 3" (fun () -> string_of_int b.{1, 2, 3});
  show "3d c 2 0 0" (fun () -> string_of_int b.{2, 0, 0});
  show "3d fortran 2 3 4" (fun () -> Int32.to_string g.{2, 3, 4});
  show "3d fortran 0 1 1" (fun () -> Int32.to_string g.{0, 1, 1});
  Printf.printf "dims %d %d %d %d %d\n" (Array2.dim1 a) (Array2.dim2 a) (Array3.dim1 b)
    (Array3.dim2 b) (Array3.dim3 b)

(* Wrong number of indices: a 2-dimensional array read as 1-dimensional *)
let () =
  let a = genarray_of_array2 (Array2.create int c_layout 2 2) in
  let a1 : (int, int_elt, c_layout) Array1.t = Obj.magic a in
  show "wrong dims get" (fun () -> string_of_int a1.{0});
  show "wrong dims set" (fun () ->
      a1.{0} <- 1;
      "set");
  show "nth_dim 2" (fun () -> string_of_int (Genarray.nth_dim a 2))

(* Sub-arrays share the data at an offset; boxed results survive a GC *)
let () =
  let a = Array1.init float64 c_layout 100 float_of_int in
  let s = Array1.sub a 10 20 in
  let l = List.init 20 (fun i -> s.{i}) in
  Gc.full_major ();
  print_endline (String.concat " " (List.map string_of_float l));
  let acc = ref 0. in
  for _ = 1 to 1000 do
    for i = 0 to 99 do
      acc := !acc +. a.{i};
      a.{i} <- a.{i} *. 0.5
    done
  done;
  Printf.printf "%g\n" !acc
