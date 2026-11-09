(* The division of boxed integers, computed on unboxed integers by the
   generated code: Division_by_zero, and the smallest integer divided by
   -1 *)
let opaque = Sys.opaque_identity

let test name to_string div rem xs =
  List.iter
    (fun x ->
      List.iter
        (fun y ->
          let r f =
            match f (opaque x) (opaque y) with
            | v -> to_string v
            | exception Division_by_zero -> "Division_by_zero"
          in
          Printf.printf
            "%s %s %s: %s %s\n"
            name
            (to_string x)
            (to_string y)
            (r div)
            (r rem))
        xs)
    xs

let () =
  test
    "int32"
    Int32.to_string
    Int32.div
    Int32.rem
    [ 0l; 1l; -1l; 7l; -7l; 3l; -3l; Int32.max_int; Int32.min_int ];
  test
    "int64"
    Int64.to_string
    Int64.div
    Int64.rem
    [ 0L; 1L; -1L; 7L; -7L; 3L; -3L; Int64.max_int; Int64.min_int ];
  (* (the bounds of nativeint depend on the target: not printed) *)
  test
    "nativeint"
    Nativeint.to_string
    Nativeint.div
    Nativeint.rem
    [ 0n; 1n; -1n; 7n; -7n; 3n; -3n ];
  let m = opaque Nativeint.min_int in
  Printf.printf
    "nativeint min_int / -1: %b %b %b\n"
    (Nativeint.div m (opaque (-1n)) = m)
    (Nativeint.rem m (opaque (-1n)) = 0n)
    (match Nativeint.div m (opaque 0n) with
    | _ -> false
    | exception Division_by_zero -> true)

(* In a loop, the quotient unboxed *)
let () =
  let s = ref 0L in
  for i = 1 to 1000 do
    s :=
      Int64.add !s (Int64.div (Int64.of_int (i * 7919)) (Int64.of_int ((i mod 13) + 1)))
  done;
  Printf.printf "%Ld\n" !s
