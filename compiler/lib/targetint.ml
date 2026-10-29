(* Integers of [num_bits] bits (31 or 32, or 63 for 64-bit targets),
   sign-extended to 64 bits *)
type t = Int64.t

let num_bits_ = ref 0

let set_num_bits x = num_bits_ := x

let num_bits () =
  match !num_bits_ with
  | (31 | 32 | 63) as x -> x
  | x -> failwith (Printf.sprintf "Targetint.num_bits %d unsupported" x)

type offset = Offset of int [@@ocaml.unboxed]

let () = assert (Obj.is_int (Obj.repr (Offset 0)))

let offset () = Offset (64 - num_bits ())

let equal = Int64.equal

let compare = Int64.compare

let wrap (Offset offset) i = Int64.(shift_left i offset)

let unwrap (Offset offset) i = Int64.(shift_right i offset)

let wrap_modulo i =
  let offset = offset () in
  unwrap offset (wrap offset i)

let max_int_ (Offset offset) = Int64.shift_right Int64.max_int offset

let min_int_ (Offset offset) = Int64.shift_right Int64.min_int offset

let min_int () =
  let offset = offset () in
  min_int_ offset

let max_int () =
  let offset = offset () in
  max_int_ offset

let to_string x = Int64.to_string x

let to_float x = Int64.to_float x

(* The low 32 bits *)
let to_int32 x = Int64.to_int32 x

let to_int64 x = x

let to_int_exn x =
  if Sys.int_size >= 63 || (Int64.of_int Int.min_int <= x && x <= Int64.of_int Int.max_int)
  then Int64.to_int x
  else failwith "to_int_exn"

let neg x =
  let offset = offset () in
  unwrap offset (Int64.neg (wrap offset x))

let abs x =
  let offset = offset () in
  unwrap offset (Int64.abs (wrap offset x))

let int_binop f x y = wrap_modulo (f x y)

let add = int_binop Int64.add

let zero = 0L

let one = 1L

let succ x = add x one

let sub = int_binop Int64.sub

let mul = int_binop Int64.mul

let div = int_binop Int64.div

let rem = int_binop Int64.rem

let logand = int_binop Int64.logand

let logor = int_binop Int64.logor

let logxor = int_binop Int64.logxor

let shift_op f x y =
  let offset = offset () in
  (* Limit the shift offset to [0, 31] for 31 and 32 bit integers, and to
     [0, 63] for 63 bit integers *)
  unwrap offset (f (wrap offset x) (y land if num_bits () > 32 then 0x3f else 0x1f))

let shift_left = shift_op Int64.shift_left

let shift_right = shift_op Int64.shift_right

let shift_right_logical = shift_op Int64.shift_right_logical

let is_zero x = equal x 0L

(* Bit counting over [num_bits] bits. As in the OCaml runtime, the
   low tag bit keeps [leading_zeros] from counting past [num_bits]. *)

let high x = Int64.to_int32 (Int64.shift_right_logical x 32)

let low x = Int64.to_int32 x

let leading_zeros x =
  let (Offset offset as o) = offset () in
  let x = Int64.logor (wrap o x) (Int64.pred (Int64.shift_left 1L offset)) in
  if Int32.equal (high x) 0l
  then 32 + Stdlib.Int32.leading_zeros (low x)
  else Stdlib.Int32.leading_zeros (high x)

let trailing_zeros x =
  if is_zero x
  then num_bits ()
  else if Int32.equal (low x) 0l
  then 32 + Stdlib.Int32.trailing_zeros (high x)
  else Stdlib.Int32.trailing_zeros (low x)

let popcount x =
  let x = wrap (offset ()) x in
  Stdlib.Int32.popcount (high x) + Stdlib.Int32.popcount (low x)

let of_int_exn (x : int) =
  let offset = offset () in
  let x' = Int64.of_int x in
  if min_int_ offset <= x' && x' <= max_int_ offset
  then x'
  else failwith (Printf.sprintf "of_int_exn(%d)" x)

let of_int32_exn (x : int32) =
  let offset = offset () in
  let x = Int64.of_int32 x in
  if min_int_ offset <= x && x <= max_int_ offset then x else failwith "of_int32_exn"

let of_int32_truncate x = wrap_modulo (Int64.of_int32 x)

let of_int64_truncate = wrap_modulo

let of_string_exn x =
  try
    let offset = offset () in
    let x64 =
      if num_bits () <= 32
      then (* As [Int32.of_string]: unsigned 32 bit hexadecimal numbers *)
        Int64.of_int32 (Int32.of_string x)
      else Int64.of_string x
    in
    if min_int_ offset <= x64 && x64 <= max_int_ offset then x64 else raise Not_found
  with Not_found | _ -> failwith (Printf.sprintf "Targetint.of_string_exn(%s)" x)

let of_float_opt x =
  let offset = offset () in
  if Int64.to_float (min_int_ offset) <= x && x <= Int64.to_float (max_int_ offset)
  then Some (wrap_modulo (Int64.of_float x))
  else None

let convert_warning_on_overflow name ~to_t ~equal ~to_dec ~to_hex x =
  let i = to_t x in
  if not (equal i x)
  then
    Warning.warn
      `Integer_overflow
      "%s 0x%s (%s) truncated to 0x%Lx (%Ld); the generated code might be incorrect.@."
      name
      (to_hex x)
      (to_dec x)
      i
      i;
  i

let of_int_warning_on_overflow i =
  convert_warning_on_overflow
    "integer"
    ~to_t:(fun i -> wrap_modulo (Int64.of_int i))
    ~equal:(fun i' i -> Int64.equal i' (Int64.of_int i))
    ~to_dec:(Printf.sprintf "%d")
    ~to_hex:(Printf.sprintf "%x")
    i

let of_int32_warning_on_overflow n =
  convert_warning_on_overflow
    "int32"
    ~to_t:(fun i -> wrap_modulo (Int64.of_int32 i))
    ~equal:(fun i' i -> Int64.equal i' (Int64.of_int32 i))
    ~to_dec:(Printf.sprintf "%ld")
    ~to_hex:(Printf.sprintf "%lx")
    n

let of_nativeint_warning_on_overflow n =
  convert_warning_on_overflow
    "native integer"
    ~to_t:(fun i -> wrap_modulo (Int64.of_nativeint i))
    ~equal:(fun i' i -> Int64.equal i' (Int64.of_nativeint i))
    ~to_dec:(Printf.sprintf "%nd")
    ~to_hex:(Printf.sprintf "%nx")
    n

external ( < ) : int64 -> int64 -> bool = "%lessthan"

external ( <= ) : int64 -> int64 -> bool = "%lessequal"

external ( <> ) : int64 -> int64 -> bool = "%notequal"

external ( = ) : int64 -> int64 -> bool = "%equal"

external ( > ) : int64 -> int64 -> bool = "%greaterthan"

external ( >= ) : int64 -> int64 -> bool = "%greaterequal"

let unsigned_lt n m = Int64.(sub n min_int < sub m min_int)
