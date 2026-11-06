(* Numbers given to the primitives reading values through Obj: they are
   boxed (Rocq's CObj computes [Obj.size (Obj.repr 1.0)]) *)

(* A float is a word, or two in 32 bits *)
let words n = n * Sys.word_size / 64

let size_of_double = words (Obj.size (Obj.repr 1.0))

let is_int = Obj.is_int (Obj.repr 2.5)

let f x = words (Obj.size (Obj.repr (x +. 1.0)))

let g x = Obj.is_int (Obj.repr (Int64.add x 1L))

let () =
  Printf.printf
    "%d %b %d %b\n"
    size_of_double
    is_int
    (f (float_of_string "3"))
    (g (Int64.of_string "4"))
