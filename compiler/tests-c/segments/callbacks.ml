(* A recursion through a C function, which has a frame on the C stack of
   emscripten (64 KB in tools/c_of_ocaml_segments_test.sh, checked): it
   raises Stack_overflow before the stack is exhausted *)
external through_c : (int -> int) -> int -> int = "callbacks_through_c"

let rec down n = if n = 0 then 0 else 1 + through_c down (n - 1)

let () =
  Printf.printf "down %d\n" (down 50);
  (match down 100_000 with
  | n -> Printf.printf "down %d\n" n
  | exception Stack_overflow -> print_endline "Stack_overflow");
  Printf.printf "down %d\n" (down 50)
