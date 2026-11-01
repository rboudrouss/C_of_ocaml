(* The 16, 32 and 64-bit accesses to bytes and strings, which c_of_ocaml
   inlines on unboxed integers: values, bounds, exceptions caught by the
   right handler, results kept across allocations *)

let b = Bytes.init 16 (fun i -> Char.chr ((i * 37 + 200) land 255))
let s = Bytes.to_string b

let show name f =
  let r = try f () with Invalid_argument m -> "Invalid_argument " ^ m in
  print_endline (name ^ ": " ^ r)

let () =
  for i = -2 to 17 do
    let p = string_of_int i in
    show ("u16le " ^ p) (fun () -> string_of_int (Bytes.get_uint16_le b i));
    show ("i16be " ^ p) (fun () -> string_of_int (Bytes.get_int16_be b i));
    show ("s16ne " ^ p) (fun () -> string_of_int (String.get_int16_ne s i));
    show ("i32le " ^ p) (fun () -> Int32.to_string (Bytes.get_int32_le b i));
    show ("s32be " ^ p) (fun () -> Int32.to_string (String.get_int32_be s i));
    show ("i64le " ^ p) (fun () -> Int64.to_string (Bytes.get_int64_le b i));
    show ("s64be " ^ p) (fun () -> Int64.to_string (String.get_int64_be s i))
  done

let () =
  let c = Bytes.make 12 '\000' in
  for i = -1 to 12 do
    let p = string_of_int i in
    show ("set16 " ^ p) (fun () ->
        Bytes.set_int16_le c i (-2);
        Bytes.to_string c |> String.escaped);
    show ("set32 " ^ p) (fun () ->
        Bytes.set_int32_be c i (Int32.of_int (0x12345678 + i));
        Bytes.to_string c |> String.escaped);
    show ("set64 " ^ p) (fun () ->
        Bytes.set_int64_le c i (Int64.neg (Int64.of_int (i + 1)));
        Bytes.to_string c |> String.escaped)
  done

(* Unboxed results live across allocations and calls, then stored *)
let () =
  let acc32 = ref 0l and acc64 = ref 0L and l = ref [] in
  for r = 1 to 2000 do
    for i = 0 to Bytes.length b - 8 do
      let x = Bytes.get_int32_le b i and y = Bytes.get_int64_be b i in
      l := (r, i) :: !l;
      if List.length !l > 100 then l := [];
      acc32 := Int32.add !acc32 (Int32.mul x (Int32.of_int r));
      acc64 := Int64.logxor !acc64 (Int64.add y (Int64.of_int32 x));
      Bytes.set_int32_le b i (Int32.add x 1l);
      Bytes.set_int64_be b (i land 7) (Int64.sub y 1L)
    done
  done;
  Printf.printf "%ld %Ld %d\n" !acc32 !acc64 (List.length !l);
  let boxed = Array.init 8 (fun i -> Bytes.get_int64_le b i) in
  Gc.full_major ();
  Array.iter (fun x -> print_endline (Int64.to_string x)) boxed

(* A handler around a loop: the exception leaves the loop *)
let () =
  let n = ref 0 in
  (try
     while true do
       ignore (String.get_int64_le s !n);
       incr n
     done
   with Invalid_argument _ -> ());
  Printf.printf "stopped at %d\n" !n
