(* The write barrier of the generated code (camlc_modify_major): pointers
   from the major heap to young blocks, and old values overwritten while
   the major GC marks, with a small minor heap and many collections *)
let () = Gc.set { (Gc.get ()) with minor_heap_size = 4096; space_overhead = 20 }

let n = 1024

let a = Array.make n []

let b = Array.init n (fun i -> ref i)

let () = Gc.full_major ()

let () =
  let sum = ref 0 in
  for i = 1 to 2_000_000 do
    let j = i land (n - 1) in
    a.(j) <- [ i; i + 1 ];
    b.(j) <- (if i land 3 = 0 then ref i else b.(j * 7 land (n - 1)));
    (match a.((j + 1) land (n - 1)) with
    | [ x; y ] -> if y <> x + 1 then failwith "corrupted"
    | [] -> ()
    | _ -> failwith "corrupted");
    sum := !sum + !(b.((j + 3) land (n - 1)));
    if i land 0xffff = 0 then Gc.major_slice 0 |> ignore
  done;
  Gc.full_major ();
  Array.iteri
    (fun j l ->
      match l with
      | [ x; y ] when y = x + 1 && x land (n - 1) = j -> ()
      | _ -> failwith "corrupted")
    a;
  Printf.printf "%d\n" (!sum land 0xffffff)
