(* Marshal.Closures *)

let copy ?(flags = []) (v : 'a) : 'a =
  Marshal.from_string (Marshal.to_string v (Marshal.Closures :: flags)) 0

let top x = x + 1

type t =
  | Leaf
  | Node of t * (int -> int) * t

(* Without closures, the output of the marshaler of OCaml *)
let same ?(cyclic = false) v =
  let same flags =
    String.equal (Marshal.to_string v flags) (Marshal.to_string v (Marshal.Closures :: flags))
  in
  Printf.printf "%b %b\n" (same []) (cyclic || same [ Marshal.No_sharing ])

let () =
  let s = String.make 300 'a' in
  let rec cycle = 1 :: 2 :: cycle in
  same
    ( [ 0; 1; 63; 64; -1; 127; 128; -129; 32767; 32768; -32769; 1 lsl 29; -(1 lsl 29) ]
    , [ ""; "a"; String.make 31 'b'; String.make 32 'c'; s; s ]
    , [| 1.5; -0. |]
    , (Array.make 300 2.5, 3.25, [ 1.5; 1.5 ])
    , (1L, -123456789012345L, 7l, -7l, 5n, Int64.min_int)
    , (Some (Some [||]), [| [||] |], Array.make 1000 'x', Array.init 20 Fun.id)
    , lazy 3
    , Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [| 1l; 2l |] );
  same ~cyclic:true (cycle, ref cycle);
  if Sys.word_size = 64 then same (1 lsl 40, -(1 lsl 40), Int64.max_int) else same 0

let () =
  let k = Sys.opaque_identity 3 in
  (* Static closures, and closures of arity 1 or more with an environment *)
  Printf.printf "%d\n" ((copy top) 4);
  Printf.printf "%d\n" ((copy (fun x -> x + k)) 4);
  let s = Sys.opaque_identity "hello" in
  Printf.printf "%d\n" ((copy (fun x y -> String.length s + x + y)) 1 2);
  let f = copy (fun a b c d e f g -> a + b + c + d + e + f + g + k) in
  Printf.printf "%d\n" (f 1 2 3 4 5 6 7);
  (* Partial applications *)
  let add3 a b c = a + b + c + k in
  Printf.printf "%d\n" ((copy (add3 1)) 2 3);
  Printf.printf "%d\n" ((copy (add3 1 2)) 3);
  Printf.printf "%d\n" ((copy (List.fold_left ( + ) 10)) [ 1; 2; 3 ]);
  (* Mutually recursive functions: Infix pointers *)
  let t = Sys.opaque_identity true and f = Sys.opaque_identity false in
  let rec even n = if n = 0 then t else odd (n - 1)
  and odd n = if n = 0 then f else even (n - 1) in
  let even', odd' = copy (even, odd) in
  Printf.printf "%b %b %b %b\n" (even' 10) (odd' 10) (even' 7) (odd' 7);
  let rec g2 a b = if a = 0 then b + k else h2 (a - 1) (b + 1)
  and h2 a b = if a = 0 then b - k else g2 (a - 1) (b * 2) in
  let g2', h2' = copy (g2, h2) in
  Printf.printf "%d %d\n" (g2' 5 1) (h2' 5 1);
  Printf.printf "%d\n" ((copy h2) 4 3);
  (* Floats, sharing, cycles *)
  let x = Sys.opaque_identity 1.5 in
  Printf.printf "%g\n" ((copy (fun y -> x *. y)) 4.);
  let c = fun y -> y * k in
  let c1, c2 = copy (c, c) in
  Printf.printf "%b %d\n" (c1 == c2) (c2 5);
  let c1, c2 = copy ~flags:[ Marshal.No_sharing ] (c, c) in
  Printf.printf "%b %d\n" (c1 == c2) (c2 5);
  let rec tree n = if n = 0 then Leaf else Node (tree (n - 1), (fun x -> x + n), tree (n - 1)) in
  let rec sum t x = match t with Leaf -> x | Node (l, f, r) -> sum r (f (sum l x)) in
  Printf.printf "%d\n" (sum (copy (tree 6)) 0);
  let r = ref (fun _ -> 0) in
  r := (fun n -> if n = 0 then k else 1 + !r (n - 1));
  let r' = copy r in
  Printf.printf "%d\n" (!r' 10);
  (* Objects *)
  let o = object val mutable v = k method get = v method set n = v <- n end in
  let o' = copy o in
  o'#set 7;
  Printf.printf "%d %d %b\n" o#get o'#get (Oo.id o <> Oo.id o');
  (* Custom blocks among closures *)
  let i = Sys.opaque_identity 123456789012L in
  Printf.printf "%Ld\n" ((copy (fun y -> Int64.add i y)) 1L);
  (* Bytes, buffers and channels *)
  let b = Marshal.to_bytes (fun x -> x - k) [ Marshal.Closures ] in
  Printf.printf "%d\n" ((Marshal.from_bytes b 0 : int -> int) 10);
  let buf = Bytes.create 1000 in
  let n = Marshal.to_buffer buf 10 990 (fun x -> x * x + k) [ Marshal.Closures ] in
  Printf.printf "%b %d\n" (n = Marshal.total_size buf 10) ((Marshal.from_bytes buf 10 : int -> int) 5);
  (try ignore (Marshal.to_buffer buf 0 10 (fun x -> x + k) [ Marshal.Closures ])
   with Failure s -> print_endline s);
  let file = Filename.temp_file "marshal" "" in
  let oc = open_out_bin file in
  Marshal.to_channel oc (fun x -> x + k) [ Marshal.Closures ];
  Marshal.to_channel oc [ 1; 2; 3 ] [];
  output_value oc (Some "x");
  close_out oc;
  let ic = open_in_bin file in
  let f = (Marshal.from_channel ic : int -> int) in
  let l = (Marshal.from_channel ic : int list) in
  let o = (input_value ic : string option) in
  close_in ic;
  Sys.remove file;
  Printf.printf "%d %d %s\n" (f 1) (List.length l) (Option.get o);
  (* Errors *)
  (try ignore (Marshal.to_string (fun x -> x + k) []) with Invalid_argument s -> print_endline s);
  (try ignore (Marshal.to_string (Lazy.from_fun (fun () -> k)) []) with Invalid_argument s -> print_endline s);
  Printf.printf "%d\n" (Lazy.force (copy (lazy (k + 1))))
