(* An application whose result is applied right after, fused into one
   application (compiler/lib/apply_fusion.ml): the same results and the
   same order of effects *)
let opaque = Sys.opaque_identity

(* Unknown functions (ocamlc drops Sys.opaque_identity) *)
let hide (f : 'a) : 'a = Obj.obj (Obj.dup (Obj.repr f))

let log = Buffer.create 64

let trace s = Buffer.add_string log s

(* Arity 2, 1 then 1 *)
let add = hide (fun a b -> a + b)

(* Arity 1, returning a closure after an effect *)
let staged =
  hide (fun a ->
      trace "s";
      fun b ->
        trace "t";
        a * b)

(* Arity 3, partially applied twice *)
let three = hide (fun a b c -> (a * 100) + (b * 10) + c)

(* An exception raised by the first application *)
let failing = hide (fun a -> if a = 0 then raise Not_found else fun b -> a + b)

let () =
  let s = ref 0 in
  for i = 1 to 1000 do
    let p = add i in
    s := p !s land 0xffff
  done;
  let q = staged 6 in
  let r = q 7 in
  let p3 = three 1 in
  let p32 = p3 2 in
  let r3 = p32 3 in
  let e =
    match
      let p = failing (opaque 0) in
      p 5
    with
    | n -> n
    | exception Not_found -> -1
  in
  let shared = add 1 in
  let u = shared 2 + shared 3 in
  Printf.printf "%d %d %d %d %d %s\n" !s r r3 e u (Buffer.contents log)
