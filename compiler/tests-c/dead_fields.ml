(* The field y of the records is never read: the dead code elimination puts
   its sentinel, an integer, in place of the field, in a float block. The C
   code must store some float there, not read the sentinel as a boxed float
   (which crashes natively) *)
type p =
  { x : float
  ; y : float
  ; z : float
  }

let theta p = atan2 p.x p.z

let f a b c = theta { x = a *. b; y = a +. c; z = c }

let g a b c =
  theta { x = a; y = b; z = c }
  +. theta { x = c; y = b; z = a }
  +. theta { x = b; y = c; z = a }

let () =
  let o = Sys.opaque_identity in
  Printf.printf "%.4f %.4f\n" (f (o 1.5) (o 2.) (o 3.)) (g (o 1.) (o 2.) (o 3.))
