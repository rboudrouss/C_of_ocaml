(* A value no longer live, whose slot in the frame is not overwritten, is
   not kept alive by the frame when the GC runs *)
let w = Weak.create 1

let () =
  for round = 1 to 3 do
    let l = ref [] in
    for i = 1 to 10_000 do
      l := Array.make (i mod 50) i :: !l
    done;
    Weak.set w 0 (Some !l);
    let n = List.length (List.filter (fun a -> Array.length a > 10) !l) in
    l := [];
    Gc.full_major ();
    Printf.printf "%d %d %b\n" round n (Weak.check w 0)
  done
