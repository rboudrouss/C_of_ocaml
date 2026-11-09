(* Partial applications of a function of more than 8 parameters in CPS
   (camlc_pap_cpsN), under an effect handler: a loop of tail calls *)
type _ Effect.t += E : int Effect.t

let h = if Array.length Sys.argv > 5 then succ else fun x -> x

let pap = ref (fun _ -> 0)

let rec p9 a b c d e f g i n =
  if n = 0 then a + b + c + d + e + f + g + i + Effect.perform E else !pap (n - 1)

let () = pap := (Obj.obj (Obj.dup (Obj.repr p9)) : _ -> _) 1 2 3 4 5 6 7 (h 8)

let () =
  let open Effect.Deep in
  Printf.printf
    "%d\n"
    (match_with
       (fun () -> !pap 10_000_000)
       ()
       { retc = (fun x -> x)
       ; exnc = raise
       ; effc =
           (fun (type a) (e : a Effect.t) ->
             match e with
             | E -> Some (fun (k : (a, _) continuation) -> continue k 100)
             | _ -> None)
       })
