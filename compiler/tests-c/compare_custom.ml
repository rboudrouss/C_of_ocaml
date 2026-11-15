(* Polymorphic comparisons of boxed integers (camlc_compare_val): the same
   results as the C primitives *)
let opaque = Sys.opaque_identity

let show name l =
  Printf.printf
    "%s: %s\n"
    name
    (String.concat
       " "
       (List.concat_map
          (fun (a, b) ->
            [ string_of_int (compare a b)
            ; string_of_bool (a = b)
            ; string_of_bool (a <> b)
            ; string_of_bool (a < b)
            ; string_of_bool (a <= b)
            ; string_of_bool (a > b)
            ; string_of_bool (a >= b)
            ])
          l))

let pairs l = List.concat_map (fun a -> List.map (fun b -> opaque a, opaque b) l) l

let () =
  show "int64" (pairs [ Int64.min_int; -1L; 0L; 1L; Int64.max_int ]);
  show "int32" (pairs [ Int32.min_int; -1l; 0l; 1l; Int32.max_int ]);
  show "nativeint" (pairs [ Nativeint.min_int; -1n; 0n; 1n; Nativeint.max_int ]);
  (* Inside structured blocks *)
  show "tuples" (pairs [ 1, 2L, "a"; 1, 2L, "b"; 1, 3L, "a"; 0, Int64.max_int, "" ]);
  show "lists" (pairs [ [ 1l; 2l ]; [ 1l ]; [ 1l; 3l ]; [] ]);
  (* Custom blocks of different operations: by the C primitive *)
  let r x = Obj.magic (Obj.repr x) in
  let a : int64 = r 5L and b : int64 = r 5l and c : int64 = r 5n in
  show "mixed" (pairs [ a; b; c ]);
  (* An integer and a custom block *)
  let d : int64 = r 3 in
  show "immediate" (pairs [ a; d ]);
  Printf.printf
    "sorted %s\n"
    (String.concat
       " "
       (List.map
          Int64.to_string
          (List.sort compare [ 3L; -7L; Int64.max_int; 0L; Int64.min_int; 3L ])))
