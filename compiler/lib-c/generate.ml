(* C_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Translation of the intermediate code into C, for the runtime described
   in runtime/c/camlc.h.

   Each function of the program becomes a C function. Its variables live
   in a frame on the shadow stack ([fp[i]]), so that they are seen and
   updated by the GC; the variables of the enclosing functions are read
   from the closure ([fp[0]]). Blocks are labels, and branches are gotos.
   An exception is returned as an exception result: each call is followed
   by a check, which jumps to the handler of the innermost [Pushtrap] of
   the function or returns the exception. *)

open! Stdlib
open Code

(* 4 on wasm32 (31-bit integers), 8 on wasm64 (63-bit integers) *)
let word_size () = if Targetint.num_bits () > 32 then 8 else 4

(* Effect handlers: functions which may perform an effect have a CPS
   version, run on fibers by the runtime (see camlc.h) *)
let double_translation () =
  match Config.effects () with
  | `Double_translation -> true
  | `Disabled | `Cps | `Jspi | `Native -> false

(****)

let sanitize s =
  String.map s ~f:(fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> c
      | _ -> '_')

let c_string s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter s ~f:(fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | ' ' .. '~' -> Buffer.add_char b c
      | _ -> Buffer.add_string b (Printf.sprintf "\\%03o" (Char.code c)));
  Buffer.add_char b '"';
  Buffer.contents b

let tagged_int i =
  (* The tagged representation fits in a word *)
  let i = Targetint.to_int64 i in
  let t = Int64.add (Int64.shift_left i 1) 1L in
  if word_size () = 8
  then Printf.sprintf "((value)0x%LxULL)" t
  else Printf.sprintf "((value)0x%lxu)" (Int64.to_int32 t)

(* The representation of a variable in C: OCaml values, or unboxed numbers
   (see {!Typing}) *)
type repr =
  | Value
  | Double
  | Unboxed_int64
  | Unboxed_int32
  | Unboxed_nativeint

let c_type r =
  match r with
  | Value -> "value"
  | Double -> "double"
  | Unboxed_int64 -> "int64_t"
  | Unboxed_int32 -> "int32_t"
  | Unboxed_nativeint -> "intnat"

let repr_of_type (t : Typing.typ) =
  match t with
  | Number (Float, Unboxed) -> Double
  | Number (Int64, Unboxed) -> Unboxed_int64
  | Number (Int32, Unboxed) -> Unboxed_int32
  | Number (Nativeint, Unboxed) -> Unboxed_nativeint
  | _ -> Value

let box r e =
  match r with
  | Value -> e
  | Double -> Printf.sprintf "camlc_box_double(%s)" e
  | Unboxed_int64 -> Printf.sprintf "caml_copy_int64(%s)" e
  | Unboxed_int32 -> Printf.sprintf "caml_copy_int32(%s)" e
  | Unboxed_nativeint -> Printf.sprintf "caml_copy_nativeint(%s)" e

let unbox r e =
  match r with
  | Value -> e
  | Double -> Printf.sprintf "Double_val(%s)" e
  | Unboxed_int64 -> Printf.sprintf "Int64_val(%s)" e
  | Unboxed_int32 -> Printf.sprintf "Int32_val(%s)" e
  | Unboxed_nativeint -> Printf.sprintf "Nativeint_val(%s)" e

let repr_equal (r : repr) r' = Poly.equal r r'

(****)

(* The output, and what the generated code refers to *)
type output =
  { data : Buffer.t  (** Initializers of the static data *)
  ; data_fields : Buffer.t  (** Fields of the structure holding the static data *)
  ; functions : Buffer.t  (** Function definitions *)
  ; mutable data_count : int
  ; constants : (constant * string) list Int.Hashtbl.t
        (** Static data already emitted for each constant *)
  ; primitives : int String.Hashtbl.t  (** C primitives called, with their arity *)
  ; mutable apply_arities : IntSet.t  (** Arities of the [camlc_applyN] functions used *)
  ; mutable max_arity : int  (** Maximal arity of the functions *)
  ; global_data_size : int
  ; fun_names : string Var.Hashtbl.t  (** C name of each function *)
  ; types : Typing.t
  ; fun_reprs : repr list Var.Hashtbl.t
        (** The representation of the parameters of each function *)
  ; debug_info : bool  (** Record backtraces *)
  ; locations : string String.Hashtbl.t  (** The C names of the locations *)
  ; location_defs : Buffer.t
  ; in_cps : Var.Set.t  (** The CPS calls *)
  ; trampolined : Var.Set.t  (** The CPS calls which may bounce *)
  ; cps_pairs : Var.t Var.Map.t
        (** The CPS version of each function with two versions, which share a
            closure *)
  ; cps_sides : Var.Set.t  (** The CPS versions *)
  ; mutable cps_apply_arities : IntSet.t
        (** Arities (with the continuation) of the [camlc_apply_cpsN] and
            [camlc_call_cpsN] functions used *)
  }

(* A static description of a source location, for backtraces *)
let location out (pi : Parse_info.t) ~fun_name ~is_raise =
  let file =
    match pi.name, pi.src with
    | Some n, _ | None, Some n -> n
    | None, None -> "_none_"
  in
  (* As in native code: the module, then the function *)
  let defname =
    let m =
      String.capitalize_ascii (Filename.remove_extension (Filename.basename file))
    in
    match fun_name with
    | Some f -> m ^ "." ^ f
    | None -> m
  in
  let key = Printf.sprintf "%s:%d:%d:%s:%b" file pi.line pi.col defname is_raise in
  match String.Hashtbl.find_opt out.locations key with
  | Some n -> "&" ^ n
  | None ->
      let n = Printf.sprintf "camlc_l%d" (String.Hashtbl.length out.locations) in
      String.Hashtbl.add out.locations key n;
      Printf.bprintf
        out.location_defs
        "static const struct camlc_location %s = { %s, %s, %d, %d, %d, %d };\n"
        n
        (c_string file)
        (c_string defname)
        pi.line
        pi.col
        pi.col
        (if is_raise then 1 else 0);
      "&" ^ n

let fun_name out f =
  match Var.Hashtbl.find_opt out.fun_names f with
  | Some n -> n
  | None ->
      let n =
        match Var.get_name f with
        | Some s -> Printf.sprintf "camlc_f%d_%s" (Var.idx f) (sanitize s)
        | None -> Printf.sprintf "camlc_f%d" (Var.idx f)
      in
      Var.Hashtbl.add out.fun_names f n;
      n

let fun_type arity =
  Printf.sprintf
    "value (*)(%s)"
    (String.concat ~sep:", " (List.init ~len:(arity + 1) ~f:(fun _ -> "value")))

let fun_params arity =
  String.concat
    ~sep:", "
    (List.init ~len:arity ~f:(fun i -> Printf.sprintf "value a%d" i) @ [ "value env" ])

(****)

(* Static data *)

let header ~wosize ~tag =
  Printf.sprintf "Make_header(%d, %d, CAMLC_STATIC_COLOR)" wosize tag

let words_of_bytes s =
  (* Little-endian words of the string [s] followed by the padding of
     OCaml strings *)
  let word_size = word_size () in
  let len = String.length s in
  let wosize = (len / word_size) + 1 in
  let bytes = Bytes.make (wosize * word_size) '\000' in
  Bytes.blit_string ~src:s ~src_pos:0 ~dst:bytes ~dst_pos:0 ~len;
  Bytes.set bytes ((wosize * word_size) - 1) (Char.chr ((wosize * word_size) - 1 - len));
  List.init ~len:wosize ~f:(fun i ->
      let w = ref 0L in
      for j = word_size - 1 downto 0 do
        w :=
          Int64.logor
            (Int64.shift_left !w 8)
            (Int64.of_int (Char.code (Bytes.get bytes ((i * word_size) + j))))
      done;
      Printf.sprintf "(value)0x%LxULL" !w)

(* The words of a 64-bit number *)
let int64_words i =
  if word_size () = 8
  then [ Printf.sprintf "(value)0x%LxULL" i ]
  else
    [ Printf.sprintf "(value)0x%Lxu" (Int64.logand i 0xffffffffL)
    ; Printf.sprintf "(value)0x%Lxu" (Int64.shift_right_logical i 32)
    ]

let int32_word i = Printf.sprintf "(value)0x%lxu" i

(* The static data is a single structure, so that the runtime can tell
   where it is (OCaml 4 needs it in its page table). [static_block out
   words] adds a block (header and fields) to it, and returns its C name. *)
let static_block out words =
  let n = out.data_count in
  out.data_count <- n + 1;
  Printf.bprintf out.data_fields "  value d%d[%d];\n" n (List.length words);
  Printf.bprintf out.data "  .d%d = { %s },\n" n (String.concat ~sep:", " words);
  Printf.sprintf "camlc_data.d%d" n

let emit_block out ~tag fields =
  let name = static_block out (header ~wosize:(List.length fields) ~tag :: fields) in
  Printf.sprintf "((value)&%s[1])" name

let rec constant out c =
  match c with
  | Int i -> tagged_int i
  | _ -> (
      let h = Hashtbl.hash c in
      let l = Int.Hashtbl.find_opt out.constants h |> Option.value ~default:[] in
      match List.find_opt l ~f:(fun (c', _) -> Constant.equal c c') with
      | Some (_, e) -> e
      | None ->
          let e = block_constant out c in
          Int.Hashtbl.replace out.constants h ((c, e) :: l);
          e)

and block_constant out c =
  match c with
  | Int _ -> assert false
  | String s | NativeString (Byte s) | NativeString (Utf (Utf8 s)) ->
      emit_block out ~tag:Obj.string_tag (words_of_bytes s)
  | Float f -> emit_block out ~tag:Obj.double_tag (int64_words f)
  | Float_array (a, _) ->
      emit_block
        out
        ~tag:Obj.double_array_tag
        (List.concat_map (Array.to_list a) ~f:int64_words)
  | Int64 i ->
      emit_block out ~tag:Obj.custom_tag ("(value)&caml_int64_ops" :: int64_words i)
  | Int32 i ->
      emit_block out ~tag:Obj.custom_tag [ "(value)&caml_int32_ops"; int32_word i ]
  | NativeInt i ->
      emit_block
        out
        ~tag:Obj.custom_tag
        [ "(value)&caml_nativeint_ops"
        ; (if word_size () = 8
           then Printf.sprintf "(value)0x%LxULL" i
           else int32_word (Int64.to_int32 i))
        ]
  | Tuple (tag, a, _, _) ->
      let fields = Array.to_list (Array.map a ~f:(fun c -> constant out c)) in
      emit_block out ~tag fields
  | Float32 _ -> failwith "c_of_ocaml: float32 constants are not supported"
  | Null_ -> failwith "c_of_ocaml: null constants are not supported"

(* A constant used in the code (rather than in other static data) *)
let code_constant out c =
  match c with
  | Tuple (tag, [||], _, _) -> Printf.sprintf "Atom(%d)" tag
  | _ -> constant out c

(****)

(* Primitives *)

type prim =
  | Plain of repr * string * bool
      (** A C expression of this representation, which cannot raise, and
          whether it may trigger a GC. It may allocate only if it is a single
          call whose arguments are the arguments of the primitive *)
  | Checked of string
      (** A single call whose arguments are the arguments of the primitive,
          returning a value or an exception result *)
  | Ccallv  (** A C primitive with more than 5 arguments *)

(* An argument of a primitive: its type, and its C expression in a given
   representation *)
type parg =
  { a_typ : Typing.typ
  ; get : repr -> string
  }

let c_call f args = Printf.sprintf "%s(%s)" f (String.concat ~sep:", " args)

(* C primitives which do not raise: they are called directly *)
let noraise_primitives =
  StringSet.of_list
    [ "caml_fmod_float"
    ; "caml_power_float"
    ; "caml_classify_float"
    ; "caml_string_equal"
    ; "caml_string_notequal"
    ; "caml_string_compare"
    ; "caml_bytes_equal"
    ; "caml_bytes_compare"
    ; "caml_blit_string"
    ; "caml_blit_bytes"
    ; "caml_fill_bytes"
    ; "caml_create_bytes"
    ; "caml_obj_dup"
    ; "caml_array_unsafe_set"
    ]

let float_binops =
  [ "caml_add_float", "+"
  ; "caml_sub_float", "-"
  ; "caml_mul_float", "*"
  ; "caml_div_float", "/"
  ]

let float_comparisons =
  [ "caml_eq_float", "=="
  ; "caml_neq_float", "!="
  ; "caml_lt_float", "<"
  ; "caml_le_float", "<="
  ; "caml_gt_float", ">"
  ; "caml_ge_float", ">="
  ]

let float_functions =
  [ "caml_sqrt_float", "sqrt"
  ; "caml_exp_float", "exp"
  ; "caml_log_float", "log"
  ; "caml_sin_float", "sin"
  ; "caml_cos_float", "cos"
  ; "caml_tan_float", "tan"
  ; "caml_asin_float", "asin"
  ; "caml_acos_float", "acos"
  ; "caml_atan_float", "atan"
  ; "caml_floor_float", "floor"
  ; "caml_ceil_float", "ceil"
  ; "caml_abs_float", "fabs"
  ; "caml_trunc_float", "trunc"
  ]

(* The boxed integers, with the C type of their unboxed representation *)
let boxed_ints =
  [ "int64", Unboxed_int64; "int32", Unboxed_int32; "nativeint", Unboxed_nativeint ]

(* Arithmetic on boxed integers, computed on unsigned integers to wrap
   around *)
let boxed_int_binops =
  [ "add", "+"; "sub", "-"; "mul", "*"; "and", "&"; "or", "|"; "xor", "^" ]

let unsigned_type r =
  match r with
  | Unboxed_int64 -> "uint64_t"
  | Unboxed_int32 -> "uint32_t"
  | Unboxed_nativeint -> "uintnat"
  | Value | Double -> assert false

let bits r =
  match r with
  | Unboxed_int64 -> 64
  | Unboxed_int32 -> 32
  | Unboxed_nativeint -> 8 * word_size ()
  | Value | Double -> assert false

(* Polymorphic comparisons specialized to integers or numbers, as allowed
   by {!Typing.type_specialized_primitive} *)
let comparison_operator name =
  match name with
  | "caml_equal" -> Some "=="
  | "caml_notequal" -> Some "!="
  | "caml_lessthan" -> Some "<"
  | "caml_lessequal" -> Some "<="
  | "caml_greaterthan" -> Some ">"
  | "caml_greaterequal" -> Some ">="
  | "caml_compare" -> Some "compare"
  | _ -> None

let specialized_comparison name args =
  match comparison_operator name, args with
  | Some op, [ a; b ] -> (
      match a.a_typ, b.a_typ with
      | Int _, Int _ -> Some (op, `Int)
      | Number (n, _), Number (n', _) when Poly.equal n n' -> (
          match n with
          | Float -> Some (op, `Number Double)
          | Int64 -> Some (op, `Number Unboxed_int64)
          | Int32 -> Some (op, `Number Unboxed_int32)
          | Nativeint -> Some (op, `Number Unboxed_nativeint)
          | Float32 -> None)
      | _ -> None)
  | _ -> None

(* Primitives which never trigger a garbage collection *)
let nogc_primitives =
  StringSet.of_list
    [ "%identity"
    ; "caml_bytes_of_string"
    ; "caml_string_of_bytes"
    ; "%int_add"
    ; "%int_sub"
    ; "%int_mul"
    ; "%direct_int_mul"
    ; "%int_neg"
    ; "%int_and"
    ; "%int_or"
    ; "%int_xor"
    ; "%int_lsl"
    ; "%int_lsr"
    ; "%int_asr"
    ; "%direct_int_div"
    ; "%direct_int_mod"
    ; "%int_div"
    ; "%int_mod"
    ; "caml_int_compare"
    ; "%direct_obj_tag"
    ; "caml_ml_string_length"
    ; "caml_ml_bytes_length"
    ; "caml_string_unsafe_get"
    ; "caml_bytes_unsafe_get"
    ; "caml_string_unsafe_set"
    ; "caml_bytes_unsafe_set"
    ; "caml_array_unsafe_set_addr"
    ; "caml_register_global"
    ; "caml_register_global_predef"
    ; "caml_register_global_by_index"
    ; "caml_get_global_data"
    ; "caml_oo_cache_id"
    ; "caml_get_cached_method"
    ; "caml_eq_float"
    ; "caml_neq_float"
    ; "caml_lt_float"
    ; "caml_le_float"
    ; "caml_gt_float"
    ; "caml_ge_float"
    ; "caml_int_of_float"
    ; "caml_float_compare"
    ]

let primitive out name (args : parg list) =
  let n = List.length args in
  let arg r i = (List.nth args i).get r in
  let value i = arg Value i in
  let lookup l =
    List.find_map l ~f:(fun (n, op) -> if String.equal n name then Some op else None)
  in
  let boxed_int_op =
    List.find_map boxed_ints ~f:(fun (t, r) ->
        let p = "caml_" ^ t ^ "_" in
        if
          String.length name > String.length p
          && String.equal (String.sub name ~pos:0 ~len:(String.length p)) p
        then
          Some
            ( r
            , String.sub
                name
                ~pos:(String.length p)
                ~len:(String.length name - String.length p) )
        else None)
  in
  let pure (r, e) = Plain (r, e, false) in
  let boxed_int_of name =
    List.find_map boxed_ints ~f:(fun (t, r) ->
        if String.equal t name then Some r else None)
  in
  match lookup float_binops, lookup float_comparisons, lookup float_functions, n with
  | Some op, _, _, 2 ->
      pure (Double, Printf.sprintf "(%s %s %s)" (arg Double 0) op (arg Double 1))
  | _, Some op, _, 2 ->
      pure (Value, Printf.sprintf "Val_bool(%s %s %s)" (arg Double 0) op (arg Double 1))
  | _, _, Some f, 1 -> pure (Double, Printf.sprintf "%s(%s)" f (arg Double 0))
  | _ -> (
      match specialized_comparison name args, boxed_int_op with
      | Some ("compare", `Int), _ ->
          pure (Value, c_call "Camlc_int_compare" [ value 0; value 1 ])
      | Some (op, `Int), _ ->
          pure
            ( Value
            , Printf.sprintf "Val_bool((intnat)%s %s (intnat)%s)" (value 0) op (value 1)
            )
      | Some ("compare", `Number Double), _ ->
          pure (Value, c_call "camlc_float_compare_unboxed" [ arg Double 0; arg Double 1 ])
      | Some ("compare", `Number r), _ ->
          pure
            ( Value
            , Printf.sprintf
                "Val_int((%s > %s) - (%s < %s))"
                (arg r 0)
                (arg r 1)
                (arg r 0)
                (arg r 1) )
      | Some (op, `Number r), _ ->
          pure (Value, Printf.sprintf "Val_bool(%s %s %s)" (arg r 0) op (arg r 1))
      | None, Some (r, op)
        when n = 2 && List.exists boxed_int_binops ~f:(fun (o, _) -> String.equal o op) ->
          let u = unsigned_type r in
          pure
            ( r
            , Printf.sprintf
                "(%s)((%s)%s %s (%s)%s)"
                (c_type r)
                u
                (arg r 0)
                (List.find_map boxed_int_binops ~f:(fun (o, c) ->
                     if String.equal o op then Some c else None)
                |> Option.get)
                u
                (arg r 1) )
      | None, Some (r, "neg") when n = 1 ->
          pure
            (r, Printf.sprintf "(%s)(0 - (%s)%s)" (c_type r) (unsigned_type r) (arg r 0))
      | None, Some (r, "shift_left") when n = 2 ->
          pure
            ( r
            , Printf.sprintf
                "(%s)((%s)%s << (Long_val(%s) & %d))"
                (c_type r)
                (unsigned_type r)
                (arg r 0)
                (value 1)
                (bits r - 1) )
      | None, Some (r, "shift_right") when n = 2 ->
          pure
            ( r
            , Printf.sprintf "(%s >> (Long_val(%s) & %d))" (arg r 0) (value 1) (bits r - 1)
            )
      | None, Some (r, "shift_right_unsigned") when n = 2 ->
          pure
            ( r
            , Printf.sprintf
                "(%s)((%s)%s >> (Long_val(%s) & %d))"
                (c_type r)
                (unsigned_type r)
                (arg r 0)
                (value 1)
                (bits r - 1) )
      | None, Some (r, "of_int") when n = 1 ->
          pure (r, Printf.sprintf "(%s)Long_val(%s)" (c_type r) (value 0))
      | None, Some (r, "to_int") when n = 1 ->
          pure (Value, Printf.sprintf "Val_long((intnat)%s)" (arg r 0))
      | None, Some (r, "of_float") when n = 1 ->
          pure (r, Printf.sprintf "(%s)%s" (c_type r) (arg Double 0))
      | None, Some (r, "to_float") when n = 1 ->
          pure (Double, Printf.sprintf "(double)%s" (arg r 0))
      | None, Some (r, "compare") when n = 2 ->
          pure
            ( Value
            , Printf.sprintf
                "Val_int((%s > %s) - (%s < %s))"
                (arg r 0)
                (arg r 1)
                (arg r 0)
                (arg r 1) )
      | None, Some (r, conv)
        when n = 1
             && String.length conv > 3
             && Option.is_some
                  (boxed_int_of (String.sub conv ~pos:3 ~len:(String.length conv - 3)))
        -> (
          let other =
            Option.get
              (boxed_int_of (String.sub conv ~pos:3 ~len:(String.length conv - 3)))
          in
          match String.sub conv ~pos:0 ~len:3 with
          | "of_" -> pure (r, Printf.sprintf "(%s)%s" (c_type r) (arg other 0))
          | "to_" -> pure (other, Printf.sprintf "(%s)%s" (c_type other) (arg r 0))
          | _ -> assert false)
      | None, Some (Unboxed_int64, "bits_of_float") when n = 1 ->
          pure (Unboxed_int64, Printf.sprintf "camlc_bits_of_double(%s)" (arg Double 0))
      | None, Some (Unboxed_int64, "float_of_bits") when n = 1 ->
          pure (Double, Printf.sprintf "camlc_double_of_bits(%s)" (arg Unboxed_int64 0))
      | _ -> (
          match name, n with
          | "caml_neg_float", 1 -> pure (Double, Printf.sprintf "(-%s)" (arg Double 0))
          | "caml_float_of_int", 1 ->
              pure (Double, Printf.sprintf "(double)Long_val(%s)" (value 0))
          | "caml_int_of_float", 1 ->
              pure (Value, Printf.sprintf "Val_long((intnat)%s)" (arg Double 0))
          | "caml_float_compare", 2 ->
              pure
                ( Value
                , c_call "camlc_float_compare_unboxed" [ arg Double 0; arg Double 1 ] )
          | "caml_floatarray_unsafe_get", 2 ->
              pure
                ( Double
                , Printf.sprintf "Double_flat_field(%s, Long_val(%s))" (value 0) (value 1)
                )
          | "caml_floatarray_unsafe_set", 3 ->
              pure
                ( Value
                , c_call "camlc_store_double_flat" [ value 0; value 1; arg Double 2 ] )
          | "%identity", 1 -> pure (Value, value 0)
          | ("caml_bytes_of_string" | "caml_string_of_bytes"), 1 -> pure (Value, value 0)
          | _ -> (
              let args = List.map args ~f:(fun a -> a.get Value) in
              let arg i = List.nth args i in
              let plain e = Plain (Value, e, not (StringSet.mem name nogc_primitives)) in
              match name, n with
              | "%int_add", 2 -> plain (c_call "Camlc_int_add" args)
              | "%int_sub", 2 -> plain (c_call "Camlc_int_sub" args)
              | ("%int_mul" | "%direct_int_mul"), 2 -> plain (c_call "Camlc_int_mul" args)
              | "%int_neg", 1 -> plain (c_call "Camlc_int_neg" args)
              | "%int_and", 2 -> plain (c_call "Camlc_int_and" args)
              | "%int_or", 2 -> plain (c_call "Camlc_int_or" args)
              | "%int_xor", 2 -> plain (c_call "Camlc_int_xor" args)
              | "%int_lsl", 2 -> plain (c_call "Camlc_int_lsl" args)
              | "%int_lsr", 2 -> plain (c_call "Camlc_int_lsr" args)
              | "%int_asr", 2 -> plain (c_call "Camlc_int_asr" args)
              | "%direct_int_div", 2 -> plain (c_call "Camlc_int_div_unsafe" args)
              | "%direct_int_mod", 2 -> plain (c_call "Camlc_int_mod_unsafe" args)
              | "%int_div", 2 -> Checked (c_call "camlc_int_div" args)
              | "%int_mod", 2 -> Checked (c_call "camlc_int_mod" args)
              | "caml_int_compare", 2 -> plain (c_call "Camlc_int_compare" args)
              | "%direct_obj_tag", 1 ->
                  plain (Printf.sprintf "Val_long(Tag_val(%s))" (arg 0))
              | ("caml_ml_string_length" | "caml_ml_bytes_length"), 1 ->
                  plain (Printf.sprintf "Val_long(caml_string_length(%s))" (arg 0))
              | ("caml_string_unsafe_get" | "caml_bytes_unsafe_get"), 2 ->
                  plain
                    (Printf.sprintf "Val_int(Byte_u(%s, Long_val(%s)))" (arg 0) (arg 1))
              | ("caml_string_unsafe_set" | "caml_bytes_unsafe_set"), 3 ->
                  plain
                    (Printf.sprintf
                       "(Byte_u(%s, Long_val(%s)) = (unsigned char)Long_val(%s), \
                        Val_unit)"
                       (arg 0)
                       (arg 1)
                       (arg 2))
              | ("caml_string_get" | "caml_bytes_get"), 2 ->
                  Checked (c_call "camlc_string_get" args)
              | ("caml_string_set" | "caml_bytes_set"), 3 ->
                  Checked (c_call "camlc_bytes_set" args)
              | "caml_check_bound", 2 -> Checked (c_call "camlc_check_bound" args)
              | "caml_check_bound_float", 2 ->
                  Checked (c_call "camlc_check_bound_float" args)
              | "caml_check_bound_gen", 2 -> Checked (c_call "camlc_check_bound_gen" args)
              | "caml_array_unsafe_get", 2 -> plain (c_call "camlc_array_unsafe_get" args)
              | "caml_array_unsafe_set_addr", 3 ->
                  plain
                    (Printf.sprintf
                       "(camlc_modify(&Field(%s, Long_val(%s)), %s), Val_unit)"
                       (arg 0)
                       (arg 1)
                       (arg 2))
              | "caml_string_concat", 2 -> plain (c_call "camlc_string_concat" args)
              | ("caml_register_global" | "caml_register_global_predef"), _ ->
                  plain "Val_unit"
              | "caml_register_global_by_index", 2 ->
                  plain
                    (Printf.sprintf
                       "(caml_modify(&Field(caml_global_data, Long_val(%s)), %s), \
                        Val_unit)"
                       (arg 1)
                       (arg 0))
              | "caml_get_global_data", 0 -> plain "caml_global_data"
              | "caml_oo_cache_id", 0 -> plain "camlc_oo_cache_id()"
              | "caml_get_cached_method", 3 ->
                  plain (c_call "camlc_get_cached_method" args)
              | ( ( "%perform"
                  | "%reperform"
                  | "%resume"
                  | "%with_stack"
                  | "%with_stack_bind" )
                , _ ) -> Checked "camlc_raise_unhandled_effect()"
              (* Effect handlers (see [Effects]) *)
              | "caml_perform_effect", 2 when double_translation () ->
                  Checked (c_call "camlc_perform_effect" args)
              | "caml_reperform_effect", 4 when double_translation () ->
                  Checked (c_call "camlc_reperform_effect" args)
              | "caml_resume_stack", 3 when double_translation () ->
                  Checked (c_call "camlc_resume_stack" args)
              | "caml_resume", 4 when double_translation () ->
                  Checked (c_call "camlc_resume" args)
              | "caml_raise_unhandled", 1 when double_translation () ->
                  Checked (c_call "camlc_raise_unhandled" args)
              | "caml_alloc_stack", 3 when double_translation () ->
                  plain (c_call "camlc_alloc_stack" args)
              | "caml_push_trap", 1 when double_translation () ->
                  plain (c_call "camlc_push_trap" args)
              | "caml_pop_trap", 0 when double_translation () -> plain "camlc_pop_trap()"
              | "caml_maybe_attach_backtrace", 2 when double_translation () ->
                  Plain (Value, arg 0, false)
              | "caml_continuation_use_noexc", 1 when double_translation () ->
                  plain (c_call "camlc_continuation_use_noexc" args)
              | "caml_continuation_use_and_update_handler_noexc", 4
                when double_translation () ->
                  plain (c_call "camlc_continuation_use_and_update_handler_noexc" args)
              | _ ->
                  if String.length name > 0 && Char.equal name.[0] '%'
                  then
                    failwith (Printf.sprintf "c_of_ocaml: unsupported primitive %s" name);
                  (match String.Hashtbl.find_opt out.primitives name with
                  | Some n' when n <> n' ->
                      failwith
                        (Printf.sprintf
                           "c_of_ocaml: primitive %s called with %d and %d arguments"
                           name
                           n
                           n')
                  | _ -> String.Hashtbl.replace out.primitives name n);
                  if StringSet.mem name noraise_primitives && n <= 5
                  then plain (c_call name args)
                  else if n <= 5
                  then Checked (c_call (Printf.sprintf "camlc_ccall%d" n) (name :: args))
                  else Ccallv)))

(* The types of the primitives computed on unboxed numbers, for {!Typing}:
   whether they take unboxed arguments, and their result *)
let register_typing_primitives () =
  let float = Typing.Number (Float, Unboxed) in
  let int = Typing.Int Normalized in
  List.iter float_binops ~f:(fun (name, _) -> Typing.register_prim name ~unbox:true float);
  List.iter float_functions ~f:(fun (name, _) ->
      Typing.register_prim name ~unbox:true float);
  List.iter float_comparisons ~f:(fun (name, _) ->
      Typing.register_prim name ~unbox:true int);
  Typing.register_prim "caml_neg_float" ~unbox:true float;
  Typing.register_prim "caml_float_of_int" ~unbox:false float;
  Typing.register_prim "caml_int_of_float" ~unbox:true int;
  Typing.register_prim "caml_float_compare" ~unbox:true int;
  Typing.register_prim "caml_floatarray_unsafe_get" ~unbox:false float;
  Typing.register_prim "caml_floatarray_unsafe_set" ~unbox:true int;
  List.iter boxed_ints ~f:(fun (t, r) ->
      let number : Typing.boxed_number =
        match r with
        | Unboxed_int64 -> Int64
        | Unboxed_int32 -> Int32
        | Unboxed_nativeint -> Nativeint
        | Value | Double -> assert false
      in
      let typ = Typing.Number (number, Unboxed) in
      List.iter
        ~f:(fun f ->
          Typing.register_prim (Printf.sprintf "caml_%s_%s" t f) ~unbox:true typ)
        [ "add"
        ; "sub"
        ; "mul"
        ; "and"
        ; "or"
        ; "xor"
        ; "neg"
        ; "shift_left"
        ; "shift_right"
        ; "shift_right_unsigned"
        ; "of_float"
        ];
      Typing.register_prim (Printf.sprintf "caml_%s_of_int" t) ~unbox:false typ;
      Typing.register_prim (Printf.sprintf "caml_%s_to_int" t) ~unbox:true int;
      Typing.register_prim (Printf.sprintf "caml_%s_to_float" t) ~unbox:true float;
      Typing.register_prim (Printf.sprintf "caml_%s_compare" t) ~unbox:true int;
      List.iter boxed_ints ~f:(fun (t', r') ->
          if not (repr_equal r r')
          then (
            let number' : Typing.boxed_number =
              match r' with
              | Unboxed_int64 -> Int64
              | Unboxed_int32 -> Int32
              | Unboxed_nativeint -> Nativeint
              | Value | Double -> assert false
            in
            Typing.register_prim (Printf.sprintf "caml_%s_of_%s" t t') ~unbox:true typ;
            Typing.register_prim
              (Printf.sprintf "caml_%s_to_%s" t t')
              ~unbox:true
              (Typing.Number (number', Unboxed)))));
  Typing.register_prim
    "caml_int64_bits_of_float"
    ~unbox:true
    (Typing.Number (Int64, Unboxed));
  Typing.register_prim "caml_int64_float_of_bits" ~unbox:true float

(* Primitives whose result is an integer *)
let int_primitives =
  StringSet.of_list
    [ "%int_add"
    ; "%int_sub"
    ; "%int_mul"
    ; "%direct_int_mul"
    ; "%int_neg"
    ; "%int_and"
    ; "%int_or"
    ; "%int_xor"
    ; "%int_lsl"
    ; "%int_lsr"
    ; "%int_asr"
    ; "%direct_int_div"
    ; "%direct_int_mod"
    ; "%int_div"
    ; "%int_mod"
    ; "caml_int_compare"
    ; "%direct_obj_tag"
    ; "caml_ml_string_length"
    ; "caml_ml_bytes_length"
    ; "caml_string_unsafe_get"
    ; "caml_bytes_unsafe_get"
    ; "caml_string_get"
    ; "caml_bytes_get"
    ; "caml_int_of_string"
    ; "caml_int_of_float"
    ; "caml_string_compare"
    ; "caml_bytes_compare"
    ; "caml_string_equal"
    ; "caml_string_notequal"
    ; "caml_string_lessthan"
    ; "caml_string_lessequal"
    ; "caml_string_greaterthan"
    ; "caml_string_greaterequal"
    ; "caml_bytes_equal"
    ; "caml_bytes_notequal"
    ; "caml_compare"
    ; "caml_equal"
    ; "caml_notequal"
    ; "caml_lessthan"
    ; "caml_lessequal"
    ; "caml_greaterthan"
    ; "caml_greaterequal"
    ; "caml_float_compare"
    ; "caml_eq_float"
    ; "caml_neq_float"
    ; "caml_lt_float"
    ; "caml_le_float"
    ; "caml_gt_float"
    ; "caml_ge_float"
    ; "caml_hash"
    ; "caml_obj_tag"
    ; "caml_ml_input_char"
    ; "caml_ml_input_int"
    ; "caml_ml_input"
    ; "caml_ml_channel_size"
    ; "caml_ml_pos_in"
    ; "caml_ml_pos_out"
    ; "caml_lex_engine"
    ; "caml_new_lex_engine"
    ; "caml_int64_to_int"
    ; "caml_int32_to_int"
    ; "caml_nativeint_to_int"
    ; "caml_int64_compare"
    ; "caml_int32_compare"
    ; "caml_nativeint_compare"
    ]

(* Primitives whose arguments are integers *)
let int_argument_primitives =
  StringSet.of_list
    [ "%int_add"
    ; "%int_sub"
    ; "%int_mul"
    ; "%direct_int_mul"
    ; "%int_neg"
    ; "%int_and"
    ; "%int_or"
    ; "%int_xor"
    ; "%int_lsl"
    ; "%int_lsr"
    ; "%int_asr"
    ; "%direct_int_div"
    ; "%direct_int_mod"
    ; "%int_div"
    ; "%int_mod"
    ; "caml_int_compare"
    ]

(****)

(* Program-wide facts about variables *)

type facts =
  { is_int : BitSet.t  (** The variable holds an integer *)
  ; is_static : BitSet.t  (** The variable holds static data, outside the heap *)
  }

let compute_facts (p : program) closures =
  let is_int = BitSet.create' (Var.count ()) in
  let is_static = BitSet.create' (Var.count ()) in
  let set_int x = BitSet.set is_int (Var.idx x) in
  let int x = BitSet.mem is_int (Var.idx x) in
  let int_arg a =
    match a with
    | Pv x -> set_int x
    | Pc _ -> ()
  in
  (* Definitions and uses which imply an integer *)
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, Constant (Int _)) -> set_int x
          | Let (x, Constant (Tuple (_, [||], _, _))) -> BitSet.set is_static (Var.idx x)
          | Let (x, Constant _) -> BitSet.set is_static (Var.idx x)
          | Let (x, Closure _) -> (
              match Var.Map.find_opt x closures with
              | Some { Closure_conversion.free_variables = []; _ } ->
                  BitSet.set is_static (Var.idx x)
              | Some _ | None -> ())
          | Let (x, Block (_, [||], _, _, _)) -> BitSet.set is_static (Var.idx x)
          | Let (x, Field (_, _, Immediate)) -> set_int x
          | Let (x, Prim (Array_get Immediate, _)) -> set_int x
          | Let (x, Prim ((Vectlength _ | Not | IsInt _ | Eq _ | Neq _), _)) -> set_int x
          | Let (x, Prim ((Lt | Le | Ult), args)) ->
              set_int x;
              List.iter args ~f:int_arg
          | Let (x, Prim (Extern (name, _), args)) ->
              if StringSet.mem name int_primitives then set_int x;
              if StringSet.mem name int_argument_primitives then List.iter args ~f:int_arg
          | Offset_ref _ | Let _ | Assign _ | Set_field _ | Array_set _ | Event _ -> ());
      match block.branch with
      | Switch (x, _) -> set_int x
      | _ -> ())
    p.blocks;
  (* Block parameters and assigned variables all of whose values are
     integers *)
  let sources = Var.Hashtbl.create 1024 in
  let add_source x y =
    Var.Hashtbl.replace
      sources
      x
      (y :: (Var.Hashtbl.find_opt sources x |> Option.value ~default:[]))
  in
  let add_cont (pc, args) =
    let params = (Addr.Map.find pc p.blocks).params in
    List.iter2 params args ~f:add_source
  in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Assign (x, y) -> add_source x y
          | Let (_, Closure (params, cont, _)) ->
              (* Function parameters can be anything *)
              List.iter params ~f:(fun x -> add_source x x);
              add_cont cont
          | _ -> ());
      match block.branch with
      | Branch cont | Poptrap cont -> add_cont cont
      | Cond (_, c1, c2) ->
          add_cont c1;
          add_cont c2
      | Switch (_, conts) -> Array.iter conts ~f:add_cont
      | Pushtrap (c1, x, c2) ->
          add_source x x;
          add_cont c1;
          add_cont c2
      | Return _ | Raise _ | Stop -> ())
    p.blocks;
  let changed = ref true in
  while !changed do
    changed := false;
    Var.Hashtbl.iter
      (fun x l ->
        if (not (int x)) && List.for_all l ~f:(fun y -> (not (Var.equal x y)) && int y)
        then (
          set_int x;
          changed := true))
      sources
  done;
  (* An assigned variable or a block parameter is static only if it is
     always the same static value: we do not track that *)
  Var.Hashtbl.iter (fun x _ -> BitSet.unset is_static (Var.idx x)) sources;
  { is_int; is_static }

(****)

(* Functions *)

type location =
  | Local  (** A variable of the function *)
  | Env of int  (** A field of the closure *)
  | Sibling of int  (** A function of the same closure, at this offset in words *)

type trap =
  { exn_var : Var.t
  ; id : int
  }

type fctx =
  { out : output
  ; facts : facts
  ; global_flow_info : Global_flow.info
  ; blocks : block Addr.Map.t
  ; closures : Closure_conversion.closure Var.Map.t
  ; env_var : Var.t  (** Stands for the closure of the function *)
  ; arity : repr list option
        (** The representation of the parameters of the function, not the toplevel *)
  ; static_env : bool  (** The closure is static data *)
  ; locations : location Var.Hashtbl.t
  ; slots : int Var.Hashtbl.t  (** The variables in the frame *)
  ; traps : trap list Addr.Hashtbl.t  (** The handlers in scope in each block *)
  ; handler_ids : int Addr.Hashtbl.t  (** The handler of the [Pushtrap] ending a block *)
  ; buf : Buffer.t
  ; mutable position : Parse_info.t option  (** The last debug event *)
  ; source_name : string option  (** The name of the function in the source *)
  }

(* Record a backtrace slot, if debug information is enabled: [`Call] when
   an exception goes through a call, [`Raise reraise] when one is raised
   (with the exception [e]) *)
let backtrace_stmt ctx kind e =
  match ctx.position with
  | Some pi when ctx.out.debug_info -> (
      match kind with
      | `Call ->
          Printf.sprintf
            "if (Caml_state->backtrace_active) camlc_backtrace_call(%s); "
            (location ctx.out pi ~fun_name:ctx.source_name ~is_raise:false)
      | `Raise reraise ->
          Printf.sprintf
            "if (Caml_state->backtrace_active) camlc_backtrace_raise(%s, %s, %d); "
            e
            (location ctx.out pi ~fun_name:ctx.source_name ~is_raise:true)
            (if reraise then 1 else 0))
  | Some _ | None -> ""

let local_name ctx x =
  if Var.equal x ctx.env_var then "env" else Printf.sprintf "v%d" (Var.idx x)

let local ctx x =
  match Var.Hashtbl.find_opt ctx.slots x with
  | Some i -> Printf.sprintf "fp[%d]" i
  | None -> local_name ctx x

let var ctx x =
  match Var.Hashtbl.find_opt ctx.locations x with
  | Some Local -> local ctx x
  | Some (Env i) -> Printf.sprintf "Field(%s, %d)" (local ctx ctx.env_var) i
  | Some (Sibling 0) -> local ctx ctx.env_var
  | Some (Sibling d) ->
      Printf.sprintf "((value)((value *)%s + %d))" (local ctx ctx.env_var) d
  | None -> failwith (Format.asprintf "c_of_ocaml: unbound variable %a" Var.print x)

let slot ctx x =
  match Var.Hashtbl.find_opt ctx.locations x with
  | Some Local -> local ctx x
  | Some (Env _ | Sibling _) | None ->
      failwith (Format.asprintf "c_of_ocaml: variable %a is not local" Var.print x)

let var_repr ctx x =
  if Var.equal x ctx.env_var
  then Value
  else
    match Var.Hashtbl.find_opt ctx.locations x with
    | Some Local -> repr_of_type (Typing.var_type ctx.out.types x)
    | Some (Env _ | Sibling _) | None -> Value

(* The variable [x] in representation [r]: boxed values are unboxed, while
   the typing ensures that unboxed numbers are never needed boxed (they are
   boxed where they are defined) *)
let as_repr ctx r x =
  let e = var ctx x in
  let own = var_repr ctx x in
  if repr_equal own r
  then e
  else
    match own with
    | Value -> unbox r e
    | Double | Unboxed_int64 | Unboxed_int32 | Unboxed_nativeint ->
        failwith (Format.asprintf "c_of_ocaml: %a would need to be boxed" Var.print x)

let as_value ctx x = as_repr ctx Value x

let constant_repr out c r =
  match r, c with
  | Value, _ -> code_constant out c
  | Double, Float f -> Printf.sprintf "camlc_double_of_bits(0x%LxULL)" f
  | Unboxed_int64, Int64 i -> Printf.sprintf "((int64_t)0x%LxULL)" i
  | Unboxed_int32, Int32 i -> Printf.sprintf "((int32_t)0x%lxU)" i
  | Unboxed_nativeint, NativeInt i -> Printf.sprintf "((intnat)0x%LxULL)" i
  | _ -> failwith "c_of_ocaml: a constant of the wrong type"

let prim_arg ctx a =
  match a with
  | Pv x -> { a_typ = Typing.var_type ctx.out.types x; get = (fun r -> as_repr ctx r x) }
  | Pc c -> { a_typ = Typing.constant_type c; get = (fun r -> constant_repr ctx.out c r) }

let line ctx fmt = Printf.bprintf ctx.buf ("  " ^^ fmt ^^ "\n")

(* [x = e], [e] being in representation [r] *)
let define ctx x r e =
  let own = var_repr ctx x in
  let e =
    if repr_equal own r
    then e
    else
      match r, own with
      | Value, _ -> unbox own e
      | _, Value -> box r e
      | _ -> failwith (Format.asprintf "c_of_ocaml: %a of the wrong type" Var.print x)
  in
  line ctx "%s = %s;" (slot ctx x) e

let return_stmt ctx e =
  if Var.Hashtbl.length ctx.slots = 0
  then Printf.sprintf "return %s;" e
  else Printf.sprintf "{ value res = %s; camlc_sp = fp + FRAME_SIZE; return res; }" e

(* Jump to the handler of the innermost trap, or return the exception
   result [r] *)
let raise_stmt ?bt ctx traps r =
  let pre =
    match bt with
    | None -> ""
    | Some kind -> backtrace_stmt ctx kind (Printf.sprintf "Extract_exception(%s)" r)
  in
  let stmt =
    match traps with
    | [] -> return_stmt ctx r
    | { exn_var; id; _ } :: _ ->
        Printf.sprintf "{ %s = Extract_exception(%s); goto H%d; }" (slot ctx exn_var) r id
  in
  if String.equal pre "" then stmt else Printf.sprintf "{ %s%s }" pre stmt

let branch ctx (pc, args) =
  let params = (Addr.Map.find pc ctx.blocks).params in
  let moves =
    List.filter_map (List.combine params args) ~f:(fun (p, a) ->
        if Var.equal p a then None else Some (p, a))
  in
  match moves with
  | [] -> Printf.sprintf "goto L%d;" pc
  | [ (p, a) ] ->
      Printf.sprintf
        "{ %s = %s; goto L%d; }"
        (slot ctx p)
        (as_repr ctx (var_repr ctx p) a)
        pc
  | _ ->
      let temps =
        List.mapi moves ~f:(fun i (p, a) ->
            let r = var_repr ctx p in
            Printf.sprintf "%s t%d = %s;" (c_type r) i (as_repr ctx r a))
      in
      let assigns =
        List.mapi moves ~f:(fun i (p, _) -> Printf.sprintf "%s = t%d;" (slot ctx p) i)
      in
      Printf.sprintf
        "{ %s %s goto L%d; }"
        (String.concat ~sep:" " temps)
        (String.concat ~sep:" " assigns)
        pc

(* Allocate a block whose fields are [fields] (C expressions, read after
   the allocation), and store it in [dst] *)
let alloc_block ctx ~dst ~tag ~prefix fields =
  let wosize = List.length prefix + List.length fields in
  if wosize <= 256
  then (
    line ctx "b = camlc_alloc_small(%d, %d);" wosize tag;
    List.iteri prefix ~f:(fun i e -> line ctx "Field(b, %d) = %s;" i e);
    List.iteri fields ~f:(fun i e ->
        line ctx "Field(b, %d) = %s;" (i + List.length prefix) e))
  else (
    line ctx "b = caml_alloc_shr(%d, %d);" wosize tag;
    List.iteri prefix ~f:(fun i e -> line ctx "Field(b, %d) = %s;" i e);
    List.iteri fields ~f:(fun i e ->
        line ctx "caml_initialize(&Field(b, %d), %s);" (i + List.length prefix) e));
  line ctx "%s = b;" dst

(* The start of the environment, and the position of the code pointer of
   each function. In a closure with several functions, a function of arity 2
   or more takes three words, as in native code: the compactor of OCaml 4.14
   finds the Infix headers from the arities. With effect handlers, each
   function takes three words: its code pointer, its closure info and the
   code pointer of its CPS version (or 0). *)
let closure_layout functions =
  let size arity =
    if double_translation () || (List.length functions > 1 && arity >= 2) then 3 else 2
  in
  let rec layout pos l =
    match l with
    | [] -> assert false
    | [ (_, arity) ] -> pos + size arity, [ pos ]
    | (_, arity) :: r ->
        let startenv, positions = layout (pos + size arity + 1) r in
        startenv, pos :: positions
  in
  layout 0 functions

let translate_closure ctx f =
  let info = Var.Map.find f ctx.closures in
  match info.functions with
  | _ when Var.Set.mem f ctx.out.cps_sides ->
      (* A CPS version, in the closure of its direct-style version *)
      ()
  | (f0, _) :: _ when not (Var.equal f f0) ->
      (* Allocated with the first function of the closure *)
      ()
  | functions ->
      let startenv, positions = closure_layout functions in
      let prefix =
        List.concat
          (List.map2 functions positions ~f:(fun (g, arity) pos ->
               let code =
                 [ Printf.sprintf "(value)&%s" (fun_name ctx.out g)
                 ; Printf.sprintf "Make_closinfo(%d, %d)" arity (startenv - pos)
                 ]
               in
               let code =
                 if double_translation ()
                 then
                   code
                   @ [ (match Var.Map.find_opt g ctx.out.cps_pairs with
                       | Some c -> Printf.sprintf "(value)&%s" (fun_name ctx.out c)
                       | None -> "0")
                     ]
                 else if List.length functions > 1 && arity >= 2
                 then code @ [ "Val_unit" ]
                 else code
               in
               if pos = 0
               then code
               else Printf.sprintf "Make_header(%d, Infix_tag, 0)" pos :: code))
      in
      if List.is_empty info.free_variables
      then
        (* A closed closure is static data *)
        let name =
          static_block
            ctx.out
            (header ~wosize:(List.length prefix) ~tag:Obj.closure_tag :: prefix)
        in
        List.iter2 functions positions ~f:(fun (g, _) pos ->
            line ctx "%s = (value)&%s[%d];" (slot ctx g) name (pos + 1))
      else (
        alloc_block
          ctx
          ~dst:(slot ctx f)
          ~tag:Obj.closure_tag
          ~prefix
          (List.map info.free_variables ~f:(fun x -> as_value ctx x));
        List.iter2 functions positions ~f:(fun (g, _) pos ->
            if pos > 0 then line ctx "%s = (value)((value *)b + %d);" (slot ctx g) pos))

let apply_arity out n = out.apply_arities <- IntSet.add n out.apply_arities

let out_cps_apply_arity out n =
  out.cps_apply_arities <- IntSet.add n out.cps_apply_arities

let param_reprs ctx g =
  Var.Hashtbl.find_opt ctx.out.fun_reprs g |> Option.value ~default:[]

(* A CPS call: its last argument is the continuation *)
let is_cps_call ctx x = double_translation () && Var.Set.mem x ctx.out.in_cps

type callee =
  | Known of Var.t  (** The function statically known to be called *)
  | Known_direct of Var.t
      (** A CPS call of a function without CPS version: it is called in
          direct style, and its result passed to the continuation *)
  | Unknown

let callee ctx x ~f ~exact =
  if exact
  then
    match Global_flow.get_unique_closure ctx.global_flow_info ~call:x f with
    | Some (g, _, cont) when Var.Map.mem g ctx.closures ->
        if cont then Known_direct g else Known g
    | Some _ | None -> Unknown
  else Unknown

(* The representation of the parameters of the function called *)
let callee_reprs ctx x ~f ~args ~exact =
  match callee ctx x ~f ~exact with
  | Known g -> param_reprs ctx g
  | Known_direct g -> param_reprs ctx g @ [ Value ]
  | Unknown -> List.map args ~f:(fun _ -> Value)

(* The arguments of an application which are unboxed numbers passed to a
   parameter holding values: they are boxed before the call *)
let boxed_arguments ctx x ~f ~args ~exact =
  List.map2 args (callee_reprs ctx x ~f ~args ~exact) ~f:(fun a r ->
      repr_equal r Value && not (repr_equal (var_repr ctx a) Value))

(* The C function called by an application, its arguments ([n] arguments
   and a closure in all cases), and the representation of its parameters.
   The numbers to box are boxed first, into slots of the shadow stack
   ([bx], see [boxing_prelude]). For [Known_direct], the arguments are those
   of the function, its closure, then the continuation. *)
let apply ctx x ~f ~args ~exact =
  let n = List.length args in
  let reprs = callee_reprs ctx x ~f ~args ~exact in
  let boxed = boxed_arguments ctx x ~f ~args ~exact in
  let j = ref 0 in
  let args =
    List.map2 (List.combine args reprs) boxed ~f:(fun (a, r) b ->
        if b
        then (
          let e = Printf.sprintf "bx[%d]" !j in
          incr j;
          e)
        else as_repr ctx r a)
  in
  let fe = as_value ctx f in
  match callee ctx x ~f ~exact with
  | Known g -> fun_name ctx.out g, args @ [ fe ], reprs
  | Known_direct g ->
      let args, k = List.take (n - 1) args in
      fun_name ctx.out g, args @ [ fe ] @ k, reprs
  | Unknown ->
      if is_cps_call ctx x
      then (
        out_cps_apply_arity ctx.out n;
        ( Printf.sprintf (if exact then "camlc_call_cps%d" else "camlc_apply_cps%d") n
        , fe :: args
        , reprs ))
      else if exact
      then Printf.sprintf "((%s)Field(%s, 0))" (fun_type n) fe, args @ [ fe ], reprs
      else (
        apply_arity ctx.out n;
        Printf.sprintf "camlc_apply%d" n, fe :: args, reprs)

(* Box the numbers passed to parameters holding values into [bx], a part
   of the shadow stack (an allocation may move the previous ones). Returns
   the number of slots. *)
let boxing_prelude ctx traps x ~f ~args ~exact =
  let boxed =
    List.filter_map
      (List.combine args (boxed_arguments ctx x ~f ~args ~exact))
      ~f:(fun (a, b) -> if b then Some a else None)
  in
  let k = List.length boxed in
  if k > 0
  then (
    line ctx "bx = camlc_sp - %d;" k;
    line
      ctx
      "if (CAMLC_UNLIKELY(bx < camlc_stack_limit)) %s"
      (raise_stmt ctx traps "camlc_stack_overflow()");
    line ctx "camlc_sp = bx;";
    List.iteri boxed ~f:(fun i _ -> line ctx "bx[%d] = Val_unit;" i);
    List.iteri boxed ~f:(fun i a ->
        let r = var_repr ctx a in
        line ctx "bx[%d] = %s;" i (box r (var ctx a))));
  k

(* An application whose result is returned: the frame is released before
   the call, which is a tail call when the callee has the signature of the
   function (as required by [musttail]).

   A CPS call which may bounce, unless it is a tail call, first decrements
   [camlc_cps_depth]: when it gets negative, the call is returned to the
   trampoline ([camlc_cps_bounce]), which performs it on a shorter stack. *)
let translate_tail_call ctx x ~f ~args ~exact =
  let n = List.length args in
  let k = boxing_prelude ctx [] x ~f ~args ~exact in
  let kind = callee ctx x ~f ~exact in
  let callee, args, reprs = apply ctx x ~f ~args ~exact in
  let types =
    List.map
      (match kind with
      | Known_direct _ -> fst (List.take (n - 1) reprs) @ [ Value; Value ]
      | Known _ | Unknown -> reprs @ [ Value ])
      ~f:c_type
  in
  let temps =
    List.mapi (List.combine types args) ~f:(fun i (t, a) ->
        Printf.sprintf "%s t%d = %s;" t i a)
  in
  let t i = Printf.sprintf "t%d" i in
  let musttail =
    match kind, ctx.arity with
    | (Known _ | Unknown), Some reprs' -> List.equal ~eq:repr_equal reprs reprs'
    | Known_direct _, _ | _, None -> false
  in
  let bounce =
    if
      is_cps_call ctx x
      && Var.Set.mem x ctx.out.trampolined
      && (not musttail)
      && List.for_all reprs ~f:(fun r -> repr_equal r Value)
    then
      (* The closure, then the arguments *)
      let closure, arguments =
        match kind with
        | Known _ -> n, List.init ~len:n ~f:Fun.id
        | Known_direct _ -> n - 1, List.init ~len:(n - 1) ~f:Fun.id @ [ n ]
        | Unknown -> 0, List.init ~len:n ~f:(fun i -> i + 1)
      in
      Printf.sprintf
        " if (CAMLC_UNLIKELY(--camlc_cps_depth < 0)) { %s return camlc_cps_bounce(%s, \
         %d); }"
        (String.concat
           ~sep:" "
           (List.mapi arguments ~f:(fun i j ->
                Printf.sprintf "camlc_bounce_args[%d] = %s;" i (t j))))
        (t closure)
        n
    else ""
  in
  let pop =
    if Var.Hashtbl.length ctx.slots = 0
    then if k > 0 then Printf.sprintf " camlc_sp = bx + %d;" k else ""
    else " camlc_sp = fp + FRAME_SIZE;"
  in
  match kind with
  | Known_direct _ ->
      (* The continuation is kept on the shadow stack during the call *)
      let call = c_call callee (List.init ~len:n ~f:t) in
      line
        ctx
        "{ %s%s%s value *ks = camlc_sp - 1; if (CAMLC_UNLIKELY(ks < camlc_stack_limit)) \
         return camlc_stack_overflow(); *ks = t%d; camlc_sp = ks; value r = %s; camlc_sp \
         = ks + 1; if (CAMLC_UNLIKELY(Is_exception_result(r))) return r; return ((value \
         (*)(value, value))Field(*ks, 0))(r, *ks); }"
        (String.concat ~sep:" " temps)
        pop
        bounce
        n
        call
  | Known _ | Unknown ->
      let call = c_call callee (List.mapi args ~f:(fun i _ -> t i)) in
      line
        ctx
        "{ %s%s%s %sreturn %s; }"
        (String.concat ~sep:" " temps)
        pop
        bounce
        (if musttail then "CAMLC_MUSTTAIL " else "")
        call

let translate_instr ctx traps i =
  match i with
  | Let (x, e) -> (
      let dst = slot ctx x in
      let checked ?(bt = `Raise false) call =
        line ctx "r = %s;" call;
        line
          ctx
          "if (CAMLC_UNLIKELY(Is_exception_result(r))) %s"
          (raise_stmt ~bt ctx traps "r");
        define ctx x Value "r"
      in
      match e with
      | Apply { f; args; exact } ->
          if is_cps_call ctx x then failwith "c_of_ocaml: a CPS call is not a tail call";
          let k = boxing_prelude ctx traps x ~f ~args ~exact in
          let callee, args, _ = apply ctx x ~f ~args ~exact in
          if k = 0
          then checked ~bt:`Call (c_call callee args)
          else (
            line ctx "r = %s;" (c_call callee args);
            line ctx "camlc_sp = bx + %d;" k;
            line
              ctx
              "if (CAMLC_UNLIKELY(Is_exception_result(r))) %s"
              (raise_stmt ~bt:`Call ctx traps "r");
            define ctx x Value "r")
      | Block (tag, a, _, _, _) ->
          if Array.length a = 0
          then line ctx "%s = Atom(%d);" dst tag
          else if tag = Obj.double_array_tag
          then (
            let wosize = Array.length a * 8 / word_size () in
            if wosize <= 256
            then line ctx "b = camlc_alloc_small(%d, Double_array_tag);" wosize
            else line ctx "b = caml_alloc_shr(%d, Double_array_tag);" wosize;
            Array.iteri a ~f:(fun i x ->
                line ctx "Store_double_flat_field(b, %d, %s);" i (as_repr ctx Double x));
            line ctx "%s = b;" dst)
          else
            alloc_block
              ctx
              ~dst
              ~tag
              ~prefix:[]
              (Array.to_list (Array.map a ~f:(fun x -> as_value ctx x)))
      | Field (y, i, (Non_float | Immediate)) ->
          define ctx x Value (Printf.sprintf "Field(%s, %d)" (as_value ctx y) i)
      | Field (y, i, Float) ->
          define
            ctx
            x
            Double
            (Printf.sprintf "Double_flat_field(%s, %d)" (as_value ctx y) i)
      | Closure _ -> translate_closure ctx x
      | Constant c -> (
          match var_repr ctx x with
          | Value -> line ctx "%s = %s;" dst (code_constant ctx.out c)
          | r -> line ctx "%s = %s;" dst (constant_repr ctx.out c r))
      | Special (Alias_prim name) ->
          failwith (Printf.sprintf "c_of_ocaml: unsupported alias primitive %s" name)
      | Prim (Extern ("caml_cps_closure", _), [ Pv d; Pv _ ]) ->
          (* The closure of the direct-style version holds both versions *)
          define ctx x Value (as_value ctx d)
      | Prim (Extern (name, _), args) -> (
          let args = List.map args ~f:(fun a -> prim_arg ctx a) in
          let n = List.length args in
          match primitive ctx.out name args with
          | Plain (r, e, _) -> define ctx x r e
          | Ccallv ->
              (* More than 5 arguments: the C function takes an array *)
              line ctx "argv = camlc_sp - %d;" n;
              line
                ctx
                "if (CAMLC_UNLIKELY(argv < camlc_stack_limit)) %s"
                (raise_stmt ctx traps "camlc_stack_overflow()");
              line ctx "camlc_sp = argv;";
              List.iteri args ~f:(fun i a -> line ctx "argv[%d] = %s;" i (a.get Value));
              line ctx "r = camlc_ccallv((camlc_primv)%s, argv, %d);" name n;
              line ctx "camlc_sp = argv + %d;" n;
              line
                ctx
                "if (CAMLC_UNLIKELY(Is_exception_result(r))) %s"
                (raise_stmt ~bt:(`Raise false) ctx traps "r");
              define ctx x Value "r"
          | Checked e -> checked e)
      | Prim (p, args) -> (
          let args = List.map args ~f:(fun a -> (prim_arg ctx a).get Value) in
          let plain e = define ctx x Value e in
          match p, args with
          | Vectlength Value, [ a ] -> plain (Printf.sprintf "Val_long(Wosize_val(%s))" a)
          | Vectlength Float, [ a ] ->
              plain (Printf.sprintf "Val_long(Wosize_val(%s) / Double_wosize)" a)
          | Vectlength Generic, [ a ] ->
              plain (Printf.sprintf "Val_long(camlc_array_length(%s))" a)
          | Array_get (Non_float | Immediate | Float), [ a; i ] ->
              plain (Printf.sprintf "Field(%s, Long_val(%s))" a i)
          | Not, [ a ] -> plain (Printf.sprintf "Val_bool(%s == Val_false)" a)
          | IsInt _, [ a ] -> plain (Printf.sprintf "Val_bool(Is_long(%s))" a)
          | Eq _, [ a; b ] -> plain (Printf.sprintf "Val_bool(%s == %s)" a b)
          | Neq _, [ a; b ] -> plain (Printf.sprintf "Val_bool(%s != %s)" a b)
          | Lt, [ a; b ] -> plain (Printf.sprintf "Val_bool((intnat)%s < (intnat)%s)" a b)
          | Le, [ a; b ] ->
              plain (Printf.sprintf "Val_bool((intnat)%s <= (intnat)%s)" a b)
          | Ult, [ a; b ] ->
              plain (Printf.sprintf "Val_bool((uintnat)%s < (uintnat)%s)" a b)
          | (Vectlength _ | Array_get _ | Not | IsInt _ | Eq _ | Neq _ | Lt | Le | Ult), _
            -> assert false
          | Extern _, _ -> assert false))
  | Assign (x, y) -> line ctx "%s = %s;" (slot ctx x) (as_repr ctx (var_repr ctx x) y)
  | Set_field (x, i, (Non_float | Immediate), y) ->
      line ctx "camlc_modify(&Field(%s, %d), %s);" (as_value ctx x) i (as_value ctx y)
  | Set_field (x, i, Float, y) ->
      line
        ctx
        "Store_double_flat_field(%s, %d, %s);"
        (as_value ctx x)
        i
        (as_repr ctx Double y)
  | Offset_ref (x, n) ->
      let x = as_value ctx x in
      line ctx "Field(%s, 0) = (value)((uintnat)Field(%s, 0) + %d);" x x (2 * n)
  | Array_set (x, i, y) ->
      line
        ctx
        "camlc_modify(&Field(%s, Long_val(%s)), %s);"
        (as_value ctx x)
        (as_value ctx i)
        (as_value ctx y)
  | Event pi -> ctx.position <- Some pi

let translate_last ctx traps last =
  match last with
  | Return x -> line ctx "%s" (return_stmt ctx (as_value ctx x))
  | Raise (x, kind) -> (
      let bt =
        match kind with
        | `Normal -> backtrace_stmt ctx (`Raise false) (as_value ctx x)
        | `Reraise -> backtrace_stmt ctx (`Raise true) (as_value ctx x)
        | `Notrace -> ""
      in
      if not (String.equal bt "") then line ctx "%s" bt;
      match traps with
      | [] ->
          line
            ctx
            "%s"
            (return_stmt
               ctx
               (Printf.sprintf "Make_exception_result(%s)" (as_value ctx x)))
      | { exn_var; id; _ } :: _ ->
          line ctx "%s = %s; goto H%d;" (slot ctx exn_var) (as_value ctx x) id)
  | Stop -> line ctx "%s" (return_stmt ctx "Val_unit")
  | Branch cont -> line ctx "%s" (branch ctx cont)
  | Cond (x, c1, c2) ->
      line ctx "if (%s != Val_false) %s" (as_value ctx x) (branch ctx c1);
      line ctx "else %s" (branch ctx c2)
  | Switch (x, conts) ->
      line ctx "switch (Long_val(%s)) {" (as_value ctx x);
      let n = Array.length conts in
      Array.iteri conts ~f:(fun i cont ->
          if i = n - 1
          then line ctx "default: %s" (branch ctx cont)
          else line ctx "case %d: %s" i (branch ctx cont));
      line ctx "}"
  | Pushtrap (cont, _, _) -> line ctx "%s" (branch ctx cont)
  | Poptrap cont -> line ctx "%s" (branch ctx cont)

(* The blocks of a function, and the handlers in scope in each of them *)
let function_blocks ctx pc =
  let traps = ctx.traps in
  let next_id = ref 0 in
  let order = ref [] in
  let rec visit pc t =
    match Addr.Hashtbl.find_opt traps pc with
    | Some t' ->
        if List.length t <> List.length t'
        then failwith "c_of_ocaml: inconsistent exception handlers"
    | None -> (
        Addr.Hashtbl.add traps pc t;
        order := pc :: !order;
        let block = Addr.Map.find pc ctx.blocks in
        match block.branch with
        | Return _ | Raise _ | Stop -> ()
        | Branch (pc', _) -> visit pc' t
        | Poptrap (pc', _) -> (
            match t with
            | _ :: t' -> visit pc' t'
            | [] -> failwith "c_of_ocaml: unbalanced exception handlers")
        | Cond (_, (pc1, _), (pc2, _)) ->
            visit pc1 t;
            visit pc2 t
        | Switch (_, conts) -> Array.iter conts ~f:(fun (pc', _) -> visit pc' t)
        | Pushtrap ((pc1, _), x, (pc2, _)) ->
            let id = !next_id in
            incr next_id;
            Addr.Hashtbl.add ctx.handler_ids pc id;
            visit pc1 ({ exn_var = x; id } :: t);
            visit pc2 t)
  in
  visit pc [];
  List.rev !order

(****)

(* Liveness, to find the variables which must survive a garbage
   collection: they are kept in the frame of the function *)

(* The local variables used by an expression, the variables of the
   enclosing functions standing for the closure *)
let use ctx x acc =
  match Var.Hashtbl.find_opt ctx.locations x with
  | Some Local -> Var.Set.add x acc
  | Some (Env _ | Sibling _) -> Var.Set.add ctx.env_var acc
  | None -> acc

let use_list ctx l acc = List.fold_left l ~init:acc ~f:(fun acc x -> use ctx x acc)

let use_prim_args ctx l acc =
  List.fold_left l ~init:acc ~f:(fun acc a ->
      match a with
      | Pv x -> use ctx x acc
      | Pc _ -> acc)

let expr_uses ctx x e acc =
  match e with
  | Apply { f; args; _ } -> use ctx f (use_list ctx args acc)
  | Block (_, a, _, _, _) -> use_list ctx (Array.to_list a) acc
  | Field (y, _, _) -> use ctx y acc
  | Closure _ -> (
      match Var.Map.find_opt x ctx.closures with
      | Some info -> use_list ctx info.free_variables acc
      | None -> acc)
  | Constant _ | Special _ -> acc
  | Prim (Extern ("caml_cps_closure", _), [ Pv d; _ ]) -> use ctx d acc
  | Prim (_, args) -> use_prim_args ctx args acc

(* How an instruction may trigger a garbage collection: [`Call] if its
   operands are read before, [`Alloc] if they are read after *)
let gc_point ctx i =
  match i with
  | Let (x, Apply { f; args; exact }) ->
      if List.exists (boxed_arguments ctx x ~f ~args ~exact) ~f:Fun.id
      then `Alloc
      else `Call
  | Let (_, Block (_, [||], _, _, _)) -> `No
  | Let (_, Block _) -> `Alloc
  | Let (x, Closure _) -> (
      match Var.Map.find_opt x ctx.closures with
      | Some { functions = (f0, _) :: _; free_variables = _ :: _; _ } when Var.equal x f0
        -> `Alloc
      | Some _ | None -> `No)
  | Let (_, Prim (Extern ("caml_cps_closure", _), _)) -> `No
  | Let (x, Field (_, _, Float)) ->
      (* The float is boxed if the variable holds a value *)
      if repr_equal (var_repr ctx x) Value then `Call else `No
  | Let (x, Prim (Extern (name, _), args)) -> (
      let dummy a = { (prim_arg ctx a) with get = (fun _ -> "_") } in
      let dummy a =
        match a with
        | Pc c -> { a_typ = Typing.constant_type c; get = (fun _ -> "_") }
        | Pv _ -> dummy a
      in
      match primitive ctx.out name (List.map args ~f:dummy) with
      | Plain (r, _, gc) ->
          if gc || ((not (repr_equal r Value)) && repr_equal (var_repr ctx x) Value)
          then `Call
          else `No
      | Checked _ -> if StringSet.mem name nogc_primitives then `No else `Call
      | Ccallv -> `Call)
  | Let (_, Prim ((Vectlength Generic | Array_get Float), _)) -> `No
  | Let (_, (Field _ | Constant _ | Special _ | Prim _)) -> `No
  | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> `No

let instr_defs i =
  match i with
  | Let (x, _) | Assign (x, _) -> Some x
  | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> None

let instr_uses ctx i acc =
  match i with
  | Let (x, e) -> expr_uses ctx x e acc
  | Assign (_, y) -> use ctx y acc
  | Set_field (x, _, _, y) -> use ctx x (use ctx y acc)
  | Offset_ref (x, _) -> use ctx x acc
  | Array_set (x, y, z) -> use ctx x (use ctx y (use ctx z acc))
  | Event _ -> acc

let cont_uses ctx live_in (pc, args) acc =
  let params = (Addr.Map.find pc ctx.blocks).params in
  let l = Addr.Hashtbl.find_opt live_in pc |> Option.value ~default:Var.Set.empty in
  use_list
    ctx
    args
    (Var.Set.union acc (List.fold_left params ~init:l ~f:(fun l x -> Var.Set.remove x l)))

let live_out ctx live_in pc =
  let block = Addr.Map.find pc ctx.blocks in
  let acc =
    match block.branch with
    | Return x | Raise (x, _) -> use ctx x Var.Set.empty
    | Stop -> Var.Set.empty
    | Branch cont | Poptrap cont | Pushtrap (cont, _, _) ->
        cont_uses ctx live_in cont Var.Set.empty
    | Cond (x, c1, c2) ->
        use ctx x (cont_uses ctx live_in c1 (cont_uses ctx live_in c2 Var.Set.empty))
    | Switch (x, conts) ->
        use
          ctx
          x
          (Array.fold_left conts ~init:Var.Set.empty ~f:(fun acc c ->
               cont_uses ctx live_in c acc))
  in
  (* The handler of the innermost trap may be reached from any point of
     the block *)
  match Addr.Hashtbl.find ctx.traps pc with
  | [] -> acc
  | { exn_var; id } :: _ ->
      let handler =
        Addr.Hashtbl.fold
          (fun pc' id' h ->
            if id = id'
            then
              match (Addr.Map.find pc' ctx.blocks).branch with
              | Pushtrap (_, _, h) -> Some h
              | _ -> h
            else h)
          ctx.handler_ids
          None
      in
      let h =
        match handler with
        | Some h -> h
        | None -> assert false
      in
      Var.Set.remove exn_var (cont_uses ctx live_in h acc)

(* The variables which must be kept in the frame *)
let rooted_variables ctx pcs ~params =
  let live_in = Addr.Hashtbl.create 64 in
  let block_live_in pc live =
    let block = Addr.Map.find pc ctx.blocks in
    List.fold_right block.body ~init:live ~f:(fun i live ->
        let live =
          match instr_defs i with
          | Some x -> Var.Set.remove x live
          | None -> live
        in
        instr_uses ctx i live)
  in
  let changed = ref true in
  let rpcs = List.rev pcs in
  while !changed do
    changed := false;
    List.iter rpcs ~f:(fun pc ->
        let l = block_live_in pc (live_out ctx live_in pc) in
        match Addr.Hashtbl.find_opt live_in pc with
        | Some l' when Var.Set.equal l l' -> ()
        | _ ->
            Addr.Hashtbl.replace live_in pc l;
            changed := true)
  done;
  let rooted = ref Var.Set.empty in
  List.iter pcs ~f:(fun pc ->
      let block = Addr.Map.find pc ctx.blocks in
      ignore
        (List.fold_right block.body ~init:(live_out ctx live_in pc) ~f:(fun i live ->
             let def = instr_defs i in
             let live_after =
               match def with
               | Some x -> Var.Set.remove x live
               | None -> live
             in
             (match gc_point ctx i with
             | `No -> ()
             | `Call -> rooted := Var.Set.union !rooted live_after
             | `Alloc ->
                 let l = instr_uses ctx i live_after in
                 rooted :=
                   Var.Set.union
                     !rooted
                     (match def with
                     | Some x -> Var.Set.remove x l
                     | None -> l));
             instr_uses ctx i live_after)));
  ignore params;
  Var.Set.filter
    (fun x ->
      if Var.equal x ctx.env_var
      then not ctx.static_env
      else
        repr_equal (var_repr ctx x) Value
        && (match Typing.var_type ctx.out.types x with
          | Int _ -> false
          | _ -> true)
        && not
             (BitSet.mem ctx.facts.is_int (Var.idx x)
             || BitSet.mem ctx.facts.is_static (Var.idx x)))
    !rooted

(****)

let translate_function
    out
    facts
    global_flow_info
    blocks
    closures
    ~name
    ~params
    ~param_reprs
    ~env
    ~static_env
    ~source_name
    (pc, args) =
  let ctx =
    { out
    ; facts
    ; global_flow_info
    ; blocks
    ; closures
    ; env_var = Var.fresh ()
    ; arity = None
    ; static_env
    ; locations = Var.Hashtbl.create 64
    ; slots = Var.Hashtbl.create 16
    ; traps = Addr.Hashtbl.create 16
    ; handler_ids = Addr.Hashtbl.create 16
    ; buf = Buffer.create 4096
    ; position = None
    ; source_name
    }
  in
  List.iter env ~f:(fun (x, loc) -> Var.Hashtbl.replace ctx.locations x loc);
  let ctx = if List.is_empty env then ctx else { ctx with arity = Some param_reprs } in
  let locals = ref [] in
  let add_local x =
    if not (Var.Hashtbl.mem ctx.locations x)
    then (
      Var.Hashtbl.add ctx.locations x Local;
      locals := x :: !locals)
  in
  List.iter params ~f:add_local;
  let pcs = function_blocks ctx pc in
  List.iter pcs ~f:(fun pc ->
      let block = Addr.Map.find pc blocks in
      List.iter block.params ~f:add_local;
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, _) | Assign (x, _) -> add_local x
          | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ());
      match block.branch with
      | Pushtrap (_, x, _) -> add_local x
      | Return _ | Raise _ | Stop | Branch _ | Cond _ | Switch _ | Poptrap _ -> ());
  let rooted = rooted_variables ctx pcs ~params in
  Var.Set.iter
    (fun x -> Var.Hashtbl.add ctx.slots x (Var.Hashtbl.length ctx.slots))
    rooted;
  let frame_size = Var.Hashtbl.length ctx.slots in
  (* Body *)
  List.iteri params ~f:(fun i x ->
      if Var.Hashtbl.mem ctx.slots x then line ctx "%s = a%d;" (local ctx x) i);
  if Var.Hashtbl.mem ctx.slots ctx.env_var
  then line ctx "%s = env;" (local ctx ctx.env_var);
  line ctx "%s" (branch ctx (pc, args));
  List.iter pcs ~f:(fun pc ->
      let block = Addr.Map.find pc blocks in
      let traps = Addr.Hashtbl.find ctx.traps pc in
      Printf.bprintf ctx.buf " L%d:;\n" pc;
      (* An application whose result is returned, outside any handler *)
      let tail_call =
        let rec strip_events l =
          match l with
          | Event _ :: r -> strip_events r
          | _ -> l
        in
        match strip_events (List.rev block.body), block.branch, traps with
        | Let (x, Apply { f; args; exact }) :: rem, Return y, [] when Var.equal x y ->
            Some (List.rev rem, x, f, args, exact)
        | _ -> None
      in
      match tail_call with
      | Some (body, x, f, args, exact) ->
          List.iter body ~f:(fun i -> translate_instr ctx traps i);
          translate_tail_call ctx x ~f ~args ~exact
      | None -> (
          List.iter block.body ~f:(fun i -> translate_instr ctx traps i);
          translate_last ctx traps block.branch;
          match block.branch with
          | Pushtrap (_, _, handler) ->
              Printf.bprintf ctx.buf " H%d:;\n" (Addr.Hashtbl.find ctx.handler_ids pc);
              line ctx "%s" (branch ctx handler)
          | Return _ | Raise _ | Stop | Branch _ | Cond _ | Switch _ | Poptrap _ -> ()));
  let param_set = Var.Set.of_list params in
  let decls =
    List.filter_map (List.rev !locals) ~f:(fun x ->
        if Var.Hashtbl.mem ctx.slots x || Var.Set.mem x param_set
        then None
        else Some (var_repr ctx x, local_name ctx x))
  in
  let params_c =
    String.concat
      ~sep:", "
      (List.mapi params ~f:(fun i x ->
           if Var.Hashtbl.mem ctx.slots x
           then Printf.sprintf "value a%d" i
           else Printf.sprintf "%s %s" (c_type (var_repr ctx x)) (local_name ctx x))
      @ if List.is_empty env then [] else [ "value env" ])
  in
  Printf.bprintf
    out.functions
    "%s(%s) {\n"
    name
    (if String.equal params_c "" then "void" else params_c);
  List.iter
    [ Value; Double; Unboxed_int64; Unboxed_int32; Unboxed_nativeint ]
    ~f:(fun r ->
      let l =
        List.filter_map decls ~f:(fun (r', d) -> if repr_equal r r' then Some d else None)
      in
      List.iteri l ~f:(fun i d ->
          if i mod 16 = 0
          then
            Printf.bprintf
              out.functions
              (if i = 0 then "  %s " else ";\n  %s ")
              (c_type r);
          if i mod 16 <> 0 then Buffer.add_string out.functions ", ";
          Buffer.add_string out.functions d);
      if not (List.is_empty l) then Buffer.add_string out.functions ";\n");
  Buffer.add_string
    out.functions
    "  value r, b, *argv, *bx;\n  (void)r; (void)b; (void)argv; (void)bx;\n";
  if frame_size > 0
  then
    Printf.bprintf
      out.functions
      "#define FRAME_SIZE %d\n\
      \  value *fp = camlc_sp - FRAME_SIZE;\n\
      \  if (CAMLC_UNLIKELY(fp < camlc_stack_limit)) return camlc_stack_overflow();\n\
      \  camlc_sp = fp;\n\
      \  for (int i = 0; i < FRAME_SIZE; i++) fp[i] = Val_unit;\n"
      frame_size;
  Buffer.add_buffer out.functions ctx.buf;
  Buffer.add_string out.functions "}\n";
  if frame_size > 0 then Buffer.add_string out.functions "#undef FRAME_SIZE\n";
  Buffer.add_char out.functions '\n'

(****)

(* The size of the runtime's table of global data, where the predefined
   exceptions are registered *)
let global_data_size (p : program) =
  Addr.Map.fold
    (fun _ block n ->
      List.fold_left block.body ~init:n ~f:(fun n i ->
          match i with
          | Let (_, Prim (Extern ("caml_register_global_by_index", _), [ _; Pc (Int i) ]))
            -> max n (Targetint.to_int_exn i + 1)
          | _ -> n))
    p.blocks
    16

let collect_functions p =
  (* All the closures, with their parameters and entry point *)
  fold_closures
    p
    (fun name_opt params cont _ acc ->
      match name_opt with
      | Some f -> (f, params, cont) :: acc
      | None -> acc)
    []

let f
    (p : program)
    ~global_flow_data
    ~deadcode_sentinel
    ~debug_info
    ~in_cps
    ~trampolined_calls
    closures =
  let global_flow_state, global_flow_info = global_flow_data in
  let facts = compute_facts p closures in
  let cps_pairs, cps_sides, public_name =
    Var.Hashtbl.fold
      (fun x (d, c) (cps_pairs, cps_sides, public_name) ->
        ( Var.Map.add d c cps_pairs
        , Var.Set.add c cps_sides
        , Var.Map.add d x (Var.Map.add c x public_name) ))
      global_flow_info.Global_flow.info_cps_closures
      (Var.Map.empty, Var.Set.empty, Var.Map.empty)
  in
  let functions = collect_functions p in
  (* Which numbers can be unboxed. Closures hold values, and so do the
     parameters of the CPS versions, which the runtime may call. *)
  Typing.reset ();
  register_typing_primitives ();
  let types =
    let fun_info = Call_graph_analysis.f p global_flow_info in
    let boxed =
      Var.Map.fold
        (fun _ (info : Closure_conversion.closure) acc -> info.free_variables @ acc)
        closures
        []
    in
    let boxed =
      List.fold_left functions ~init:boxed ~f:(fun acc (f, params, _) ->
          if Var.Set.mem f cps_sides then params @ acc else acc)
    in
    Typing.f ~global_flow_state ~global_flow_info ~fun_info ~deadcode_sentinel ~boxed p
  in
  let out =
    { data = Buffer.create 65536
    ; data_fields = Buffer.create 65536
    ; functions = Buffer.create 65536
    ; data_count = 0
    ; constants = Int.Hashtbl.create 1024
    ; primitives = String.Hashtbl.create 128
    ; apply_arities = IntSet.empty
    ; max_arity = 1
    ; global_data_size = global_data_size p
    ; fun_names = Var.Hashtbl.create 1024
    ; types
    ; fun_reprs = Var.Hashtbl.create 1024
    ; debug_info
    ; locations = String.Hashtbl.create 1024
    ; location_defs = Buffer.create 4096
    ; in_cps
    ; trampolined = trampolined_calls
    ; cps_pairs
    ; cps_sides
    ; cps_apply_arities = IntSet.empty
    }
  in
  List.iter functions ~f:(fun (f, params, _) ->
      out.max_arity <- max out.max_arity (List.length params);
      Var.Hashtbl.replace
        out.fun_reprs
        f
        (List.map params ~f:(fun x -> repr_of_type (Typing.var_type types x)));
      ignore (fun_name out f));
  List.iter functions ~f:(fun (f, params, cont) ->
      let info = Var.Map.find f closures in
      let startenv, positions = closure_layout info.Closure_conversion.functions in
      let k =
        let rec find l pos =
          match l, pos with
          | (g, _) :: _, pos :: _ when Var.equal g f -> pos
          | _ :: r, _ :: rpos -> find r rpos
          | _ -> assert false
        in
        find info.functions positions
      in
      let env =
        List.mapi info.free_variables ~f:(fun j x -> x, Env (startenv - k + j))
        @ List.concat
            (List.map2 info.functions positions ~f:(fun (g, _) pos ->
                 (* The code refers to a function with two versions by the
                    variable bound to its closure *)
                 let l = Sibling (pos - k) in
                 match Var.Map.find_opt g public_name with
                 | Some x -> [ g, l; x, l ]
                 | None -> [ g, l ]))
      in
      translate_function
        out
        facts
        global_flow_info
        p.blocks
        closures
        ~name:(Printf.sprintf "static value %s" (fun_name out f))
        ~params
        ~param_reprs:(Var.Hashtbl.find out.fun_reprs f)
        ~env
        ~static_env:(List.is_empty info.free_variables)
        ~source_name:(Var.get_name f)
        cont);
  translate_function
    out
    facts
    global_flow_info
    p.blocks
    closures
    ~name:"value camlc_program"
    ~params:[]
    ~param_reprs:[]
    ~env:[]
    ~static_env:true
    ~source_name:None
    (p.start, []);
  (* Assemble the output *)
  let b = Buffer.create (Buffer.length out.data + Buffer.length out.functions + 65536) in
  Buffer.add_string b "/* Generated by c_of_ocaml */\n#include \"camlc.h\"\n\n";
  Printf.bprintf
    b
    "_Static_assert(sizeof(value) == %d, \"c_of_ocaml: wrong word size\");\n\n"
    (word_size ());
  String.Hashtbl.iter
    (fun name n ->
      if n <= 5
      then
        Printf.bprintf
          b
          "extern value %s(%s);\n"
          name
          (if n = 0
           then "void"
           else String.concat ~sep:", " (List.init ~len:n ~f:(fun _ -> "value")))
      else Printf.bprintf b "extern value %s(value *, int);\n" name)
    out.primitives;
  (* Never called: a direct call makes wasm-ld report the C functions
     whose definition does not have the signature of the call, which would
     trap when called through a function pointer *)
  Buffer.add_string b "\nvoid camlc_signature_check(void) {\n";
  String.Hashtbl.iter
    (fun name n ->
      if n <= 5
      then
        Printf.bprintf
          b
          "  %s(%s);\n"
          name
          (String.concat ~sep:", " (List.init ~len:n ~f:(fun _ -> "0")))
      else Printf.bprintf b "  %s((value *)0, 0);\n" name)
    out.primitives;
  Buffer.add_string b "}\n";
  Buffer.add_char b '\n';
  List.iter functions ~f:(fun (f, _, _) ->
      Printf.bprintf
        b
        "static value %s(%s);\n"
        (fun_name out f)
        (String.concat
           ~sep:", "
           (List.map (Var.Hashtbl.find out.fun_reprs f @ [ Value ]) ~f:c_type)));
  let dt = double_translation () in
  for m = 1 to out.max_arity - 1 do
    Printf.bprintf b "static value camlc_pap%d(%s);\n" m (fun_params m);
    if dt
    then Printf.bprintf b "static value camlc_pap_cps%d(%s);\n" m (fun_params (m + 1))
  done;
  Buffer.add_char b '\n';
  Printf.bprintf b "const int camlc_global_data_size = %d;\n" out.global_data_size;
  Printf.bprintf b "const int camlc_max_arity = %d;\n" out.max_arity;
  Printf.bprintf
    b
    "const value camlc_pap_code[] = { 0%s };\n"
    (String.concat
       ~sep:""
       (List.init ~len:(out.max_arity - 1) ~f:(fun i ->
            Printf.sprintf ", (value)&camlc_pap%d" (i + 1))));
  Printf.bprintf b "const int camlc_double_translation = %d;\n" (if dt then 1 else 0);
  Printf.bprintf
    b
    "const value camlc_pap_cps_code[] = { 0%s };\n\n"
    (if dt
     then
       String.concat
         ~sep:""
         (List.init ~len:(out.max_arity - 1) ~f:(fun i ->
              Printf.sprintf ", (value)&camlc_pap_cps%d" (i + 1)))
     else "");
  Buffer.add_string b "static struct camlc_data {\n  value unused;\n";
  Buffer.add_buffer b out.data_fields;
  Buffer.add_string b "} camlc_data = {\n  .unused = 0,\n";
  Buffer.add_buffer b out.data;
  Buffer.add_string
    b
    "};\n\n\
     value *const camlc_static_data_begin = (value *)&camlc_data;\n\
     value *const camlc_static_data_end = (value *)(&camlc_data + 1);\n\n";
  Buffer.add_buffer b out.location_defs;
  Buffer.add_char b '\n';
  (* Partial applications: the closure holds the function and the
     arguments already given (after the code pointer of the CPS version, with
     effect handlers), see [camlc_make_pap] *)
  let e = if dt then 3 else 2 in
  for m = 1 to out.max_arity - 1 do
    Printf.bprintf
      b
      "static value camlc_pap%d(%s) {\n\
      \  mlsize_t k = Wosize_val(env) - %d;\n\
      \  value *argv = camlc_sp - (k + %d);\n\
      \  value r;\n\
      \  if (CAMLC_UNLIKELY(argv < camlc_stack_limit)) return camlc_stack_overflow();\n\
      \  camlc_sp = argv;\n\
      \  for (mlsize_t i = 0; i < k; i++) argv[i] = Field(env, %d + i);\n\
       %s  r = camlc_applyv(Field(env, %d), k + %d, argv);\n\
      \  camlc_sp = argv + k + %d;\n\
      \  return r;\n\
       }\n\n"
      m
      (fun_params m)
      (e + 1)
      m
      (e + 1)
      (String.concat
         ~sep:""
         (List.init ~len:m ~f:(fun i -> Printf.sprintf "  argv[k + %d] = a%d;\n" i i)))
      e
      m
      m;
    if dt
    then
      (* CPS version: the last argument is the continuation *)
      Printf.bprintf
        b
        "static value camlc_pap_cps%d(%s) {\n\
        \  mlsize_t k = Wosize_val(env) - 4;\n\
        \  value *argv = camlc_sp - (k + %d);\n\
        \  value r;\n\
        \  if (CAMLC_UNLIKELY(argv < camlc_stack_limit)) return camlc_stack_overflow();\n\
        \  camlc_sp = argv;\n\
        \  for (mlsize_t i = 0; i < k; i++) argv[i] = Field(env, 4 + i);\n\
         %s  r = camlc_applyv_cps(Field(env, 3), k + %d, argv);\n\
        \  camlc_sp = argv + k + %d;\n\
        \  return r;\n\
         }\n\n"
        m
        (fun_params (m + 1))
        (m + 1)
        (String.concat
           ~sep:""
           (List.init ~len:(m + 1) ~f:(fun i ->
                Printf.sprintf "  argv[k + %d] = a%d;\n" i i)))
        m
        (m + 1)
  done;
  (* Exact calls with the arguments in an array *)
  Printf.bprintf
    b
    "value camlc_call_exact(value f, int n, value *args) {\n  switch (n) {\n";
  for n = 1 to out.max_arity do
    Printf.bprintf
      b
      "  case %d: return ((%s)Field(f, 0))(%s);\n"
      n
      (fun_type n)
      (String.concat
         ~sep:", "
         (List.init ~len:n ~f:(fun i -> Printf.sprintf "args[%d]" i) @ [ "f" ]))
  done;
  Printf.bprintf
    b
    "  default: caml_fatal_error(\"c_of_ocaml: arity %%d\", n);\n  }\n}\n\n";
  (* Same, for the CPS versions, the last argument being the continuation *)
  Printf.bprintf
    b
    "value camlc_call_exact_cps(value f, int n, value *args) {\n  switch (n) {\n";
  if dt
  then
    for n = 1 to out.max_arity do
      Printf.bprintf
        b
        "  case %d: return ((%s)Field(f, 2))(%s);\n"
        n
        (fun_type (n + 1))
        (String.concat
           ~sep:", "
           (List.init ~len:(n + 1) ~f:(fun i -> Printf.sprintf "args[%d]" i) @ [ "f" ]))
    done;
  Printf.bprintf
    b
    "  default: caml_fatal_error(\"c_of_ocaml: arity %%d\", n);\n  }\n}\n\n";
  (* CPS calls of unknown functions, which may not have a CPS version *)
  IntSet.iter
    (fun m ->
      let n = m - 1 in
      let args = List.init ~len:n ~f:(fun i -> Printf.sprintf "a%d" i) in
      let params = String.concat ~sep:", " (List.map args ~f:(fun a -> "value " ^ a)) in
      Printf.bprintf
        b
        "static value camlc_direct_k%d(value f, %s, value k) {\n\
        \  value *ks = camlc_sp - 1, r;\n\
        \  if (CAMLC_UNLIKELY(ks < camlc_stack_limit)) return camlc_stack_overflow();\n\
        \  *ks = k;\n\
        \  camlc_sp = ks;\n\
        \  r = ((%s)Field(f, 0))(%s, f);\n\
        \  camlc_sp = ks + 1;\n\
        \  if (CAMLC_UNLIKELY(Is_exception_result(r))) return r;\n\
        \  return ((value (*)(value, value))Field(*ks, 0))(r, *ks);\n\
         }\n\n\
         static inline value camlc_call_cps%d(value f, %s, value k) {\n\
        \  value c = Field(f, 2);\n\
        \  if (c) CAMLC_MUSTTAIL return ((%s)c)(%s, k, f);\n\
        \  CAMLC_MUSTTAIL return camlc_direct_k%d(f, %s, k);\n\
         }\n\n\
         static inline value camlc_apply_cps%d(value f, %s, value k) {\n\
        \  if (Arity_closinfo(Closinfo_val(f)) == %d)\n\
        \    CAMLC_MUSTTAIL return camlc_call_cps%d(f, %s, k);\n\
        \  value args[%d] = { %s, k };\n\
        \  return camlc_applyv_cps(f, %d, args);\n\
         }\n\n"
        n
        params
        (fun_type n)
        (String.concat ~sep:", " args)
        m
        params
        (fun_type m)
        (String.concat ~sep:", " args)
        n
        (String.concat ~sep:", " args)
        m
        params
        n
        m
        (String.concat ~sep:", " args)
        m
        (String.concat ~sep:", " args)
        n)
    out.cps_apply_arities;
  IntSet.iter
    (fun n ->
      let args = List.init ~len:n ~f:(fun i -> Printf.sprintf "a%d" i) in
      Printf.bprintf
        b
        "static inline value camlc_apply%d(value f, %s) {\n\
        \  if (Arity_closinfo(Closinfo_val(f)) == %d)\n\
        \    CAMLC_MUSTTAIL return ((%s)Field(f, 0))(%s, f);\n\
        \  value args[%d] = { %s };\n\
        \  return camlc_applyv(f, %d, args);\n\
         }\n\n"
        n
        (String.concat ~sep:", " (List.map args ~f:(fun a -> "value " ^ a)))
        n
        (fun_type n)
        (String.concat ~sep:", " args)
        n
        (String.concat ~sep:", " args)
        n)
    out.apply_arities;
  Buffer.add_buffer b out.functions;
  Buffer.contents b
