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

(* The primitives: their translation into C, and what is known of them
   (whether they raise or trigger a garbage collection, their types) *)

open! Stdlib
open Code
open! C_repr
open! C_output

(* Primitives *)

type prim =
  | Plain of repr * string * bool
      (** A C expression of this representation, which cannot raise, and
          whether it may trigger a GC. It may allocate only if it is a single
          call whose arguments are the arguments of the primitive *)
  | Checked of string
      (** A single call whose arguments are the arguments of the primitive,
          returning a value or an exception result *)
  | Guarded of string * repr * string
      (** A check, returning [Val_unit] or an exception result, then a C
          expression of this representation, which cannot raise nor trigger
          a GC *)
  | Ccallv  (** A C primitive with more than 5 arguments *)

(* An argument of a primitive: its type, and its C expression in a given
   representation *)
type parg =
  { a_typ : Typing.typ
  ; a_const : constant option
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
    ; "caml_string_lessthan"
    ; "caml_string_lessequal"
    ; "caml_string_greaterthan"
    ; "caml_string_greaterequal"
    ; "caml_bytes_equal"
    ; "caml_bytes_notequal"
    ; "caml_bytes_compare"
    ; "caml_bytes_lessthan"
    ; "caml_bytes_lessequal"
    ; "caml_bytes_greaterthan"
    ; "caml_bytes_greaterequal"
    ; "caml_blit_string"
    ; "caml_blit_bytes"
    ; "caml_fill_bytes"
    ; "caml_obj_dup"
    ; "caml_array_unsafe_set"
    ; "caml_hash"
    ; "caml_obj_tag"
    ; "caml_lxm_next"
    ; "caml_fresh_oo_id"
    ; "caml_atomic_load"
    ; "caml_atomic_exchange"
    ; "caml_atomic_cas"
    ; "caml_atomic_fetch_add"
    ; "caml_atomic_cas_field"
    ; "caml_atomic_exchange_field"
    ; "caml_atomic_fetch_add_field"
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

(* The float functions of the C library which the primitives of the
   runtime call (with emscripten's libm, which conforms to C99:
   HAS_C99_FLOAT_OPS, HAS_WORKING_ROUND, HAS_WORKING_FMA), on unboxed
   arguments *)
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
  ; "caml_round_float", "round"
  ; "caml_exp2_float", "exp2"
  ; "caml_expm1_float", "expm1"
  ; "caml_log10_float", "log10"
  ; "caml_log2_float", "log2"
  ; "caml_log1p_float", "log1p"
  ; "caml_cbrt_float", "cbrt"
  ; "caml_sinh_float", "sinh"
  ; "caml_cosh_float", "cosh"
  ; "caml_tanh_float", "tanh"
  ; "caml_asinh_float", "asinh"
  ; "caml_acosh_float", "acosh"
  ; "caml_atanh_float", "atanh"
  ; "caml_erf_float", "erf"
  ; "caml_erfc_float", "erfc" (* Two arguments *)
  ; "caml_atan2_float", "atan2"
  ; "caml_hypot_float", "hypot"
  ; "caml_power_float", "pow"
  ; "caml_fmod_float", "fmod"
  ; "caml_copysign_float", "copysign"
  ; "caml_nextafter_float", "nextafter" (* Three *)
  ; "caml_fma_float", "fma"
  ]

(* The functions of the C library the generated code may call (beyond the
   primitives and the runtime): those of [float_functions], and those of
   camlc.h *)
let plugin_c_functions = List.map float_functions ~f:snd @ [ "memcpy" ]

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

(* The elements of a bigarray of a given kind: their C type, their
   representation, the expression of the element as this representation,
   and the expression to store from a [parg] *)
let bigarray_element (kind : Optimization_hint.Bigarray.kind) =
  let int t =
    ( t
    , Value
    , Printf.sprintf "Val_long(%s)"
    , fun a -> Printf.sprintf "Long_val(%s)" (a.get Value) )
  in
  let number t r = t, r, Fun.id, fun a -> a.get r in
  match kind with
  | Float32 -> number "float" Double
  | Float64 -> number "double" Double
  | Int8_signed -> int "int8_t"
  | Int8_unsigned -> int "uint8_t"
  | Int16_signed -> int "int16_t"
  | Int16_unsigned -> int "uint16_t"
  | Int32 -> number "int32_t" Unboxed_int32
  | Int64 -> number "int64_t" Unboxed_int64
  | Int -> int "intnat"
  | Nativeint -> number "intnat" Unboxed_nativeint
  | Float16 | Float32_t | Complex32 | Complex64 -> assert false

let primitive out name (args : parg list) =
  let n = List.length args in
  let arg r i = (List.nth args i).get r in
  (* The name of a compilation unit *)
  let unit_name i =
    match (List.nth args i).a_const with
    | Some (String s | NativeString (Byte s | Utf (Utf8 s))) -> s
    | _ -> failwith (Printf.sprintf "c_of_ocaml: %s: unknown unit" name)
  in
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
  | _, _, Some f, _ ->
      pure
        ( Double
        , Printf.sprintf
            "%s(%s)"
            f
            (String.concat ~sep:", " (List.init ~len:n ~f:(fun i -> arg Double i))) )
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
      | None, Some (r, (("div" | "mod") as op)) when n = 2 ->
          Guarded
            ( Printf.sprintf "camlc_check_divisor(%s == 0)" (arg r 1)
            , r
            , if String.equal op "div"
              then
                Printf.sprintf
                  "Camlc_boxed_div(%s, %s, %s, %s)"
                  (c_type r)
                  (unsigned_type r)
                  (arg r 0)
                  (arg r 1)
              else
                Printf.sprintf
                  "Camlc_boxed_mod(%s, %s, %s)"
                  (c_type r)
                  (arg r 0)
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
      | None, Some (r, "bswap") when n = 1 ->
          pure
            ( r
            , Printf.sprintf
                "(%s)__builtin_bswap%d((%s)%s)"
                (c_type r)
                (bits r)
                (unsigned_type r)
                (arg r 0) )
      | None, Some (Unboxed_int64, "bits_of_float") when n = 1 ->
          pure (Unboxed_int64, Printf.sprintf "camlc_bits_of_double(%s)" (arg Double 0))
      | None, Some (Unboxed_int64, "float_of_bits") when n = 1 ->
          pure (Double, Printf.sprintf "camlc_double_of_bits(%s)" (arg Unboxed_int64 0))
      | _ -> (
          match name, n with
          | "caml_bswap16", 1 ->
              pure
                ( Value
                , Printf.sprintf
                    "Val_int(__builtin_bswap16((uint16_t)Long_val(%s)))"
                    (value 0) )
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
          | ( ( "caml_ba_get_1"
              | "caml_ba_get_2"
              | "caml_ba_get_3"
              | "caml_ba_set_1"
              | "caml_ba_set_2"
              | "caml_ba_set_3" )
            , _ )
            when Option.is_some (Typing.specialized_bigarray (List.hd args).a_typ) ->
              (* Direct access, the kind and layout of the bigarray being known *)
              let { Optimization_hint.Bigarray.kind; layout; _ } =
                Option.get (Typing.specialized_bigarray (List.hd args).a_typ)
              in
              let get = Char.equal name.[8] 'g' in
              let dims = if get then n - 1 else n - 2 in
              let location =
                [ value 0
                ; string_of_int dims
                ; (match layout with
                  | C -> "0"
                  | Fortran -> "1")
                ]
                @ List.init ~len:3 ~f:(fun d ->
                    if d < dims then value (d + 1) else "Val_unit")
              in
              let t, r, load, store = bigarray_element kind in
              let elt = c_call "camlc_ba_elt" (t :: location) in
              if get
              then Guarded (c_call "camlc_ba_check_get" location, r, load elt)
              else
                Guarded
                  ( c_call "camlc_ba_check_set" location
                  , Value
                  , Printf.sprintf
                      "(%s = %s, Val_unit)"
                      elt
                      (store (List.nth args (n - 1))) )
          | ("caml_string_get32" | "caml_bytes_get32"), 2 ->
              Guarded
                ( c_call "camlc_check_bytes_access" [ value 0; value 1; "4" ]
                , Unboxed_int32
                , c_call "camlc_bytes_load32" [ value 0; value 1 ] )
          | ("caml_string_get64" | "caml_bytes_get64"), 2 ->
              Guarded
                ( c_call "camlc_check_bytes_access" [ value 0; value 1; "8" ]
                , Unboxed_int64
                , c_call "camlc_bytes_load64" [ value 0; value 1 ] )
          | "caml_bytes_set32", 3 ->
              Guarded
                ( c_call "camlc_check_bytes_access" [ value 0; value 1; "4" ]
                , Value
                , c_call "camlc_bytes_store32" [ value 0; value 1; arg Unboxed_int32 2 ]
                )
          | "caml_bytes_set64", 3 ->
              Guarded
                ( c_call "camlc_check_bytes_access" [ value 0; value 1; "8" ]
                , Value
                , c_call "camlc_bytes_store64" [ value 0; value 1; arg Unboxed_int64 2 ]
                )
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
                  plain (Printf.sprintf "Val_long(Camlc_tag_val(%s))" (arg 0))
              | ("caml_ml_string_length" | "caml_ml_bytes_length"), 1 ->
                  plain (Printf.sprintf "Val_long(camlc_string_length(%s))" (arg 0))
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
              | ("caml_string_get16" | "caml_bytes_get16"), 2 ->
                  Guarded
                    ( c_call "camlc_check_bytes_access" (args @ [ "2" ])
                    , Value
                    , c_call "camlc_bytes_load16" args )
              | "caml_bytes_set16", 3 ->
                  Guarded
                    ( c_call "camlc_check_bytes_access" [ arg 0; arg 1; "2" ]
                    , Value
                    , c_call "camlc_bytes_store16" args )
              | ( ( "caml_ba_get_1"
                  | "caml_ba_get_2"
                  | "caml_ba_get_3"
                  | "caml_ba_set_1"
                  | "caml_ba_set_2"
                  | "caml_ba_set_3"
                  | "caml_ba_dim_1"
                  | "caml_ba_dim_2"
                  | "caml_ba_dim_3" )
                , _ ) ->
                  Checked
                    (c_call
                       ("camlc" ^ String.sub name ~pos:4 ~len:(String.length name - 4))
                       args)
              | ( ("caml_check_bound" | "caml_check_bound_float" | "caml_check_bound_gen")
                , 2 ) ->
                  Guarded
                    ( c_call
                        ("camlc" ^ String.sub name ~pos:4 ~len:(String.length name - 4))
                        args
                    , Value
                    , arg 0 )
              | "caml_array_unsafe_get", 2 -> plain (c_call "camlc_array_unsafe_get" args)
              | "caml_array_unsafe_set_addr", 3 ->
                  plain
                    (Printf.sprintf
                       "(camlc_modify(&Field(%s, Long_val(%s)), %s), Val_unit)"
                       (arg 0)
                       (arg 1)
                       (arg 2))
              | "caml_string_concat", 2 -> plain (c_call "camlc_string_concat" args)
              | "caml_format_int", 2 -> Checked (c_call "camlc_format_int" args)
              | "caml_create_bytes", 1 -> Checked (c_call "camlc_create_bytes" args)
              | "caml_int_of_string", 1 -> Checked (c_call "camlc_int_of_string" args)
              | ( ( "caml_compare"
                  | "caml_equal"
                  | "caml_notequal"
                  | "caml_lessthan"
                  | "caml_lessequal"
                  | "caml_greaterthan"
                  | "caml_greaterequal" )
                , 2 ) ->
                  Checked
                    (c_call
                       ("camlc_" ^ String.sub name ~pos:5 ~len:(String.length name - 5))
                       args)
              (* The compilation units are registered by name with --dynlink
                 and in plugins, and found by name in plugins *)
              | "caml_register_global", 2 ->
                  plain
                    (Printf.sprintf
                       "camlc_register_unit(%s, %s)"
                       (c_string (unit_name 1))
                       (arg 0))
              | "caml_get_global", 1 ->
                  Checked (Printf.sprintf "camlc_get_unit(%s)" (c_string (unit_name 0)))
              | "caml_get_global_predef", 1 ->
                  let exn = unit_name 0 in
                  let rec index i =
                    if i = Array.length Runtimedef.builtin_exceptions
                    then failwith (Printf.sprintf "c_of_ocaml: unknown exception %s" exn)
                    else if String.equal Runtimedef.builtin_exceptions.(i) exn
                    then i
                    else index (i + 1)
                  in
                  plain (Printf.sprintf "Field(caml_global_data, %d)" (index 0))
              | "caml_register_global_predef", _ -> plain "Val_unit"
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
                  plain (c_call "camlc_get_method_cached" args)
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
              (* A dummy closure for a [let rec] (before OCaml 5.2): its
                 size is that of the closure of the bytecode, which is
                 smaller with double translation (see the runtime) *)
              | "caml_alloc_dummy_function", 2 ->
                  Checked (c_call "camlc_ccall2" ("camlc_alloc_dummy_function" :: args))
              (* [Gc.ramp_up] (OCaml 5.4) calls its function back from
                 the runtime's C, which the code calls without JavaScript
                 in between: the function keeps the segments of the stack
                 of the caller (see the runtime) *)
              | "caml_ml_gc_ramp_up", 1 ->
                  Checked (c_call "camlc_ccall1" ("camlc_ml_gc_ramp_up" :: args))
              (* The write barrier of the code is cheaper until a
                 domain is spawned (see the runtime) *)
              | "caml_domain_spawn", 2 ->
                  Checked (c_call "camlc_ccall2" ("camlc_domain_spawn" :: args))
              (* With [Marshal.Closures], marshaled by the runtime *)
              | ( ( "caml_output_value"
                  | "caml_output_value_to_bytes"
                  | "caml_output_value_to_string"
                  | "caml_output_value_to_buffer" )
                , _ ) ->
                  Checked
                    (c_call
                       (Printf.sprintf "camlc_ccall%d" n)
                       (("camlc" ^ String.sub name ~pos:4 ~len:(String.length name - 4))
                       :: args))
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
    ; "caml_string_get16"
    ; "caml_bytes_get16"
    ; "caml_bswap16"
    ; "caml_ba_dim_1"
    ; "caml_ba_dim_2"
    ; "caml_ba_dim_3"
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

(* The types of the primitives computed on unboxed numbers, for {!Typing}:
   whether they take unboxed arguments, and their result *)
let register_typing_primitives () =
  let float = Typing.Number (Float, Unboxed) in
  let int = Typing.Int Normalized in
  (* The primitives returning integers, unless their type is more precise
     below *)
  StringSet.iter
    (fun name -> Typing.register_prim name ~unbox:false (Typing.Int Unnormalized))
    int_primitives;
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
        ; "div"
        ; "mod"
        ; "and"
        ; "or"
        ; "xor"
        ; "neg"
        ; "shift_left"
        ; "shift_right"
        ; "shift_right_unsigned"
        ; "of_float"
        ; "bswap"
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
  Typing.register_prim "caml_int64_float_of_bits" ~unbox:true float;
  List.iter
    [ "caml_string_get32", Typing.Int32
    ; "caml_bytes_get32", Int32
    ; "caml_string_get64", Int64
    ; "caml_bytes_get64", Int64
    ]
    ~f:(fun (name, n) -> Typing.register_prim name ~unbox:false (Number (n, Unboxed)));
  List.iter [ "caml_bytes_set32"; "caml_bytes_set64" ] ~f:(fun name ->
      Typing.register_prim name ~unbox:true (Typing.Int Normalized))
