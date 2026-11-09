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

   Each function of the program becomes a C function. Its variables are C
   variables; those live where the GC may run are also stored in a frame
   on the shadow stack ([fp[i]]), so that they are seen and updated by the
   GC, and reloaded from it after. The variables of the enclosing functions
   are read from the closure. Blocks are labels, and branches are gotos.
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

(* The field of [union camlc_word] holding a number in representation [r] *)
let repr_letter r =
  match r with
  | Value -> 'v'
  | Double -> 'd'
  | Unboxed_int64 -> 'l'
  | Unboxed_int32 -> 'i'
  | Unboxed_nativeint -> 'n'

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
  | Unboxed_int64 -> Printf.sprintf "camlc_box_int64(%s)" e
  | Unboxed_int32 -> Printf.sprintf "camlc_box_int32(%s)" e
  | Unboxed_nativeint -> Printf.sprintf "camlc_box_nativeint(%s)" e

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
  { data : Buffer.t  (** Definitions of the static data *)
  ; static_names : string String.Hashtbl.t
        (** The contents of each block of static data, by C name *)
  ; mutable static_list : string list  (** The blocks of static data, in reverse order *)
  ; mutable definitions : definition list  (** Function definitions, in reverse order *)
  ; mutable refs : Var.Set.t  (** The functions referred to by the current code *)
  ; constants : (constant * string) list Int.Hashtbl.t
        (** Static data already emitted for each constant *)
  ; primitives : int String.Hashtbl.t  (** C primitives called, with their arity *)
  ; mutable apply_arities : IntSet.t  (** Arities of the [camlc_applyN] functions used *)
  ; mutable max_arity : int  (** Maximal arity of the functions *)
  ; global_data_size : int
  ; fun_names : string Var.Hashtbl.t  (** C name of each function *)
  ; mutable types : Typing.t
  ; fun_reprs : repr list Var.Hashtbl.t
        (** The representation of the parameters of each function *)
  ; fun_returns : repr Var.Hashtbl.t
        (** The representation of the result of each function *)
  ; debug_info : bool  (** Record backtraces *)
  ; locations : string String.Hashtbl.t  (** The initializer of each location, by name *)
  ; segment_thunks : unit String.Hashtbl.t
        (** The thunks calling a function on a new segment of the stack, by name *)
  ; mutable components : int Var.Hashtbl.t
        (** The recursive functions, by strongly connected component of the
            graph of known calls *)
  ; mutable unknown_tail_calls : Var.Set.t
        (** The functions which may make an unknown tail call, by themselves
            or through known tail calls *)
  ; mutable cps_fallback : Var.Set.t
        (** With --cps-stack, the functions which may go on in their CPS version *)
  ; in_cps : Var.Set.t  (** The CPS calls *)
  ; trampolined : Var.Set.t  (** The CPS calls which may bounce *)
  ; cps_pairs : Var.t Var.Map.t
        (** The CPS version of each function with two versions, which share a
            closure *)
  ; cps_sides : Var.Set.t  (** The CPS versions *)
  ; mutable cps_apply_arities : IntSet.t
        (** Arities (with the continuation) of the [camlc_apply_cpsN] and
            [camlc_call_cpsN] functions used *)
  ; globals : string Var.Hashtbl.t  (** The C name of each global *)
  ; statics : string Var.Hashtbl.t
        (** The closed closures and the constants, which are not free
            variables *)
  ; split : bool  (** The code is split into several files *)
  ; polling : bool
        (** The program uses threads or domains, or [Gc.Memprof]: the code
            polls (see [polls]) *)
  ; memprof : bool
        (** The program uses [Gc.Memprof]: an allocation does not poll (see
            [polls]) *)
  ; mutable raising : Var.Set.t  (** The functions which may raise *)
  ; mutable collecting : Var.Set.t
        (** The functions which may trigger a garbage collection and return *)
  ; mutable collecting_on_raise : Var.Set.t
        (** The functions which may trigger a garbage collection only when
            they raise *)
  }

(* The C definition of a function, and the functions it refers to *)
and definition =
  { def_fn : Var.t option  (** [None] for the toplevel code *)
  ; def_name : string  (** The C name of the function *)
  ; def_text : string  (** The definition, without storage class *)
  ; def_size : int
  ; def_refs : Var.Set.t
  }

(* The C names of the static data, the locations and the globals do not
   depend on the rest of the program either (see [name_functions]): they
   are named after a digest of their contents, or of a stable key *)
let digest_name prefix s =
  prefix ^ String.sub (Digest.to_hex (Digest.string s)) ~pos:0 ~len:16

(* A static description of a source location, for backtraces, defined in
   each part which uses it *)
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
  let init =
    Printf.sprintf
      "{ %s, %s, %d, %d, %d, %d }"
      (c_string file)
      (c_string defname)
      pi.line
      pi.col
      pi.col
      (if is_raise then 1 else 0)
  in
  let name = digest_name "camlc_l_" init in
  String.Hashtbl.replace out.locations name init;
  "&" ^ name

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

(* The name of a function the current code refers to *)
let fun_ref out f =
  out.refs <- Var.Set.add f out.refs;
  fun_name out f

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

(* [static_block out words] adds a block (header and fields) to the static
   data, and returns its C name. The runtime finds the blocks in a table
   (OCaml 4 needs them in its page table). *)
let static_block out words =
  let contents = String.concat ~sep:", " words in
  let rec name rank =
    let n =
      digest_name
        "camlc_k_"
        (if rank = 0 then contents else Printf.sprintf "%d:%s" rank contents)
    in
    match String.Hashtbl.find_opt out.static_names n with
    | Some c when String.equal c contents -> n
    | Some _ -> name (rank + 1)
    | None ->
        String.Hashtbl.add out.static_names n contents;
        out.static_list <- n :: out.static_list;
        Printf.bprintf
          out.data
          "%svalue %s[%d] = { %s };\n"
          (if out.split then "" else "static ")
          n
          (List.length words)
          contents;
        n
  in
  name 0

let emit_block out ~tag fields =
  let n = static_block out (header ~wosize:(List.length fields) ~tag :: fields) in
  Printf.sprintf "((value)&%s[1])" n

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
  (* Definitions which imply an integer. Not the uses: a variable switched
     on or compared as an integer may be a block on other paths, after the
     test [IsInt] of a pattern matching *)
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
          | Let
              (x, Prim ((Vectlength _ | Not | IsInt _ | Eq _ | Neq _ | Lt | Le | Ult), _))
            -> set_int x
          | Let (x, Prim (Extern (name, _), _)) ->
              if StringSet.mem name int_primitives then set_int x
          | Offset_ref _ | Let _ | Assign _ | Set_field _ | Array_set _ | Event _ -> ()))
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
  | Global of string  (** A toplevel value stored in a C global *)
  | Static of string  (** A closed closure or a constant: static data *)

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
  ; fn : Var.t option  (** The function, [None] for the toplevel code *)
  ; arity : repr list option
        (** The representation of the parameters of the function, not the toplevel *)
  ; static_env : bool  (** The closure is static data *)
  ; locations : location Var.Hashtbl.t
  ; slots : int Var.Hashtbl.t  (** The variables in the frame *)
  ; traps : trap list Addr.Hashtbl.t  (** The handlers in scope in each block *)
  ; handler_ids : int Addr.Hashtbl.t  (** The handler of the [Pushtrap] ending a block *)
  ; local_names : int Var.Hashtbl.t
  ; labels : int Addr.Hashtbl.t
        (** The variables and the blocks are numbered in each function, so that
            the code of a function does not depend on the rest of the program *)
  ; buf : Buffer.t
  ; mutable position : Parse_info.t option  (** The last debug event *)
  ; source_name : string option  (** The name of the function in the source *)
  ; raises : bool  (** The function may raise *)
  ; ret : repr  (** The representation of its result *)
  ; mutable framed : bool  (** The frame is allocated in the current block *)
  ; mutable framed_blocks : Addr.Set.t option
        (** The blocks where the frame is allocated, if not all *)
  ; mutable frame_push : string  (** Allocates the frame *)
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

let location ctx x =
  match Var.Hashtbl.find_opt ctx.locations x with
  | Some _ as l -> l
  | None -> (
      match Var.Hashtbl.find_opt ctx.out.globals x with
      | Some g -> Some (Global g)
      | None -> (
          match Var.Hashtbl.find_opt ctx.out.statics x with
          | Some e -> Some (Static e)
          | None -> None))

let local_name ctx x =
  if Var.equal x ctx.env_var
  then "env"
  else
    let n =
      match Var.Hashtbl.find_opt ctx.local_names x with
      | Some n -> n
      | None ->
          let n = Var.Hashtbl.length ctx.local_names in
          Var.Hashtbl.add ctx.local_names x n;
          n
    in
    Printf.sprintf "v%d" n

let label ctx pc =
  let n =
    match Addr.Hashtbl.find_opt ctx.labels pc with
    | Some n -> n
    | None ->
        let n = Addr.Hashtbl.length ctx.labels in
        Addr.Hashtbl.add ctx.labels pc n;
        n
  in
  Printf.sprintf "L%d" n

(* A variable is read from its C variable. Those in the frame are also
   written to their slot when defined, and their C variable is reloaded
   from it after each point where the GC may run (see [reload]). *)
let local ctx x = local_name ctx x

let var ctx x =
  match location ctx x with
  | Some Local -> local ctx x
  | Some (Global g) -> g
  | Some (Static e) -> e
  | Some (Env i) -> Printf.sprintf "Field(%s, %d)" (local ctx ctx.env_var) i
  | Some (Sibling 0) -> local ctx ctx.env_var
  | Some (Sibling d) ->
      Printf.sprintf "((value)((value *)%s + %d))" (local ctx ctx.env_var) d
  | None -> failwith (Format.asprintf "c_of_ocaml: unbound variable %a" Var.print x)

let slot ctx x =
  match location ctx x with
  | Some Local -> (
      (* Assigned as [fp[i] = x = e] *)
      match Var.Hashtbl.find_opt ctx.slots x with
      | Some i -> Printf.sprintf "fp[%d] = %s" i (local_name ctx x)
      | None -> local ctx x)
  | Some (Global g) -> g
  | Some (Env _ | Sibling _ | Static _) | None ->
      failwith (Format.asprintf "c_of_ocaml: variable %a is not local" Var.print x)

(* The local variables used by an expression, the variables of the
   enclosing functions standing for the closure *)
let use ctx x acc =
  match location ctx x with
  | Some Local -> Var.Set.add x acc
  | Some (Env _ | Sibling _) -> Var.Set.add ctx.env_var acc
  | Some (Global _ | Static _) | None -> acc

let use_list ctx l acc = List.fold_left l ~init:acc ~f:(fun acc x -> use ctx x acc)

(* Reload the C variables of the variables in the frame among the variables
   [l] (the closure standing for itself) or the closure they are read
   from *)
let reload ctx l =
  String.concat
    ~sep:""
    (List.filter_map
       (Var.Set.elements
          (List.fold_left l ~init:Var.Set.empty ~f:(fun acc x ->
               if Var.equal x ctx.env_var then Var.Set.add x acc else use ctx x acc)))
       ~f:(fun x ->
         match Var.Hashtbl.find_opt ctx.slots x with
         | Some i -> Some (Printf.sprintf "%s = fp[%d]; " (local_name ctx x) i)
         | None -> None))

let var_repr ctx x =
  if Var.equal x ctx.env_var
  then Value
  else
    match location ctx x with
    | Some Local -> repr_of_type (Typing.var_type ctx.out.types x)
    | Some (Env _ | Sibling _ | Global _ | Static _) | None -> Value

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
  | Pv x ->
      { a_typ = Typing.var_type ctx.out.types x
      ; a_const = None
      ; get = (fun r -> as_repr ctx r x)
      }
  | Pc c ->
      { a_typ = Typing.constant_type c
      ; a_const = Some c
      ; get = (fun r -> constant_repr ctx.out c r)
      }

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

(* Return [e], in representation [r] *)
let return_stmt ?(r = Value) ctx e =
  if not (repr_equal r ctx.ret)
  then failwith "c_of_ocaml: a function returning an unboxed number returns a value";
  if not ctx.framed
  then Printf.sprintf "return %s;" e
  else
    Printf.sprintf "{ %s res = %s; camlc_sp = fp + FRAME_SIZE; return res; }" (c_type r) e

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

(* An overflow of the shadow stack *)
let overflow_stmt ctx traps =
  if List.is_empty traps && not ctx.raises
  then "camlc_fatal_stack_overflow();"
  else raise_stmt ctx traps "camlc_stack_overflow()"

let branch ctx (pc, args) =
  let params = (Addr.Map.find pc ctx.blocks).params in
  let moves =
    List.filter_map (List.combine params args) ~f:(fun (p, a) ->
        if Var.equal p a then None else Some (p, a))
  in
  (* Entering the blocks where the frame is allocated *)
  let push =
    match ctx.framed_blocks with
    | Some s when (not ctx.framed) && Addr.Set.mem pc s -> ctx.frame_push
    | Some _ | None -> ""
  in
  match moves with
  | [] when String.equal push "" -> Printf.sprintf "goto %s;" (label ctx pc)
  | [] -> Printf.sprintf "{ %s goto %s; }" push (label ctx pc)
  | [ (p, a) ] ->
      Printf.sprintf
        "{ %s%s = %s; goto %s; }"
        push
        (slot ctx p)
        (as_repr ctx (var_repr ctx p) a)
        (label ctx pc)
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
        "{ %s%s %s goto %s; }"
        push
        (String.concat ~sep:" " temps)
        (String.concat ~sep:" " assigns)
        (label ctx pc)

(* Allocate a block whose fields are [fields] (C expressions, read after
   the allocation), and store it in [dst] *)
let alloc_block ?(reload = "") ctx ~dst ~tag ~prefix fields =
  let wosize = List.length prefix + List.length fields in
  if wosize <= 256
  then (
    line ctx "b = camlc_alloc_small(%d, %d);" wosize tag;
    if not (String.equal reload "") then line ctx "%s" reload;
    List.iteri prefix ~f:(fun i e -> line ctx "Field(b, %d) = %s;" i e);
    List.iteri fields ~f:(fun i e ->
        line ctx "Field(b, %d) = %s;" (i + List.length prefix) e))
  else (
    line ctx "b = caml_alloc_shr(%d, %d);" wosize tag;
    if not (String.equal reload "") then line ctx "%s" reload;
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

(* The code pointers and headers of a closure, before its free variables *)
let closure_prefix out functions =
  let startenv, positions = closure_layout functions in
  List.concat
    (List.map2 functions positions ~f:(fun (g, arity) pos ->
         let code =
           [ Printf.sprintf "(value)&%s" (fun_ref out g)
           ; Printf.sprintf "Make_closinfo(%d, %d)" arity (startenv - pos)
           ]
         in
         let code =
           if double_translation ()
           then
             code
             @ [ (match Var.Map.find_opt g out.cps_pairs with
                 | Some c -> Printf.sprintf "(value)&%s" (fun_ref out c)
                 | None -> "CAMLC_NO_CPS")
               ]
           else if List.length functions > 1 && arity >= 2
           then code @ [ "Val_unit" ]
           else code
         in
         if pos = 0
         then code
         else Printf.sprintf "Make_header(%d, Infix_tag, 0)" pos :: code))

(* A closed closure is static data: the address of each function *)
let static_closure out functions =
  let prefix = closure_prefix out functions in
  let n =
    static_block out (header ~wosize:(List.length prefix) ~tag:Obj.closure_tag :: prefix)
  in
  let _, positions = closure_layout functions in
  List.map2 functions positions ~f:(fun (g, _) pos ->
      g, Printf.sprintf "(value)&%s[%d]" n (pos + 1))

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
      if List.is_empty info.free_variables
      then
        List.iter functions ~f:(fun (g, _) ->
            line ctx "%s = %s;" (slot ctx g) (Var.Hashtbl.find ctx.out.statics g))
      else
        let _, positions = closure_layout functions in
        alloc_block
          ~reload:(reload ctx info.free_variables)
          ctx
          ~dst:(slot ctx f)
          ~tag:Obj.closure_tag
          ~prefix:(closure_prefix ctx.out functions)
          (List.map info.free_variables ~f:(fun x -> as_value ctx x));
        List.iter2 functions positions ~f:(fun (g, _) pos ->
            if pos > 0 then line ctx "%s = (value)((value *)b + %d);" (slot ctx g) pos)

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

let resolve_callee global_flow_info closures x ~f ~exact =
  if exact
  then
    match Global_flow.get_unique_closure global_flow_info ~call:x f with
    | Some (g, _, cont) when Var.Map.mem g closures ->
        if cont then Known_direct g else Known g
    | Some _ | None -> Unknown
  else Unknown

let callee ctx x ~f ~exact = resolve_callee ctx.global_flow_info ctx.closures x ~f ~exact

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
  | Known g -> fun_ref ctx.out g, args @ [ fe ], reprs
  | Known_direct g ->
      let args, k = List.take (n - 1) args in
      fun_ref ctx.out g, args @ [ fe ] @ k, reprs
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
    line ctx "if (CAMLC_UNLIKELY(bx < camlc_stack_limit)) %s" (overflow_stmt ctx traps);
    line ctx "camlc_sp = bx;";
    List.iteri boxed ~f:(fun i _ -> line ctx "bx[%d] = Val_unit;" i);
    List.iteri boxed ~f:(fun i a ->
        let r = var_repr ctx a in
        line ctx "bx[%d] = %s;" i (box r (var ctx a)));
    line ctx "%s" (reload ctx (f :: args)));
  k

(* Whether a call of [fn] may be recursive: an unknown call, or a call of a
   function of its strongly connected component *)
let recursive_call out global_flow_info closures ~fn (x, f, exact) =
  match resolve_callee global_flow_info closures x ~f ~exact with
  | Known g -> (
      match fn with
      | Some fn -> (
          match
            Var.Hashtbl.find_opt out.components fn, Var.Hashtbl.find_opt out.components g
          with
          | Some c, Some c' -> c = c'
          | _ -> false)
      | None -> false)
  | Known_direct _ | Unknown -> true

(* An application whose result is returned: the frame is released before
   the call, which is a tail call when the callee has the signature of the
   function (as required by [musttail]). Otherwise, it would be a tail call
   only if the function had no other return: clang merges the returns, and
   the backend of wasm does not split them again for the call. With
   [CAMLC_TAIL_CALLS], the call then goes through a function of the
   signature of the caller ([camlc_t_...], defined by [declare_symbols]),
   called with [musttail], which finds the callee and its arguments in
   [camlc_seg] and makes the call, its only return: a tail call as well.
   Mutually recursive functions of different signatures then loop in
   constant space, as in OCaml, rather than growing the stack of the
   engine, out of reach of the frame checks (see [camlc_segment]). Only
   for the calls which may be recursive (see [recursive_call]), and those
   of functions which may make an unknown tail call (by themselves or
   through known tail calls: a loop of tail calls through closures, as the
   continuations of a function in CPS): the others end, and the C compiler
   may inline them.

   A CPS call which may bounce, unless it is a tail call (with
   [CAMLC_TAIL_CALLS]), first decrements
   [camlc_cps_depth]: when it gets negative, the call is returned to the
   trampoline ([camlc_cps_bounce]), which performs it on a shorter stack.
   Without [CAMLC_TAIL_CALLS], so do the calls of the continuations and the
   backward jumps of the CPS code ([camlc_cps_bounce_direct]). *)
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
  let ret =
    match kind with
    | Known g -> Var.Hashtbl.find ctx.out.fun_returns g
    | Known_direct _ | Unknown -> Value
  in
  (* The typing ensures that an unboxed result is never boxed *)
  if (not (repr_equal ret ctx.ret)) && repr_equal ctx.ret Value
  then failwith "c_of_ocaml: an unboxed result would need to be boxed";
  let musttail =
    match kind, ctx.arity with
    | (Known _ | Unknown), Some reprs' ->
        List.equal ~eq:repr_equal reprs reprs' && repr_equal ret ctx.ret
    | Known_direct _, _ | _, None -> false
  in
  let bounce =
    if
      Var.Set.mem x ctx.out.trampolined
      && (not (is_cps_call ctx x))
      && exact
      && List.for_all reprs ~f:(fun r -> repr_equal r Value)
    then
      (* A call of a continuation or of the closure of a block of CPS code,
         in direct style (the arguments, then the closure): a plain call
         without [CAMLC_TAIL_CALLS] *)
      let closure, arguments = n, List.init ~len:n ~f:Fun.id in
      Printf.sprintf
        " if (!CAMLC_TAIL_CALLS && CAMLC_UNLIKELY(--camlc_cps_depth < 0)) { %s return \
         camlc_cps_bounce_direct(%s, %d); }"
        (String.concat
           ~sep:" "
           (List.mapi arguments ~f:(fun i j ->
                Printf.sprintf "camlc_bounce_args[%d] = %s;" i (t j))))
        (t closure)
        n
    else if
      is_cps_call ctx x
      && Var.Set.mem x ctx.out.trampolined
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
        " if (%sCAMLC_UNLIKELY(--camlc_cps_depth < 0)) { %s return camlc_cps_bounce(%s, \
         %d); }"
        (* No need to bounce for a tail call *)
        (if musttail then "!CAMLC_TAIL_CALLS && " else "")
        (String.concat
           ~sep:" "
           (List.mapi arguments ~f:(fun i j ->
                Printf.sprintf "camlc_bounce_args[%d] = %s;" i (t j))))
        (t closure)
        n
    else ""
  in
  let pop =
    if not ctx.framed
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
      let through =
        match ctx.arity with
        | Some reprs'
          when (not musttail)
               && repr_equal ret ctx.ret
               && (recursive_call
                     ctx.out
                     ctx.global_flow_info
                     ctx.closures
                     ~fn:ctx.fn
                     (x, f, exact)
                  ||
                  match kind with
                  | Known g -> Var.Set.mem g ctx.out.unknown_tail_calls
                  | Known_direct _ | Unknown -> false) ->
            let callee_reprs =
              match kind with
              | Known _ -> reprs @ [ Value ]
              | Known_direct _ | Unknown -> List.map args ~f:(fun _ -> Value)
            in
            let caller_reprs = reprs' @ [ Value ] in
            let letters l =
              String.concat
                ~sep:""
                (List.map l ~f:(fun r -> String.make 1 (repr_letter r)))
            in
            let name =
              Printf.sprintf
                "camlc_t_%c_%s_%s"
                (repr_letter ret)
                (letters caller_reprs)
                (letters callee_reprs)
            in
            String.Hashtbl.replace ctx.out.segment_thunks name ();
            Printf.sprintf
              " if (CAMLC_TAIL_CALLS) { camlc_seg_fn = (void *)%s; %s CAMLC_MUSTTAIL \
               return %s(%s); }"
              callee
              (String.concat
                 ~sep:" "
                 (List.mapi callee_reprs ~f:(fun i r ->
                      Printf.sprintf "camlc_seg[%d].%c = t%d;" i (repr_letter r) i)))
              name
              (String.concat
                 ~sep:", "
                 (List.map caller_reprs ~f:(fun r -> Printf.sprintf "(%s)0" (c_type r))))
        | _ -> ""
      in
      line
        ctx
        "{ %s%s%s%s %sreturn %s; }"
        (String.concat ~sep:" " temps)
        pop
        bounce
        through
        (if musttail then "CAMLC_MUSTTAIL " else "")
        (if repr_equal ret ctx.ret then call else unbox ctx.ret call)

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
          let raises, ret =
            match callee ctx x ~f ~exact with
            | Known g ->
                Var.Set.mem g ctx.out.raising, Var.Hashtbl.find ctx.out.fun_returns g
            | Known_direct _ | Unknown -> true, Value
          in
          (* The typing ensures that an unboxed result is never boxed *)
          if (not (repr_equal ret Value)) && repr_equal (var_repr ctx x) Value
          then failwith "c_of_ocaml: an unboxed result would need to be boxed";
          let callee, args, _ = apply ctx x ~f ~args ~exact in
          if not raises
          then
            if k = 0
            then define ctx x ret (c_call callee args)
            else (
              line ctx "{ %s rv = %s;" (c_type ret) (c_call callee args);
              line ctx "camlc_sp = bx + %d;" k;
              define ctx x ret "rv";
              line ctx "}")
          else if k = 0
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
            line ctx "%s" (reload ctx (Array.to_list a));
            Array.iteri a ~f:(fun i x ->
                line ctx "Store_double_flat_field(b, %d, %s);" i (as_repr ctx Double x));
            line ctx "%s = b;" dst)
          else
            alloc_block
              ~reload:(reload ctx (Array.to_list a))
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
          | Checked e -> checked e
          | Guarded (check, r, e) ->
              line ctx "r = %s;" check;
              line
                ctx
                "if (CAMLC_UNLIKELY(Is_exception_result(r))) %s"
                (raise_stmt ~bt:(`Raise false) ctx traps "r");
              define ctx x r e)
      | Prim (p, args) -> (
          let args = List.map args ~f:(fun a -> (prim_arg ctx a).get Value) in
          let plain e = define ctx x Value e in
          match p, args with
          | Vectlength Value, [ a ] ->
              plain (Printf.sprintf "Val_long(Camlc_wosize_val(%s))" a)
          | Vectlength Float, [ a ] ->
              plain (Printf.sprintf "Val_long(Camlc_wosize_val(%s) / Double_wosize)" a)
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
  | Return x -> line ctx "%s" (return_stmt ~r:ctx.ret ctx (as_repr ctx ctx.ret x))
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

(* The primitives which may trigger a garbage collection only when they
   raise (to allocate the exception) *)
let gc_on_raise_primitives =
  StringSet.of_list
    [ "caml_string_get"
    ; "caml_bytes_get"
    ; "caml_string_set"
    ; "caml_bytes_set"
    ; "caml_check_bound"
    ; "caml_check_bound_float"
    ; "caml_check_bound_gen"
    ]

(* How an instruction may trigger a garbage collection: [`Call] if its
   operands are read before, [`Alloc] if they are read after, [`Raise] if
   only when it raises *)
let gc_point ctx i =
  match i with
  | Let (x, Apply { f; args; exact }) -> (
      if List.exists (boxed_arguments ctx x ~f ~args ~exact) ~f:Fun.id
      then `Alloc
      else
        match callee ctx x ~f ~exact with
        | Known g when not (is_cps_call ctx x) ->
            if Var.Set.mem g ctx.out.collecting
            then `Call
            else if Var.Set.mem g ctx.out.collecting_on_raise
            then `Raise
            else `No
        | Known _ | Known_direct _ | Unknown -> `Call)
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
        | Pc c ->
            { a_typ = Typing.constant_type c; a_const = Some c; get = (fun _ -> "_") }
        | Pv _ -> dummy a
      in
      match primitive ctx.out name (List.map args ~f:dummy) with
      | Plain (r, _, gc) ->
          if gc || ((not (repr_equal r Value)) && repr_equal (var_repr ctx x) Value)
          then `Call
          else `No
      | Checked _ ->
          if StringSet.mem name nogc_primitives
          then `No
          else if StringSet.mem name gc_on_raise_primitives
          then `Raise
          else `Call
      | Guarded (_, r, _) ->
          (* The result is boxed after the check *)
          if (not (repr_equal r Value)) && repr_equal (var_repr ctx x) Value
          then `Call
          else `Raise
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

let rec live_out ctx live_in pc =
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
  Var.Set.union acc (handler_live ctx live_in pc)

(* The variables live at the start of the handler of the innermost trap
   of a block *)
and handler_live ctx live_in pc =
  match Addr.Hashtbl.find ctx.traps pc with
  | [] -> Var.Set.empty
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
      Var.Set.remove exn_var (cont_uses ctx live_in h Var.Set.empty)

(* Polls (see [Camlc_poll_needed] in camlc.h), when the program uses
   threads or domains: the code must let the other threads run, and take
   part in the collections of the other domains, even when it does not
   allocate. A function polls at the start of its loops, and at its start
   if it makes a tail call, which may loop as well. With OCaml 5, an
   allocation of the minor heap polls already, but for the exceptions of
   the asynchronous actions it runs, raised at the next poll (see
   [camlc_alloc_small_gc] in camlc_runtime.c): there is no poll at the start
   of a loop or of a function whose first block allocates, unless the
   program uses [Gc.Memprof], whose callbacks may raise to interrupt a
   computation. Returns whether it polls at its start, and the start of its
   loops. *)
let polls out blocks pc =
  if not out.polling
  then false, Addr.Set.empty
  else
    let allocates pc =
      Ocaml_version.compare Ocaml_version.current [ 5; 0 ] >= 0
      && (not out.memprof)
      && List.exists (Addr.Map.find pc blocks).body ~f:(fun i ->
          match i with
          | Let (_, Block (_, a, _, _, _)) ->
              (* Allocated in the minor heap, even with unboxed floats *)
              Array.length a > 0 && Array.length a <= 128
          | _ -> false)
    in
    let state = Addr.Hashtbl.create 16 in
    let headers = ref Addr.Set.empty in
    let tail_call = ref false in
    let rec visit pc =
      match Addr.Hashtbl.find_opt state pc with
      | Some true -> if not (allocates pc) then headers := Addr.Set.add pc !headers
      | Some false -> ()
      | None ->
          Addr.Hashtbl.add state pc true;
          let block = Addr.Map.find pc blocks in
          (match List.rev block.body, block.branch with
          | Let (x, Apply _) :: _, Return y when Var.equal x y -> tail_call := true
          | _ -> ());
          Code.fold_children blocks pc (fun pc' () -> visit pc') ();
          Addr.Hashtbl.replace state pc false
    in
    visit pc;
    !tail_call && not (allocates pc), !headers

(* The program samples its allocations ([Gc.Memprof]): the exceptions of
   the callbacks, which an allocation runs, are raised at the polls (see
   [camlc_alloc_small_gc] in camlc_runtime.c) *)
let uses_memprof (p : program) =
  Addr.Map.fold
    (fun _ block acc ->
      acc
      || List.exists block.body ~f:(fun i ->
          match i with
          | Let (_, Prim (Extern ("caml_memprof_start", _), _)) -> true
          | _ -> false))
    p.blocks
    false

let uses_threads (p : program) =
  Addr.Map.fold
    (fun _ block acc ->
      acc
      || List.exists block.body ~f:(fun i ->
          match i with
          | Let
              ( _
              , Prim
                  ( Extern
                      ( ( "caml_thread_initialize"
                        | "caml_thread_new"
                        | "caml_domain_spawn" )
                      , _ )
                  , _ ) ) -> true
          | _ -> false))
    p.blocks
    false

(* Whether a variable may hold a pointer to the heap *)
let gc_value ctx x =
  if Var.equal x ctx.env_var
  then not ctx.static_env
  else
    repr_equal (var_repr ctx x) Value
    && (match Typing.var_type ctx.out.types x with
      | Int _ -> false
      | _ -> true)
    && not
         (BitSet.mem ctx.facts.is_int (Var.idx x)
         || BitSet.mem ctx.facts.is_static (Var.idx x))

(* A poll: the values of the C variables [vars] are saved on the shadow
   stack meanwhile (in the reserve below the limit, if needed); [raise]
   raises the exception result [r] *)
let poll_stmt vars ~raise =
  let n = List.length vars in
  let save =
    if n = 0
    then ""
    else
      Printf.sprintf
        "value *pp = camlc_sp - %d; camlc_sp = pp; %s "
        n
        (String.concat
           ~sep:" "
           (List.mapi vars ~f:(fun i x -> Printf.sprintf "pp[%d] = %s;" i x)))
  in
  let restore =
    if n = 0
    then ""
    else
      Printf.sprintf
        " %s camlc_sp = pp + %d;"
        (String.concat
           ~sep:" "
           (List.mapi vars ~f:(fun i x -> Printf.sprintf "%s = pp[%d];" x i)))
        n
  in
  Printf.sprintf
    "if (CAMLC_UNLIKELY(Camlc_poll_needed())) { %sr = camlc_poll();%s if \
     (CAMLC_UNLIKELY(Is_exception_result(r))) %s }"
    save
    restore
    raise

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
             | `Raise -> rooted := Var.Set.union !rooted (handler_live ctx live_in pc)
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
  Var.Set.filter (fun x -> gc_value ctx x) !rooted, live_in

(* The slots of the rooted variables: two variables live at the same time
   have different slots. A variable interferes with the variables live
   after its definition, and the parameters of the function with each other
   and the variables live at its start. A slot may still hold a dead value,
   which remains valid as it was a root. *)
let assign_slots ctx pcs ~params ~entry rooted live_in =
  let edges = Var.Hashtbl.create 64 in
  let add x y =
    if not (Var.equal x y)
    then (
      Var.Hashtbl.replace
        edges
        x
        (Var.Set.add
           y
           (Var.Hashtbl.find_opt edges x |> Option.value ~default:Var.Set.empty));
      Var.Hashtbl.replace
        edges
        y
        (Var.Set.add
           x
           (Var.Hashtbl.find_opt edges y |> Option.value ~default:Var.Set.empty)))
  in
  let define x live =
    if Var.Set.mem x rooted
    then Var.Set.iter (fun y -> add x y) (Var.Set.inter live rooted)
  in
  let live_at pc =
    Addr.Hashtbl.find_opt live_in pc |> Option.value ~default:Var.Set.empty
  in
  let start =
    List.fold_left
      (ctx.env_var :: params)
      ~init:(Var.Set.union (live_at (fst entry)) (use_list ctx (snd entry) Var.Set.empty))
      ~f:(fun acc x -> if Var.Set.mem x rooted then Var.Set.add x acc else acc)
  in
  List.iter (ctx.env_var :: params) ~f:(fun x -> define x start);
  List.iter pcs ~f:(fun pc ->
      let block = Addr.Map.find pc ctx.blocks in
      let l = live_at pc in
      List.iter block.params ~f:(fun x -> define x l);
      (match Addr.Hashtbl.find ctx.traps pc with
      | { exn_var; _ } :: _ -> define exn_var (handler_live ctx live_in pc)
      | [] -> ());
      ignore
        (List.fold_right block.body ~init:(live_out ctx live_in pc) ~f:(fun i live ->
             (* The functions of a closure are all defined with the first
                one *)
             (match i with
             | Let (x, Closure _) -> (
                 match Var.Map.find_opt x ctx.closures with
                 | Some { functions = (f0, _) :: _ as functions; _ } when Var.equal x f0
                   ->
                     let live =
                       List.fold_left functions ~init:live ~f:(fun live (g, _) ->
                           if Var.Set.mem g rooted then Var.Set.add g live else live)
                     in
                     List.iter functions ~f:(fun (g, _) -> define g live)
                 | Some _ | None -> ())
             | _ -> ());
             let live_after =
               match instr_defs i with
               | Some x ->
                   define x live;
                   Var.Set.remove x live
               | None -> live
             in
             instr_uses ctx i live_after)));
  (* Greedy coloring, the parameters first *)
  let order =
    List.filter (ctx.env_var :: params) ~f:(fun x -> Var.Set.mem x rooted)
    @ Var.Set.elements
        (List.fold_left (ctx.env_var :: params) ~init:rooted ~f:(fun s x ->
             Var.Set.remove x s))
  in
  List.iter order ~f:(fun x ->
      let used =
        Var.Set.fold
          (fun y acc ->
            match Var.Hashtbl.find_opt ctx.slots y with
            | Some i -> IntSet.add i acc
            | None -> acc)
          (Var.Hashtbl.find_opt edges x |> Option.value ~default:Var.Set.empty)
          IntSet.empty
      in
      let rec first i = if IntSet.mem i used then first (i + 1) else i in
      Var.Hashtbl.add ctx.slots x (first 0))

(****)

(****)

(* The functions which may raise an exception, that is return an
   exception result: the calls of the other ones are not followed by a
   check. A function may raise if, outside any handler, it raises, calls a
   primitive which may raise, an unknown function or a function which may
   raise. CPS versions and CPS calls are assumed to raise. An overflow of
   the shadow stack in a function which cannot raise is fatal.

   Similarly, the functions which may trigger a garbage collection: the
   calls of the other ones are not GC points. A call of a function which
   may trigger one only when it raises, outside any handler, only roots the
   variables live in the handler of the caller. *)
type summary =
  { fn : Var.t
  ; raises : bool  (** Raises by itself *)
  ; outer : Var.t list  (** The functions called outside any handler *)
  ; gcs : bool  (** May trigger a GC by itself and return *)
  ; any_gcs : bool  (** May trigger a GC by itself *)
  ; inner : Var.t list  (** The functions called inside a handler *)
  ; tail : Var.t list  (** The known functions called in tail position *)
  ; unknown_tail : bool  (** Makes an unknown tail call *)
  }

let non_tail_calls blocks pc =
  let block = Addr.Map.find pc blocks in
  let rec calls l =
    match l with
    | [] -> []
    | Let (x, Apply { f; exact; _ }) :: r ->
        let tail =
          List.for_all r ~f:(function
            | Event _ -> true
            | _ -> false)
          &&
          match block.branch with
          | Return y -> Var.equal x y
          | _ -> false
        in
        if tail then calls r else (x, f, exact) :: calls r
    | _ :: r -> calls r
  in
  calls block.body

(* The largest number of calls which may be recursive on a path through
   the function starting at [pc] (back edges ignored) *)
let recursive_calls out global_flow_info closures blocks ~fn pc =
  let memo = Addr.Hashtbl.create 16 in
  let rec count pc =
    match Addr.Hashtbl.find_opt memo pc with
    | Some n -> n
    | None ->
        Addr.Hashtbl.replace memo pc 0;
        let block = Addr.Map.find pc blocks in
        let here =
          List.length
            (List.filter
               (non_tail_calls blocks pc)
               ~f:(recursive_call out global_flow_info closures ~fn))
        in
        let next =
          match block.branch with
          | Return _ | Raise _ | Stop -> 0
          | Branch (pc', _) | Poptrap (pc', _) -> count pc'
          | Cond (_, (pc1, _), (pc2, _)) | Pushtrap ((pc1, _), _, (pc2, _)) ->
              max (count pc1) (count pc2)
          | Switch (_, conts) ->
              Array.fold_left conts ~init:0 ~f:(fun n (pc', _) -> max n (count pc'))
        in
        let n = here + next in
        Addr.Hashtbl.replace memo pc n;
        n
  in
  count pc

(* A context for the analyses of the code, out of any function *)
let analysis_ctx out facts global_flow_info closures blocks =
  { out
  ; facts
  ; global_flow_info
  ; blocks
  ; closures
  ; env_var = Var.fresh ()
  ; fn = None
  ; arity = None
  ; static_env = true
  ; locations = Var.Hashtbl.create 1
  ; slots = Var.Hashtbl.create 1
  ; traps = Addr.Hashtbl.create 1
  ; handler_ids = Addr.Hashtbl.create 1
  ; local_names = Var.Hashtbl.create 1
  ; labels = Addr.Hashtbl.create 1
  ; buf = Buffer.create 1
  ; position = None
  ; source_name = None
  ; raises = true
  ; ret = Value
  ; framed = false
  ; framed_blocks = None
  ; frame_push = ""
  }

let analyze_functions out facts global_flow_info closures blocks functions =
  let ctx = analysis_ctx out facts global_flow_info closures blocks in
  let typed a =
    match a with
    | Pv x ->
        { a_typ = Typing.var_type out.types x; a_const = None; get = (fun _ -> "_") }
    | Pc c -> { a_typ = Typing.constant_type c; a_const = Some c; get = (fun _ -> "_") }
  in
  (* Whether a function raises by itself, the functions it calls outside
     any handler, whether it triggers a GC by itself (and then returns, or
     in any case), and the functions it calls inside handlers *)
  let summary pc =
    let visited = Addr.Hashtbl.create 16 in
    let raises = ref false in
    let outer = ref [] in
    let gcs = ref false in
    let any_gcs = ref false in
    let inner = ref [] in
    let tail = ref [] in
    let unknown_tail = ref false in
    let gc_point depth i =
      match gc_point ctx i with
      | `No -> ()
      | `Raise -> if depth = 0 then any_gcs := true else gcs := true
      | `Call | `Alloc -> gcs := true
    in
    let rec visit pc depth =
      if not (Addr.Hashtbl.mem visited pc)
      then (
        Addr.Hashtbl.add visited pc ();
        let block = Addr.Map.find pc blocks in
        (* A tail call (see [translate_function]) *)
        (if depth = 0
         then
           match
             ( List.find_opt (List.rev block.body) ~f:(fun i ->
                   match i with
                   | Event _ -> false
                   | _ -> true)
             , block.branch )
           with
           | Some (Let (x, Apply { f; exact; _ })), Return y when Var.equal x y -> (
               match resolve_callee global_flow_info closures x ~f ~exact with
               | Known g -> tail := g :: !tail
               | Known_direct _ | Unknown -> unknown_tail := true)
           | _ -> ());
        List.iter block.body ~f:(fun i ->
            match i with
            | Let (x, Apply { f; args; exact }) -> (
                if double_translation () && Var.Set.mem x out.in_cps
                then (
                  raises := true;
                  gcs := true)
                else
                  match resolve_callee global_flow_info closures x ~f ~exact with
                  | Known g ->
                      if depth = 0 then outer := g :: !outer else inner := g :: !inner;
                      if List.exists (boxed_arguments ctx x ~f ~args ~exact) ~f:Fun.id
                      then gcs := true
                  | Known_direct _ | Unknown ->
                      if depth = 0 then raises := true;
                      gcs := true)
            | Let (_, Prim (Extern ("caml_cps_closure", _), _)) -> ()
            | Let (_, Prim (Extern (name, _), args)) ->
                (match primitive out name (List.map args ~f:typed) with
                | Plain _ -> ()
                | Checked _ | Guarded _ | Ccallv -> if depth = 0 then raises := true);
                gc_point depth i
            | Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ ->
                gc_point depth i);
        match block.branch with
        | Raise _ -> if depth = 0 then raises := true
        | Return _ | Stop -> ()
        | Branch (pc', _) -> visit pc' depth
        | Poptrap (pc', _) -> visit pc' (depth - 1)
        | Cond (_, (pc1, _), (pc2, _)) ->
            visit pc1 depth;
            visit pc2 depth
        | Switch (_, conts) -> Array.iter conts ~f:(fun (pc', _) -> visit pc' depth)
        | Pushtrap ((pc1, _), _, (pc2, _)) ->
            visit pc1 (depth + 1);
            visit pc2 depth)
    in
    visit pc 0;
    (* A poll may raise and collect *)
    let entry_poll, loop_polls = polls out blocks pc in
    if entry_poll || not (Addr.Set.is_empty loop_polls)
    then (
      raises := true;
      gcs := true);
    !raises, !outer, !gcs, !gcs || !any_gcs, !inner, !tail, !unknown_tail
  in
  (* The functions with the property, and the callers of each function *)
  let propagate roots callers =
    let set = Var.Hashtbl.create 1024 in
    let rec mark f =
      if not (Var.Hashtbl.mem set f)
      then (
        Var.Hashtbl.add set f ();
        List.iter (Var.Hashtbl.find_all callers f) ~f:mark)
    in
    List.iter roots ~f:mark;
    Var.Hashtbl.fold (fun f () acc -> Var.Set.add f acc) set Var.Set.empty
  in
  let summaries =
    List.map functions ~f:(fun (f, _, (pc, _)) ->
        let raises, outer, gcs, any_gcs, inner, tail, unknown_tail = summary pc in
        let side = Var.Set.mem f out.cps_sides in
        { fn = f
        ; raises = raises || side
        ; outer
        ; gcs = gcs || side
        ; any_gcs = any_gcs || side
        ; inner
        ; tail
        ; unknown_tail
        })
  in
  let callers l =
    let h = Var.Hashtbl.create 1024 in
    List.iter summaries ~f:(fun s ->
        List.iter (l s) ~f:(fun g -> Var.Hashtbl.add h g s.fn));
    h
  in
  let roots p = List.filter_map summaries ~f:(fun s -> if p s then Some s.fn else None) in
  out.raising <- propagate (roots (fun s -> s.raises)) (callers (fun s -> s.outer));
  out.unknown_tail_calls <-
    propagate (roots (fun s -> s.unknown_tail)) (callers (fun s -> s.tail));
  (* The functions which may trigger a GC in any way, then those which may
     return after one: a call inside a handler is in the second case if
     the function called may trigger a GC in any way *)
  let set_collecting fallback =
    let any =
      propagate
        (roots (fun s -> s.any_gcs || Var.Set.mem s.fn fallback))
        (callers (fun s -> s.outer @ s.inner))
    in
    out.collecting <-
      propagate
        (roots (fun s ->
             s.gcs
             || Var.Set.mem s.fn fallback
             || List.exists s.inner ~f:(fun g -> Var.Set.mem g any)))
        (callers (fun s -> s.outer));
    out.collecting_on_raise <- Var.Set.diff any out.collecting
  in
  set_collecting Var.Set.empty;
  (* The strongly connected components of the graph of known calls
     (Tarjan), those with a cycle *)
  let callees = Var.Hashtbl.create 1024 in
  List.iter summaries ~f:(fun s -> Var.Hashtbl.replace callees s.fn (s.outer @ s.inner));
  let index = Var.Hashtbl.create 1024 in
  let low = Var.Hashtbl.create 1024 in
  let on_stack = Var.Hashtbl.create 1024 in
  let stack = ref [] in
  let next = ref 0 in
  let components = Var.Hashtbl.create 1024 in
  let rec visit f =
    Var.Hashtbl.replace index f !next;
    Var.Hashtbl.replace low f !next;
    incr next;
    stack := f :: !stack;
    Var.Hashtbl.replace on_stack f ();
    List.iter
      (Option.value ~default:[] (Var.Hashtbl.find_opt callees f))
      ~f:(fun g ->
        if not (Var.Hashtbl.mem index g)
        then (
          visit g;
          Var.Hashtbl.replace
            low
            f
            (min (Var.Hashtbl.find low f) (Var.Hashtbl.find low g)))
        else if Var.Hashtbl.mem on_stack g
        then
          Var.Hashtbl.replace
            low
            f
            (min (Var.Hashtbl.find low f) (Var.Hashtbl.find index g)));
    if Var.Hashtbl.find low f = Var.Hashtbl.find index f
    then
      let rec pop acc =
        match !stack with
        | g :: r ->
            stack := r;
            Var.Hashtbl.remove on_stack g;
            if Var.equal g f then g :: acc else pop (g :: acc)
        | [] -> assert false
      in
      let component = pop [] in
      let cyclic =
        match component with
        | [ g ] ->
            List.exists
              (Option.value ~default:[] (Var.Hashtbl.find_opt callees g))
              ~f:(Var.equal g)
        | _ -> true
      in
      if cyclic
      then
        List.iter component ~f:(fun g ->
            Var.Hashtbl.replace components g (Var.Hashtbl.find index f))
  in
  List.iter summaries ~f:(fun s -> if not (Var.Hashtbl.mem index s.fn) then visit s.fn);
  out.components <- components;
  (* With --cps-stack, the recursive functions with a CPS version may go on
     in it, which allocates (see [camlc_cps_stack]): the linear recursions
     (one call which may be recursive on each path), and the others which
     may trigger a GC anyway, which then have a frame measuring their depth
     (that of a recursion without frame, such as [fib], is not measured) *)
  if Config.Flag.cps_recursions ()
  then (
    out.cps_fallback <-
      List.fold_left functions ~init:Var.Set.empty ~f:(fun acc (f, _, (pc, _)) ->
          if Var.Map.mem f out.cps_pairs
          then
            match
              recursive_calls out global_flow_info closures blocks ~fn:(Some f) pc
            with
            | 0 -> acc
            | 1 -> Var.Set.add f acc
            | _ -> if Var.Set.mem f out.collecting then Var.Set.add f acc else acc
          else acc);
    set_collecting out.cps_fallback)

let translate_function
    out
    facts
    global_flow_info
    blocks
    closures
    ~fn
    ~name
    ~params
    ~param_reprs
    ~env
    ~static_env
    ~source_name
    ~raises
    (pc, args) =
  out.refs <- Var.Set.empty;
  let ctx =
    { out
    ; facts
    ; global_flow_info
    ; blocks
    ; closures
    ; env_var = Var.fresh ()
    ; fn
    ; arity = None
    ; static_env
    ; locations = Var.Hashtbl.create 64
    ; slots = Var.Hashtbl.create 16
    ; traps = Addr.Hashtbl.create 16
    ; handler_ids = Addr.Hashtbl.create 16
    ; local_names = Var.Hashtbl.create 64
    ; labels = Addr.Hashtbl.create 16
    ; buf = Buffer.create 4096
    ; position = None
    ; source_name
    ; raises
    ; ret =
        (match fn with
        | Some f -> Var.Hashtbl.find out.fun_returns f
        | None -> Value)
    ; framed = false
    ; framed_blocks = None
    ; frame_push = ""
    }
  in
  List.iter env ~f:(fun (x, loc) -> Var.Hashtbl.replace ctx.locations x loc);
  let ctx = if List.is_empty env then ctx else { ctx with arity = Some param_reprs } in
  let locals = ref [] in
  let add_local x =
    if not (Var.Hashtbl.mem ctx.locations x || Var.Hashtbl.mem out.globals x)
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
  let rooted, live_in = rooted_variables ctx pcs ~params in
  let entry_poll, loop_polls = polls out blocks pc in
  assign_slots ctx pcs ~params ~entry:(pc, args) rooted live_in;
  let frame_size = Var.Hashtbl.fold (fun _ i n -> max n (i + 1)) ctx.slots 0 in
  (* The shadow stack measures the depth of the stack of the engine (see
     [camlc_segment_limit]) in the recursions: only the functions making
     calls which may be recursive (to their strongly connected component
     of known calls, or unknown) can go on a new segment, the others
     appearing at most once on the stack. The frame of such a function has
     at least a word for 100 bytes of its frame on the stack of the engine,
     which takes about 80 bytes and 2 per C variable (V8's baseline
     compiler). Without frame, it gets one if it makes one such call on
     each path: with more, the depth of the recursion is that of a tree
     (fib), costly to measure *)
  let non_tail_calls pc = non_tail_calls blocks pc in
  let recursive_call = recursive_call out global_flow_info closures ~fn in
  let calls pc = List.exists (non_tail_calls pc) ~f:recursive_call in
  let recursive_calls = recursive_calls out global_flow_info closures blocks ~fn pc in
  let depth_frame = frame_size = 0 && Option.is_some fn && recursive_calls = 1 in
  let depth_words =
    if Option.is_some fn && recursive_calls > 0 && (depth_frame || frame_size > 0)
    then 1 + (List.length !locals / 50)
    else 0
  in
  let frame_size = max frame_size depth_words in
  (* The first slots, written at the start of the function: those of the
     parameters, allocated first *)
  let initialized =
    let param_slots =
      List.filter_map (ctx.env_var :: params) ~f:(fun x ->
          Var.Hashtbl.find_opt ctx.slots x)
    in
    let rec count i = if List.mem ~eq:Int.equal i param_slots then count (i + 1) else i in
    count 0
  in
  (* Shrink-wrapping: the frame is allocated when entering the blocks which
     use it, and those reachable from them, if the start of the function
     does not need it. Not with exception handlers, reached from several
     blocks. *)
  let slot_var x = Var.Hashtbl.mem ctx.slots x in
  let uses_slots s = Var.Set.exists slot_var s in
  let cont_needs (pc', args) =
    uses_slots (use_list ctx args Var.Set.empty)
    || List.exists (Addr.Map.find pc' blocks).params ~f:slot_var
  in
  let needs pc =
    let block = Addr.Map.find pc blocks in
    (depth_frame && calls pc)
    || List.exists block.params ~f:slot_var
    || List.exists block.body ~f:(fun i ->
        (match instr_defs i with
          | Some x -> slot_var x
          | None -> false)
        || uses_slots (instr_uses ctx i Var.Set.empty)
        ||
        match i with
        | Let (x, Closure _) -> (
            match Var.Map.find_opt x closures with
            | Some { functions; _ } -> List.exists functions ~f:(fun (g, _) -> slot_var g)
            | None -> false)
        | _ -> false)
    ||
    match block.branch with
    | Return x | Raise (x, _) -> uses_slots (use ctx x Var.Set.empty)
    | Stop -> false
    | Branch c | Poptrap c -> cont_needs c
    | Cond (x, c1, c2) ->
        uses_slots (use ctx x Var.Set.empty) || cont_needs c1 || cont_needs c2
    | Switch (x, conts) ->
        uses_slots (use ctx x Var.Set.empty) || Array.exists conts ~f:cont_needs
    | Pushtrap _ -> true
  in
  let overflow_return =
    if ctx.raises
    then "return camlc_stack_overflow();"
    else "camlc_fatal_stack_overflow();"
  in
  let segment_label = ref false in
  let segment_stmt () =
    match fn with
    | Some f
      when Var.Set.mem f out.cps_fallback
           && repr_equal ctx.ret Value
           && List.for_all (Var.Hashtbl.find out.fun_reprs f) ~f:(fun r ->
               repr_equal r Value) ->
        (* With --cps-stack, the call goes on in the CPS version of the
           function (see [camlc_cps_stack]) *)
        let args =
          List.mapi params ~f:(fun i x ->
              if Var.Hashtbl.mem ctx.slots x
              then Printf.sprintf "a%d" i
              else local_name ctx x)
        in
        let helper = Printf.sprintf "camlc_c_%d" (List.length args) in
        String.Hashtbl.replace out.segment_thunks helper ();
        Printf.sprintf
          "if (fp < camlc_stack_limit) %s %s"
          overflow_return
          (let call =
             Printf.sprintf
               "%s(%s)"
               helper
               (String.concat ~sep:", " (as_value ctx f :: args))
           in
           if ctx.raises
           then Printf.sprintf "return %s;" call
           else Printf.sprintf "return camlc_cps_stack_noexc(%s);" call)
    | Some f ->
        let reprs = Var.Hashtbl.find out.fun_reprs f in
        let args =
          List.mapi params ~f:(fun i x ->
              let r = List.nth reprs i in
              ( r
              , if Var.Hashtbl.mem ctx.slots x
                then Printf.sprintf "a%d" i
                else local_name ctx x ))
          @ if List.is_empty env then [] else [ Value, "env" ]
        in
        let thunk =
          Printf.sprintf
            "camlc_s_%c_%s"
            (repr_letter ctx.ret)
            (String.concat
               ~sep:""
               (List.map args ~f:(fun (r, _) -> String.make 1 (repr_letter r))))
        in
        String.Hashtbl.replace out.segment_thunks thunk ();
        Printf.sprintf
          "if (fp < camlc_stack_limit) %s return %s(%s);"
          overflow_return
          thunk
          (String.concat
             ~sep:", "
             (Printf.sprintf "(void *)%s" (fun_name out f)
             :: "FRAME_SIZE"
             :: List.map args ~f:snd))
    | None -> overflow_return
  in
  (* When the frame would go below [camlc_segment_limit] but not below
     [camlc_stack_limit], the function is called again on a new segment
     of the stack of the wasm engine (see [camlc_segment]), through the
     thunk of its signature ([camlc_s_...], defined by [declare_symbols]),
     if nothing was done since its start (out of line, at label [S]);
     else it goes on on the current segment *)
  let frame_check ~restartable =
    if restartable && Option.is_some fn && recursive_calls > 0
    then (
      segment_label := true;
      "if (CAMLC_UNLIKELY(fp < camlc_segment_limit)) goto S;")
    else Printf.sprintf "if (CAMLC_UNLIKELY(fp < camlc_stack_limit)) %s" overflow_return
  in
  if
    frame_size > 0
    && initialized = 0
    && (not (cont_needs (pc, args)))
    && not
         (List.exists pcs ~f:(fun pc ->
              match (Addr.Map.find pc blocks).branch with
              | Pushtrap _ -> true
              | _ -> false))
  then (
    let framed = ref Addr.Set.empty in
    let rec mark pc =
      if not (Addr.Set.mem pc !framed)
      then (
        framed := Addr.Set.add pc !framed;
        match (Addr.Map.find pc blocks).branch with
        | Return _ | Raise _ | Stop -> ()
        | Branch (pc', _) | Poptrap (pc', _) -> mark pc'
        | Cond (_, (pc1, _), (pc2, _)) ->
            mark pc1;
            mark pc2
        | Switch (_, conts) -> Array.iter conts ~f:(fun (pc', _) -> mark pc')
        | Pushtrap _ -> assert false)
    in
    List.iter pcs ~f:(fun pc -> if needs pc then mark pc);
    if not (Addr.Set.mem pc !framed)
    then (
      (* The function can be restarted where it allocates its frame if
         what it may do before has no effect: the blocks from which the
         frame is allocated (a fixpoint) *)
      let reaches = ref !framed in
      let changed = ref true in
      while !changed do
        changed := false;
        List.iter pcs ~f:(fun pc ->
            if not (Addr.Set.mem pc !reaches)
            then
              let succ pc' = Addr.Set.mem pc' !reaches in
              let r =
                match (Addr.Map.find pc blocks).branch with
                | Return _ | Raise _ | Stop -> false
                | Branch (pc', _) | Poptrap (pc', _) -> succ pc'
                | Cond (_, (pc1, _), (pc2, _)) | Pushtrap ((pc1, _), _, (pc2, _)) ->
                    succ pc1 || succ pc2
                | Switch (_, conts) -> Array.exists conts ~f:(fun (pc', _) -> succ pc')
              in
              if r
              then (
                reaches := Addr.Set.add pc !reaches;
                changed := true))
      done;
      let restartable =
        List.for_all pcs ~f:(fun pc ->
            Addr.Set.mem pc !framed
            || (not (Addr.Set.mem pc !reaches))
            || List.for_all (Addr.Map.find pc blocks).body ~f:(fun i ->
                match i with
                | Let (_, e) -> Pure_fun.pure_expr Pure_fun.empty e
                | Event _ -> true
                | Assign _ | Set_field _ | Offset_ref _ | Array_set _ -> false))
      in
      ctx.framed_blocks <- Some !framed;
      ctx.frame_push <-
        String.concat
          ~sep:" "
          [ "fp = camlc_sp - FRAME_SIZE;"
          ; frame_check ~restartable
          ; "camlc_sp = fp;"
          ; "for (int i = 0; i < FRAME_SIZE; i++) fp[i] = Val_unit;"
          ]));
  ctx.framed <- frame_size > 0 && Option.is_none ctx.framed_blocks;
  (* Body *)
  List.iteri params ~f:(fun i x ->
      if Var.Hashtbl.mem ctx.slots x then line ctx "%s = a%d;" (slot ctx x) i);
  (match Var.Hashtbl.find_opt ctx.slots ctx.env_var with
  | Some i -> line ctx "fp[%d] = env;" i
  | None -> ());
  line ctx "%s" (branch ctx (pc, args));
  List.iter pcs ~f:(fun pc ->
      let block = Addr.Map.find pc blocks in
      let traps = Addr.Hashtbl.find ctx.traps pc in
      (match ctx.framed_blocks with
      | Some s -> ctx.framed <- Addr.Set.mem pc s
      | None -> ());
      Printf.bprintf ctx.buf " %s:;\n" (label ctx pc);
      if Addr.Set.mem pc loop_polls
      then
        line
          ctx
          "%s"
          (poll_stmt
             (Addr.Hashtbl.find live_in pc
             |> Var.Set.elements
             |> List.filter ~f:(fun x -> gc_value ctx x)
             |> List.map ~f:(fun x -> local ctx x))
             ~raise:(raise_stmt ctx traps "r"));
      (* After a point where the GC may run, the variables of the frame
         live after it are reloaded (those live in a handler are reloaded
         at its start) *)
      let translate_body ?(live_end = live_out ctx live_in pc) body =
        let body = Array.of_list body in
        let n = Array.length body in
        let live_after = Array.make n live_end in
        for k = n - 1 downto 1 do
          let i = body.(k) in
          let live =
            match instr_defs i with
            | Some x -> Var.Set.remove x live_after.(k)
            | None -> live_after.(k)
          in
          live_after.(k - 1) <- instr_uses ctx i live
        done;
        Array.iteri body ~f:(fun k i ->
            translate_instr ctx traps i;
            match gc_point ctx i with
            | `Call | `Alloc ->
                let live =
                  match instr_defs i with
                  | Some x -> Var.Set.remove x live_after.(k)
                  | None -> live_after.(k)
                in
                let s = reload ctx (Var.Set.elements live) in
                if not (String.equal s "") then line ctx "%s" s
            | `No | `Raise -> ())
      in
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
          translate_body
            ~live_end:(instr_uses ctx (Let (x, Apply { f; args; exact })) Var.Set.empty)
            body;
          translate_tail_call ctx x ~f ~args ~exact
      | None -> (
          translate_body block.body;
          translate_last ctx traps block.branch;
          match block.branch with
          | Pushtrap (_, _, handler) ->
              Printf.bprintf ctx.buf " H%d:;\n" (Addr.Hashtbl.find ctx.handler_ids pc);
              line
                ctx
                "%s"
                (reload
                   ctx
                   (Var.Set.elements
                      (Var.Set.union
                         (Addr.Hashtbl.find_opt live_in (fst handler)
                         |> Option.value ~default:Var.Set.empty)
                         (use_list ctx (snd handler) Var.Set.empty))));
              line ctx "%s" (branch ctx handler)
          | Return _ | Raise _ | Stop | Branch _ | Cond _ | Switch _ | Poptrap _ -> ()));
  let param_set = Var.Set.of_list params in
  let decls =
    List.filter_map (List.rev !locals) ~f:(fun x ->
        if Var.Set.mem x param_set && not (Var.Hashtbl.mem ctx.slots x)
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
  let fb = Buffer.create (Buffer.length ctx.buf + 1024) in
  Printf.bprintf
    fb
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
          then Printf.bprintf fb (if i = 0 then "  %s " else ";\n  %s ") (c_type r);
          if i mod 16 <> 0 then Buffer.add_string fb ", ";
          Buffer.add_string fb d);
      if not (List.is_empty l) then Buffer.add_string fb ";\n");
  Buffer.add_string
    fb
    "  value r, b, *argv, *bx;\n  (void)r; (void)b; (void)argv; (void)bx;\n";
  if entry_poll
  then
    Printf.bprintf
      fb
      "  %s\n"
      (poll_stmt
         (List.concat
            (List.mapi params ~f:(fun i x ->
                 if not (gc_value ctx x)
                 then []
                 else if Var.Hashtbl.mem ctx.slots x
                 then [ Printf.sprintf "a%d" i ]
                 else [ local_name ctx x ]))
         @ if List.is_empty env || static_env then [] else [ "env" ])
         ~raise:"return r;");
  if frame_size > 0 && Option.is_some ctx.framed_blocks
  then Printf.bprintf fb "#define FRAME_SIZE %d\n  value *fp;\n" frame_size
  else if frame_size > 0
  then
    Printf.bprintf
      fb
      "#define FRAME_SIZE %d\n\
      \  value *fp = camlc_sp - FRAME_SIZE;\n\
      \  %s\n\
      \  camlc_sp = fp;\n\
      \  for (int i = %d; i < FRAME_SIZE; i++) fp[i] = Val_unit;\n"
      frame_size
      (frame_check ~restartable:true)
      initialized;
  Buffer.add_buffer fb ctx.buf;
  if !segment_label then Printf.bprintf fb " S:;\n  %s\n" (segment_stmt ());
  Buffer.add_string fb "}\n";
  if frame_size > 0 then Buffer.add_string fb "#undef FRAME_SIZE\n";
  Buffer.add_char fb '\n';
  out.definitions <-
    { def_fn = fn
    ; def_name =
        (match String.rindex_opt name ' ' with
        | Some i -> String.sub name ~pos:(i + 1) ~len:(String.length name - i - 1)
        | None -> name)
    ; def_text = Buffer.contents fb
    ; def_size = Buffer.length fb
    ; def_refs = out.refs
    }
    :: out.definitions

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
  (* All the closures, with their parameters, entry point and location *)
  fold_closures
    p
    (fun name_opt params cont loc acc ->
      match name_opt with
      | Some f -> (f, params, cont, loc) :: acc
      | None -> acc)
    []

let source_file (pi : Parse_info.t) =
  match pi.name, pi.src with
  | Some n, _ | None, Some n -> n
  | None, None -> ""

(* The C name of each global, after the last debug event before its
   definition in the program, its name and its rank among the globals with
   the same event and name (see [name_functions]) *)
let name_globals (p : program) global_list names =
  let set = Var.Set.of_list global_list in
  let ranks = String.Hashtbl.create 1024 in
  let last = ref "" in
  let name x =
    if Var.Set.mem x set && not (Var.Hashtbl.mem names x)
    then (
      let base =
        match Var.get_name x with
        | Some s -> sanitize s
        | None -> ""
      in
      let key = !last ^ ":" ^ base in
      let rank =
        match String.Hashtbl.find_opt ranks key with
        | Some n -> n
        | None -> 0
      in
      String.Hashtbl.replace ranks key (rank + 1);
      Var.Hashtbl.add
        names
        x
        (Printf.sprintf
           "%s%s%s"
           (digest_name "camlc_g_" key)
           (if rank = 0 then "" else Printf.sprintf "_%d" rank)
           (if String.equal base "" then "" else "_" ^ base)))
  in
  Addr.Map.iter
    (fun _ (block : block) ->
      List.iter block.params ~f:name;
      List.iter block.body ~f:(fun i ->
          match i with
          | Event pi -> last := Printf.sprintf "%s:%d:%d" (source_file pi) pi.line pi.col
          | Let (x, _) -> name x
          | Assign _ | Set_field _ | Offset_ref _ | Array_set _ -> ()))
    p.blocks;
  (* Not defined in a block, which should not happen *)
  List.iter global_list ~f:(fun x ->
      if not (Var.Hashtbl.mem names x)
      then Var.Hashtbl.add names x (Printf.sprintf "camlc_g_v%d" (Var.idx x)))

(* The C names of the functions do not depend on the numbering of the
   variables, which an edit anywhere in the program shifts: they are named
   after their location in the source and their name (and their rank among
   the functions with the same location and name, the copies made by
   inlining for instance), so that the code of the rest of the program is
   left unchanged *)
(* The first debug event of the code starting at [pc] *)
let first_event blocks (pc, _) =
  let rec search visited todo =
    match todo with
    | [] -> None
    | pc :: r when Addr.Set.mem pc visited || Addr.Set.cardinal visited > 32 ->
        search visited r
    | pc :: r -> (
        let block = Addr.Map.find pc blocks in
        match
          List.find_map block.body ~f:(fun i ->
              match i with
              | Event pi -> Some pi
              | _ -> None)
        with
        | Some _ as loc -> loc
        | None ->
            let next = ref [] in
            Code.fold_children blocks pc (fun pc' () -> next := pc' :: !next) ();
            search (Addr.Set.add pc visited) (r @ List.rev !next))
  in
  search Addr.Set.empty [ pc ]

let name_functions out blocks functions =
  let ranks = String.Hashtbl.create 1024 in
  (* Without a location, the first debug event of the body *)
  let first_event = first_event blocks in
  List.iter functions ~f:(fun (f, _, cont, (loc : Parse_info.t option)) ->
      let loc =
        match loc with
        | Some _ -> loc
        | None -> first_event cont
      in
      let base =
        match Var.get_name f with
        | Some s -> sanitize s
        | None -> ""
      in
      let key =
        match loc with
        | Some pi -> Printf.sprintf "%s:%d:%d:%s" (source_file pi) pi.line pi.col base
        | None -> ":" ^ base
      in
      let rank =
        match String.Hashtbl.find_opt ranks key with
        | Some n -> n
        | None -> 0
      in
      String.Hashtbl.replace ranks key (rank + 1);
      let hash = String.sub (Digest.to_hex (Digest.string key)) ~pos:0 ~len:12 in
      Var.Hashtbl.replace
        out.fun_names
        f
        (Printf.sprintf
           "camlc_f%s%s%s"
           hash
           (if rank = 0 then "" else Printf.sprintf "_%d" rank)
           (if String.equal base "" then "" else "_" ^ base)))

(* The toplevel code of a large program is split into functions of about
   [toplevel_chunk_size] instructions (option --toplevel-chunk-size): the
   time C compilers take to optimize a function grows faster than its size. Since the values which
   live long are in globals (see [Globalize]), there are many points where
   no local variable is live. A chunk runs from such a point, a block [c]
   outside any handler, to the next one: the code reached from [c] must be
   dominated by [c] (not in a loop entered before it), and so must be the
   ends of the program, so that each chunk, unless it raises, ends by
   jumping to the start of the next one. The toplevel function calls the
   chunks in turn. The chunks start at points chosen by the hash of their
   name, with a probability proportional to the size of the code since the
   previous candidate, once a chunk has half the size: an edit leaves the
   other chunks unchanged. *)
let toplevel_chunks
    out
    facts
    global_flow_info
    closures
    blocks
    ~toplevel_chunk_size
    ~free_pc
    pc =
  let ctx = analysis_ctx out facts global_flow_info closures blocks in
  let ctx = { ctx with traps = Addr.Hashtbl.create 1024 } in
  let pcs = function_blocks ctx pc in
  let size =
    List.fold_left pcs ~init:0 ~f:(fun n pc ->
        n + List.length (Addr.Map.find pc blocks).body)
  in
  if toplevel_chunk_size = 0 || size < 2 * toplevel_chunk_size
  then []
  else
    let add_local x =
      if not (Var.Hashtbl.mem ctx.locations x || Var.Hashtbl.mem out.globals x)
      then Var.Hashtbl.add ctx.locations x Local
    in
    List.iter pcs ~f:(fun pc ->
        let block = Addr.Map.find pc blocks in
        List.iter block.params ~f:add_local;
        List.iter block.body ~f:(fun i ->
            match instr_defs i with
            | Some x -> add_local x
            | None -> ());
        match block.branch with
        | Pushtrap (_, x, _) -> add_local x
        | Return _ | Raise _ | Stop | Branch _ | Cond _ | Switch _ | Poptrap _ -> ());
    let _, live_in = rooted_variables ctx pcs ~params:[] in
    (* The blocks are split where no local variable is live: at the first
       such point of a block which starts with some (or with parameters),
       and every [toplevel_chunk_size / 16] instructions at least *)
    let free_pc = ref free_pc in
    let spacing = toplevel_chunk_size / 16 in
    let blocks =
      List.fold_left pcs ~init:blocks ~f:(fun blocks pc ->
          let block = Addr.Map.find pc blocks in
          let body = Array.of_list block.body in
          let n = Array.length body in
          let clean_entry =
            List.is_empty block.params && Var.Set.is_empty (Addr.Hashtbl.find live_in pc)
          in
          if n < 2 || (clean_entry && n < 2 * spacing)
          then blocks
          else
            let live = ref (live_out ctx live_in pc) in
            let empty = Array.make n false in
            for k = n - 1 downto 0 do
              empty.(k) <- Var.Set.is_empty !live;
              let i = body.(k) in
              live :=
                instr_uses
                  ctx
                  i
                  (match instr_defs i with
                  | Some x -> Var.Set.remove x !live
                  | None -> !live)
            done;
            let first =
              if clean_entry
              then 0
              else
                let rec find k = if k >= n || empty.(k - 1) then k else find (k + 1) in
                find 1
            in
            (* The pieces, from the last one *)
            let traps = Addr.Hashtbl.find ctx.traps pc in
            let rec split blocks k last branch =
              (* [k]: the start of the piece; [last]: its end *)
              if k = 0
              then
                Addr.Map.add
                  pc
                  { block with
                    body = Array.to_list (Array.sub body ~pos:0 ~len:last)
                  ; branch
                  }
                  blocks
              else if
                empty.(k - 1)
                && (k = first || (last - k >= spacing && k >= first + spacing))
              then (
                let pc' = !free_pc in
                incr free_pc;
                Addr.Hashtbl.replace ctx.traps pc' traps;
                Addr.Hashtbl.replace live_in pc' Var.Set.empty;
                let blocks =
                  Addr.Map.add
                    pc'
                    { params = []
                    ; body = Array.to_list (Array.sub body ~pos:k ~len:(last - k))
                    ; branch
                    }
                    blocks
                in
                split blocks (k - 1) k (Branch (pc', [])))
              else split blocks (k - 1) last branch
            in
            split blocks (n - 1) n block.branch)
    in
    let free_pc = !free_pc in
    let g = Structure.control_flow_graph blocks pc in
    let idom = Structure.immediate_dominators g in
    let rpo = Structure.blocks_in_reverse_post_order g in
    let depth = Addr.Hashtbl.create 1024 in
    List.iter rpo ~f:(fun pc' ->
        Addr.Hashtbl.replace
          depth
          pc'
          (match Addr.Hashtbl.find_opt idom pc' with
          | Some d -> Addr.Hashtbl.find depth d + 1
          | None -> 0));
    let rec common_dominator a b =
      if a = b
      then a
      else if Addr.Hashtbl.find depth a > Addr.Hashtbl.find depth b
      then common_dominator (Addr.Hashtbl.find idom a) b
      else common_dominator a (Addr.Hashtbl.find idom b)
    in
    let ends =
      List.filter rpo ~f:(fun pc ->
          match (Addr.Map.find pc blocks).branch with
          | Return _ | Stop -> true
          | Raise _ | Branch _ | Cond _ | Switch _ | Pushtrap _ | Poptrap _ -> false)
    in
    match ends with
    | [] -> []
    | e :: r ->
        (* The blocks dominating the ends of the program, in order *)
        let last = List.fold_left r ~init:e ~f:common_dominator in
        let rec dominators pc acc =
          match Addr.Hashtbl.find_opt idom pc with
          | Some d -> dominators d (pc :: acc)
          | None -> pc :: acc
        in
        let chain = Array.of_list (dominators last []) in
        let m = Array.length chain in
        let index = Addr.Hashtbl.create 1024 in
        Array.iteri chain ~f:(fun i pc -> Addr.Hashtbl.add index pc i);
        (* The last block of the chain dominating each block *)
        let level = Addr.Hashtbl.create 1024 in
        List.iter rpo ~f:(fun pc' ->
            Addr.Hashtbl.replace
              level
              pc'
              (match Addr.Hashtbl.find_opt index pc' with
              | Some i -> i
              | None -> Addr.Hashtbl.find level (Addr.Hashtbl.find idom pc')));
        (* A block [chain.(i)] cannot start a chunk if some code it dominates
           jumps to code it does not dominate *)
        let excluded = Array.make (m + 1) 0 in
        let sizes = Array.make m 0 in
        List.iter rpo ~f:(fun pc' ->
            let l = Addr.Hashtbl.find level pc' in
            sizes.(l) <- sizes.(l) + List.length (Addr.Map.find pc' blocks).body;
            Code.fold_children
              blocks
              pc'
              (fun pc'' () ->
                let l' = Addr.Hashtbl.find level pc'' in
                if l' < l
                then (
                  excluded.(l' + 1) <- excluded.(l' + 1) + 1;
                  excluded.(l + 1) <- excluded.(l + 1) - 1))
              ());
        let candidate =
          let n = ref 0 in
          Array.init m ~f:(fun i ->
              n := !n + excluded.(i);
              let pc = chain.(i) in
              let block = Addr.Map.find pc blocks in
              i > 0
              && !n = 0
              && List.is_empty block.params
              && List.is_empty (Addr.Hashtbl.find ctx.traps pc)
              && Var.Set.is_empty (Addr.Hashtbl.find live_in pc))
        in
        (* The chunks are named after the first debug event and the first
           global they define *)
        let next_global = Addr.Hashtbl.create 1024 in
        let next = ref "" in
        List.iter (List.rev rpo) ~f:(fun pc' ->
            List.iter (List.rev (Addr.Map.find pc' blocks).body) ~f:(fun i ->
                match instr_defs i with
                | Some x -> (
                    match Var.Hashtbl.find_opt out.globals x with
                    | Some g -> next := g
                    | None -> ())
                | None -> ());
            Addr.Hashtbl.replace next_global pc' !next);
        let ranks = String.Hashtbl.create 64 in
        let name pc =
          let key =
            (match first_event blocks (pc, []) with
              | Some pi -> Printf.sprintf "%s:%d:%d" (source_file pi) pi.line pi.col
              | None -> "")
            ^ ":"
            ^ Addr.Hashtbl.find next_global pc
          in
          let rank =
            match String.Hashtbl.find_opt ranks key with
            | Some n -> n
            | None -> 0
          in
          String.Hashtbl.replace ranks key (rank + 1);
          Printf.sprintf
            "%s%s"
            (digest_name "camlc_tl" key)
            (if rank = 0 then "" else Printf.sprintf "_%d" rank)
        in
        let starts = ref [ 0, name pc ] in
        let n = ref 0 in
        let since = ref 0 in
        for i = 1 to m - 1 do
          n := !n + sizes.(i - 1);
          since := !since + sizes.(i - 1);
          if candidate.(i)
          then (
            let name = name chain.(i) in
            if
              !n >= 2 * toplevel_chunk_size
              || !n >= toplevel_chunk_size / 2
                 && Hashtbl.hash name land 0xfffff
                    < !since * 0x100000 / max 1 (toplevel_chunk_size / 2)
            then (
              starts := (i, name) :: !starts;
              n := 0);
            since := 0)
        done;
        let starts = Array.of_list (List.rev !starts) in
        if Array.length starts < 2
        then []
        else
          (* The code of each chunk: the jumps to the start of the next one
             go to a block which returns, and its first block starts with the
             last debug event before it *)
          let preds = Addr.Hashtbl.create 64 in
          Array.iter starts ~f:(fun (i, _) -> Addr.Hashtbl.replace preds chain.(i) []);
          List.iter rpo ~f:(fun pc' ->
              Code.fold_children
                blocks
                pc'
                (fun pc'' () ->
                  match Addr.Hashtbl.find_opt preds pc'' with
                  | Some l -> Addr.Hashtbl.replace preds pc'' (pc' :: l)
                  | None -> ())
                ());
          let events = Addr.Hashtbl.create 64 in
          let last_event = ref None in
          List.iter rpo ~f:(fun pc' ->
              if Addr.Hashtbl.mem preds pc'
              then Addr.Hashtbl.replace events pc' !last_event;
              List.iter (Addr.Map.find pc' blocks).body ~f:(fun i ->
                  match i with
                  | Event pi -> last_event := Some pi
                  | _ -> ()));
          let exit_pc = free_pc in
          Array.to_list
            (Array.mapi starts ~f:(fun k (i, name) ->
                 let start = chain.(i) in
                 let blocks =
                   match Addr.Hashtbl.find events start with
                   | Some pi when k > 0 ->
                       let block = Addr.Map.find start blocks in
                       Addr.Map.add
                         start
                         { block with body = Event pi :: block.body }
                         blocks
                   | Some _ | None -> blocks
                 in
                 let blocks =
                   if k + 1 = Array.length starts
                   then blocks
                   else
                     let next = chain.(fst starts.(k + 1)) in
                     let redirect ((pc', args) as cont) =
                       if pc' = next
                       then (
                         assert (List.is_empty args);
                         exit_pc, [])
                       else cont
                     in
                     List.fold_left
                       (Addr.Hashtbl.find preds next)
                       ~init:
                         (Addr.Map.add
                            exit_pc
                            { params = []; body = []; branch = Stop }
                            blocks)
                       ~f:(fun blocks pc' ->
                         let block = Addr.Map.find pc' blocks in
                         let branch =
                           match block.branch with
                           | Branch c -> Branch (redirect c)
                           | Poptrap c -> Poptrap (redirect c)
                           | Cond (x, c1, c2) -> Cond (x, redirect c1, redirect c2)
                           | Switch (x, conts) -> Switch (x, Array.map conts ~f:redirect)
                           | Pushtrap (c, x, h) -> Pushtrap (redirect c, x, redirect h)
                           | (Return _ | Raise _ | Stop) as b -> b
                         in
                         Addr.Map.add pc' { block with branch } blocks)
                 in
                 name, start, blocks))

type mode =
  [ `Program
  | `Dynlink
  | `Plugin
  ]

type result =
  { files : string list
  ; primitives : (string * int) list
  }

(* The maximal arity of the functions of the plugins of a program compiled
   with --dynlink (the runtime handles their partial applications with the
   tables of the program) *)
let dynlink_max_arity = 64

(* The maximal arity of the functions whose partial applications call them
   directly, see [camlc_curry_code] *)
let curry_max = 8

(* The maximal number of arguments of the partial applications allocated
   by [camlc_partial<n>] (small blocks) *)
let partial_max = 64

(* The names of the compilation units defined by [p], and of those it
   refers to *)
let units (p : program) =
  let defined = String.Hashtbl.create 16 in
  let imported = String.Hashtbl.create 16 in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let
              ( _
              , Prim
                  ( Extern ("caml_register_global", _)
                  , [ _; Pc (String n | NativeString (Byte n | Utf (Utf8 n))) ] ) ) ->
              String.Hashtbl.replace defined n ()
          | Let
              ( _
              , Prim
                  ( Extern ("caml_get_global", _)
                  , [ Pc (String n | NativeString (Byte n | Utf (Utf8 n))) ] ) ) ->
              String.Hashtbl.replace imported n ()
          | _ -> ()))
    p.blocks;
  let names t =
    List.sort ~cmp:String.compare (String.Hashtbl.fold (fun n () acc -> n :: acc) t [])
  in
  names defined, names imported

let f
    (p : program)
    ?(mode : mode = `Program)
    ?(interfaces = [])
    ~global_flow_data
    ~deadcode_sentinel
    ~debug_info
    ~in_cps
    ~trampolined_calls
    ?(toplevel_chunk_size = 2000)
    ?part_size
    closures =
  let global_flow_state, global_flow_info = global_flow_data in
  (* Toplevel values in globals: they are no longer free variables of the
     closures *)
  let globals = Var.Hashtbl.create 1024 in
  let global_list =
    Var.Set.elements (Globalize.f p (Structure.build_graph p.blocks p.start) closures)
  in
  name_globals p global_list globals;
  (* Nor are the constants and the closed closures, static data (and the
     variables bound to the closure of a function with two versions, see
     [caml_cps_closure]), which may close other closures *)
  let constants = Var.Hashtbl.create 1024 in
  let assigned = Var.Hashtbl.create 16 in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, Constant c) -> Var.Hashtbl.replace constants x c
          | Assign (x, _) -> Var.Hashtbl.replace assigned x ()
          | _ -> ()))
    p.blocks;
  Var.Hashtbl.iter (fun x () -> Var.Hashtbl.remove constants x) assigned;
  let closed = Var.Hashtbl.create 1024 in
  Var.Hashtbl.iter (fun x _ -> Var.Hashtbl.replace closed x ()) constants;
  let rec close_closures closures =
    Var.Map.iter
      (fun _ (info : Closure_conversion.closure) ->
        if List.is_empty info.free_variables
        then List.iter info.functions ~f:(fun (g, _) -> Var.Hashtbl.replace closed g ()))
      closures;
    Var.Hashtbl.iter
      (fun x (d, _) -> if Var.Hashtbl.mem closed d then Var.Hashtbl.replace closed x ())
      global_flow_info.Global_flow.info_cps_closures;
    let changed = ref false in
    let closures =
      Var.Map.map
        (fun (info : Closure_conversion.closure) ->
          let free_variables =
            List.filter info.free_variables ~f:(fun x ->
                not (Var.Hashtbl.mem globals x || Var.Hashtbl.mem closed x))
          in
          if List.length free_variables <> List.length info.free_variables
          then changed := true;
          { info with free_variables })
        closures
    in
    if !changed then close_closures closures else closures
  in
  let closures = close_closures closures in
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
  let located_functions = collect_functions p in
  let functions =
    List.map located_functions ~f:(fun (f, params, cont, _) -> f, params, cont)
  in
  (* Which numbers can be unboxed. Closures hold values, and so do the
     parameters of the CPS versions, which the runtime may call. *)
  Typing.reset ();
  register_typing_primitives ();
  Typing.prepare ~global_flow_state p;
  let fun_info = Call_graph_analysis.f p global_flow_info in
  let boxed =
    Var.Map.fold
      (fun _ (info : Closure_conversion.closure) acc -> info.free_variables @ acc)
      closures
      global_list
  in
  let boxed =
    List.fold_left functions ~init:boxed ~f:(fun acc (f, params, _) ->
        if Var.Set.mem f cps_sides then params @ acc else acc)
  in
  let typing ~unboxed_returns =
    Typing.f
      ~global_flow_state
      ~global_flow_info
      ~fun_info
      ~unboxed_returns
      ~deadcode_sentinel
      ~boxed
      p
  in
  (* First with boxed return values, to find the functions which cannot
     raise (see below) *)
  let types = typing ~unboxed_returns:(fun _ -> false) in
  let out =
    { data = Buffer.create 65536
    ; static_names = String.Hashtbl.create 1024
    ; static_list = []
    ; definitions = []
    ; refs = Var.Set.empty
    ; constants = Int.Hashtbl.create 1024
    ; primitives = String.Hashtbl.create 128
    ; apply_arities = IntSet.empty
    ; max_arity = 1
    ; global_data_size = global_data_size p
    ; fun_names = Var.Hashtbl.create 1024
    ; types
    ; fun_reprs = Var.Hashtbl.create 1024
    ; fun_returns = Var.Hashtbl.create 1024
    ; debug_info
    ; locations = String.Hashtbl.create 1024
    ; segment_thunks = String.Hashtbl.create 16
    ; cps_fallback = Var.Set.empty
    ; components = Var.Hashtbl.create 16
    ; unknown_tail_calls = Var.Set.empty
    ; in_cps
    ; trampolined = trampolined_calls
    ; cps_pairs
    ; cps_sides
    ; cps_apply_arities = IntSet.empty
    ; globals
    ; statics = Var.Hashtbl.create 1024
    ; split = Option.is_some part_size
    ; polling = uses_threads p || uses_memprof p
    ; memprof = uses_memprof p
    ; raising = Var.Set.empty
    ; collecting = Var.Set.empty
    ; collecting_on_raise = Var.Set.empty
    }
  in
  name_functions out p.blocks located_functions;
  let set_reprs () =
    List.iter functions ~f:(fun (f, params, _) ->
        Var.Hashtbl.replace
          out.fun_reprs
          f
          (List.map params ~f:(fun x -> repr_of_type (Typing.var_type out.types x)));
        Var.Hashtbl.replace
          out.fun_returns
          f
          (repr_of_type (Typing.return_type out.types f)))
  in
  set_reprs ();
  List.iter functions ~f:(fun (f, params, _) ->
      out.max_arity <- max out.max_arity (List.length params);
      ignore (fun_name out f));
  let functions_max_arity = out.max_arity in
  (match mode with
  | `Dynlink -> out.max_arity <- max out.max_arity dynlink_max_arity
  | `Program | `Plugin -> ());
  (* The static data of the constants and the closed closures *)
  Var.Hashtbl.iter
    (fun x c -> Var.Hashtbl.replace out.statics x (code_constant out c))
    constants;
  Var.Map.iter
    (fun f (info : Closure_conversion.closure) ->
      match info.functions with
      | (f0, _) :: _
        when Var.equal f f0
             && List.is_empty info.free_variables
             && not (Var.Set.mem f cps_sides) ->
          List.iter (static_closure out info.functions) ~f:(fun (g, e) ->
              Var.Hashtbl.replace out.statics g e)
      | _ -> ())
    closures;
  Var.Hashtbl.iter
    (fun x (d, _) ->
      match Var.Hashtbl.find_opt out.statics d with
      | Some e -> Var.Hashtbl.replace out.statics x e
      | None -> ())
    global_flow_info.Global_flow.info_cps_closures;
  let data_refs = out.refs in
  analyze_functions out facts global_flow_info closures p.blocks functions;
  (* The functions which cannot raise may return unboxed numbers (when they
     are only called directly), except those with two versions and those
     called in direct style by CPS code, whose result goes to a
     continuation. Whether a function raises does not depend on the
     boxing of numbers. *)
  let direct_style =
    Addr.Map.fold
      (fun _ block acc ->
        List.fold_left block.body ~init:acc ~f:(fun acc i ->
            match i with
            | Let (x, Apply { f; exact; _ }) -> (
                match resolve_callee global_flow_info closures x ~f ~exact with
                | Known_direct g -> Var.Set.add g acc
                | Known _ | Unknown -> acc)
            | _ -> acc))
      p.blocks
      Var.Set.empty
  in
  let unboxed_returns f =
    not
      (Var.Set.mem f out.raising
      || Var.Set.mem f cps_sides
      || Var.Map.mem f cps_pairs
      || Var.Set.mem f direct_style)
  in
  if List.exists functions ~f:(fun (f, _, _) -> unboxed_returns f)
  then (
    out.types <- typing ~unboxed_returns;
    set_reprs ();
    analyze_functions out facts global_flow_info closures p.blocks functions);
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
        ~fn:(Some f)
        ~name:
          (Printf.sprintf
             "%s %s"
             (c_type (Var.Hashtbl.find out.fun_returns f))
             (fun_name out f))
        ~params
        ~param_reprs:(Var.Hashtbl.find out.fun_reprs f)
        ~env
        ~static_env:(List.is_empty info.free_variables)
        ~source_name:(Var.get_name f)
        ~raises:(Var.Set.mem f out.raising)
        cont);
  let toplevel_name =
    match mode with
    | `Program | `Dynlink -> "camlc_program"
    | `Plugin -> "camlc_plugin_program"
  in
  let translate_toplevel ~name blocks pc =
    translate_function
      out
      facts
      global_flow_info
      blocks
      closures
      ~fn:None
      ~name
      ~params:[]
      ~param_reprs:[]
      ~env:[]
      ~static_env:true
      ~source_name:None
      ~raises:true
      (pc, [])
  in
  (match
     toplevel_chunks
       out
       facts
       global_flow_info
       closures
       p.blocks
       ~toplevel_chunk_size
       ~free_pc:p.free_pc
       p.start
   with
  | [] -> translate_toplevel ~name:("value " ^ toplevel_name) p.blocks p.start
  | chunks ->
      List.iter chunks ~f:(fun (name, pc, blocks) ->
          translate_toplevel ~name:("__attribute__((noinline)) value " ^ name) blocks pc);
      let b = Buffer.create 4096 in
      List.iter chunks ~f:(fun (name, _, _) -> Printf.bprintf b "value %s(void);\n" name);
      Printf.bprintf b "\nvalue %s(void) {\n  value r;\n  (void)r;\n" toplevel_name;
      let n = List.length chunks in
      List.iteri chunks ~f:(fun i (name, _, _) ->
          if i = n - 1
          then Printf.bprintf b "  return %s();\n" name
          else
            Printf.bprintf
              b
              "  r = %s();\n  if (CAMLC_UNLIKELY(Is_exception_result(r))) return r;\n"
              name);
      Buffer.add_string b "}\n\n";
      out.definitions <-
        { def_fn = None
        ; def_name = toplevel_name
        ; def_text = Buffer.contents b
        ; def_size = Buffer.length b
        ; def_refs = Var.Set.empty
        }
        :: out.definitions);
  (* Assemble the output. Split, the functions are grouped in order into
     parts of about [part_size] bytes each, after a part with the static
     data and the tables. A function is static unless another part, or the
     static data, refers to it. *)
  let definitions = List.rev out.definitions in
  let parts =
    match part_size with
    | None -> [ definitions ]
    | Some size ->
        (* A part ends after a definition chosen by the hash of its name,
           with a probability proportional to its size, once the part has
           half the size: the parts after a definition which changes are
           left unchanged *)
        let ends_part d =
          Hashtbl.hash d.def_name land 0xfffff < d.def_size * 0x100000 / max 1 (size / 2)
        in
        let rec group parts part n l =
          match l with
          | [] -> List.rev (if List.is_empty part then parts else List.rev part :: parts)
          | d :: r ->
              if n > 0 && n + d.def_size > 2 * size
              then group (List.rev part :: parts) [] 0 l
              else
                let n = n + d.def_size in
                if n >= size / 2 && ends_part d
                then group (List.rev (d :: part) :: parts) [] 0 r
                else group parts (d :: part) n r
        in
        group [] [] 0 definitions
  in
  let split = Option.is_some part_size in
  let shared = Var.Hashtbl.create 1024 in
  if split
  then (
    let part_of = Var.Hashtbl.create 1024 in
    List.iteri parts ~f:(fun i l ->
        List.iter l ~f:(fun d ->
            Option.iter d.def_fn ~f:(fun f -> Var.Hashtbl.replace part_of f i)));
    Var.Set.iter (fun f -> Var.Hashtbl.replace shared f ()) data_refs;
    List.iteri parts ~f:(fun i l ->
        List.iter l ~f:(fun d ->
            Var.Set.iter
              (fun g ->
                match Var.Hashtbl.find_opt part_of g with
                | Some j when j = i -> ()
                | _ -> Var.Hashtbl.replace shared g ())
              d.def_refs)));
  let storage f = if Var.Hashtbl.mem shared f then "" else "static " in
  let dt = double_translation () in
  (* The locations, constants and globals that some code refers to: the
     identifiers of its text starting with [camlc_l_], [camlc_k_] and
     [camlc_g_] *)
  let symbols texts =
    let is_ident c =
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
      | _ -> false
    in
    let found = String.Hashtbl.create 64 in
    List.iter texts ~f:(fun text ->
        let n = String.length text in
        let i = ref 0 in
        while !i + 8 <= n do
          if
            Char.equal text.[!i] 'c'
            && (!i = 0 || not (is_ident text.[!i - 1]))
            && String.equal (String.sub text ~pos:!i ~len:6) "camlc_"
            && Char.equal text.[!i + 7] '_'
            &&
            match text.[!i + 6] with
            | 'l' | 'k' | 'g' | 's' | 'c' | 't' -> true
            | _ -> false
          then (
            let j = ref (!i + 8) in
            while !j < n && is_ident text.[!j] do
              incr j
            done;
            String.Hashtbl.replace found (String.sub text ~pos:!i ~len:(!j - !i)) ();
            i := !j)
          else incr i
        done);
    List.sort
      ~cmp:String.compare
      (String.Hashtbl.fold (fun name () acc -> name :: acc) found [])
  in
  let declare_symbols b names =
    List.iter names ~f:(fun name ->
        match name.[6] with
        | 'l' ->
            Printf.bprintf
              b
              "static const struct camlc_location %s = %s;\n"
              name
              (String.Hashtbl.find out.locations name)
        | 'k' -> Printf.bprintf b "extern value %s[];\n" name
        | 'c' ->
            (* camlc_c_N: applies the CPS version of a function of arity N
               (see [camlc_cps_stack]). Not inlined, its arguments in an
               array on the shadow stack: an array in a C variable would
               take room on emscripten's stack, which the segmented stack
               does not measure nor switch *)
            let n =
              int_of_string (String.sub name ~pos:8 ~len:(String.length name - 8))
            in
            let args = List.init ~len:n ~f:(fun i -> Printf.sprintf "a%d" i) in
            Printf.bprintf
              b
              "static __attribute__((noinline)) value %s(value f, %s) { value *args = \
               camlc_sp - %d, r; if (CAMLC_UNLIKELY(args < camlc_stack_limit)) return \
               camlc_stack_overflow(); camlc_sp = args; %s r = camlc_cps_stack(f, %d, \
               args); camlc_sp = args + %d; return r; }\n"
              name
              (String.concat ~sep:", " (List.map args ~f:(fun a -> "value " ^ a)))
              n
              (String.concat
                 ~sep:" "
                 (List.mapi args ~f:(fun i a -> Printf.sprintf "args[%d] = %s;" i a)))
              n
              n
        | 's' ->
            (* camlc_s_R_ARGS: calls a function of result R and arguments
               ARGS (see [repr_letter]) on a new segment, through its
               thunk, which finds them in [camlc_seg] *)
            let c_type c =
              match c with
              | 'v' -> "value"
              | 'd' -> "double"
              | 'l' -> "int64_t"
              | 'i' -> "int32_t"
              | _ -> "intnat"
            in
            let args = String.sub name ~pos:10 ~len:(String.length name - 10) in
            let args = List.init ~len:(String.length args) ~f:(fun i -> args.[i]) in
            let ret = name.[8] in
            Printf.bprintf
              b
              "static void %s_thunk(void) { camlc_seg[0].%c = ((%s \
               (*)(%s))camlc_seg_fn)(%s); }\n\
               static __attribute__((noinline)) %s %s(%s) { %s camlc_seg_fn = f; \
               camlc_segment(%s_thunk, words); return camlc_seg[0].%c; }\n"
              name
              ret
              (c_type ret)
              (String.concat ~sep:", " (List.map args ~f:c_type))
              (String.concat
                 ~sep:", "
                 (List.mapi args ~f:(fun i c ->
                      Printf.sprintf "camlc_seg[%d].%c" (i + 1) c)))
              (c_type ret)
              name
              (String.concat
                 ~sep:", "
                 ("void *f"
                 :: "mlsize_t words"
                 :: List.mapi args ~f:(fun i c -> Printf.sprintf "%s a%d" (c_type c) i)))
              (String.concat
                 ~sep:" "
                 (List.mapi args ~f:(fun i c ->
                      Printf.sprintf "camlc_seg[%d].%c = a%d;" (i + 1) c i)))
              name
              ret
        | 't' ->
            (* camlc_t_R_CALLER_CALLEE: a function of the signature of a
               caller (result R and arguments CALLER, see [repr_letter])
               which calls [camlc_seg_fn] with arguments CALLEE found in
               [camlc_seg] (see [translate_tail_call]) *)
            let c_type c =
              match c with
              | 'v' -> "value"
              | 'd' -> "double"
              | 'l' -> "int64_t"
              | 'i' -> "int32_t"
              | _ -> "intnat"
            in
            let ret = name.[8] in
            let caller, callee =
              match
                String.split_on_char
                  ~sep:'_'
                  (String.sub name ~pos:10 ~len:(String.length name - 10))
              with
              | [ a; b ] -> a, b
              | _ -> assert false
            in
            let chars s = List.init ~len:(String.length s) ~f:(fun i -> s.[i]) in
            Printf.bprintf
              b
              "static __attribute__((noinline)) %s %s(%s) { return ((%s \
               (*)(%s))camlc_seg_fn)(%s); }\n"
              (c_type ret)
              name
              (String.concat
                 ~sep:", "
                 (List.mapi (chars caller) ~f:(fun i c ->
                      Printf.sprintf "%s a%d" (c_type c) i)))
              (c_type ret)
              (String.concat ~sep:", " (List.map (chars callee) ~f:c_type))
              (String.concat
                 ~sep:", "
                 (List.mapi (chars callee) ~f:(fun i c ->
                      Printf.sprintf "camlc_seg[%d].%c" i c)))
        | _ -> Printf.bprintf b "extern value %s;\n" name);
    Buffer.add_char b '\n'
  in
  let preamble b =
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
    Buffer.add_char b '\n';
    match mode with
    | `Plugin ->
        (* The symbols of a plugin are local to it, except its entry point:
           they cannot conflict with those of the program or of other
           plugins *)
        Buffer.add_string b "#pragma GCC visibility push(hidden)\n\n"
    | `Program | `Dynlink -> ()
  in
  let prototypes b l =
    List.iter l ~f:(fun f ->
        Printf.bprintf
          b
          "%s%s %s(%s);\n"
          (storage f)
          (c_type (Var.Hashtbl.find out.fun_returns f))
          (fun_name out f)
          (String.concat
             ~sep:", "
             (List.map (Var.Hashtbl.find out.fun_reprs f @ [ Value ]) ~f:c_type)));
    Buffer.add_char b '\n'
  in
  (* The static data and the tables of the runtime *)
  let tables b =
    (* Never called: a direct call makes wasm-ld report the C functions
       whose definition does not have the signature of the call, which would
       trap when called through a function pointer *)
    Buffer.add_string b "void camlc_signature_check(void) {\n";
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
    Buffer.add_string b "}\n\n";
    let program =
      match mode with
      | `Program | `Dynlink -> true
      | `Plugin -> false
    in
    for m = 1 to if program then out.max_arity - 1 else 0 do
      Printf.bprintf b "static value camlc_pap%d(%s);\n" m (fun_params m);
      if dt
      then Printf.bprintf b "static value camlc_pap_cps%d(%s);\n" m (fun_params (m + 1))
    done;
    let curry_max = if program then min out.max_arity curry_max else 0 in
    for a = 2 to curry_max do
      for n = 1 to a - 1 do
        Printf.bprintf b "static value camlc_curry%d_%d(%s);\n" a n (fun_params (a - n))
      done
    done;
    Buffer.add_char b '\n';
    if program
    then Printf.bprintf b "const int camlc_global_data_size = %d;\n" out.global_data_size;
    (* Never static: the GC updates them, which the C compiler must not
       ignore *)
    List.iter global_list ~f:(fun x ->
        Printf.bprintf b "value %s = Val_unit;\n" (Var.Hashtbl.find globals x));
    if program
    then (
      Printf.bprintf b "const int camlc_max_arity = %d;\n" out.max_arity;
      Printf.bprintf
        b
        "const value camlc_pap_code[] = { 0%s };\n"
        (String.concat
           ~sep:""
           (List.init ~len:(out.max_arity - 1) ~f:(fun i ->
                Printf.sprintf ", (value)&camlc_pap%d" (i + 1))));
      Printf.bprintf b "const int camlc_curry_max = %d;\n" curry_max;
      Printf.bprintf
        b
        "const value camlc_curry_code[] = { %s };\n"
        (String.concat
           ~sep:", "
           (List.concat
              (List.init
                 ~len:(max 0 (curry_max - 1))
                 ~f:(fun i ->
                   let a = i + 2 in
                   List.init ~len:(a - 1) ~f:(fun n ->
                       Printf.sprintf "(value)&camlc_curry%d_%d" a (n + 1))))
           @ [ "0" ]));
      Printf.bprintf b "const int camlc_double_translation = %d;\n" (if dt then 1 else 0);
      Printf.bprintf
        b
        "const int camlc_cps_stack_enabled = %d;\n"
        (if dt && Config.Flag.cps_recursions () then 1 else 0);
      Printf.bprintf
        b
        "const value camlc_pap_cps_code[] = { 0%s };\n"
        (if dt
         then
           String.concat
             ~sep:""
             (List.init ~len:(out.max_arity - 1) ~f:(fun i ->
                  Printf.sprintf ", (value)&camlc_pap_cps%d" (i + 1)))
         else ""));
    (match mode with
    | `Dynlink ->
        (* The functions of the C library which the code of the plugins
           calls directly: the program links them, for its plugins *)
        Printf.bprintf
          b
          "__attribute__((used)) static void (*const camlc_plugin_c_functions[])(void) = \
           { %s };\n"
          (String.concat
             ~sep:", "
             (List.map plugin_c_functions ~f:(Printf.sprintf "(void (*)(void))&%s")))
    | `Program | `Plugin -> ());
    Buffer.add_char b '\n';
    if Config.Flag.small_functions ()
    then (
      (* V8 inlines less in a module whose functions are mostly large: its
         inlining budget is a tenth of the full one when less than a quarter
         of them are under 50 bytes, and full above half. Few compiled
         functions are that small (a sixth of those of ocamlc), so the
         module gets functions returning a constant: three quarters of the
         number of generated functions (some are inlined), and some for
         those of the runtime. They are in the table of functions, so that
         neither the linker nor binaryen removes them. *)
      let n = (3 * List.length definitions / 4) + 500 in
      Buffer.add_string
        b
        "#define CAMLC_PAD(n) static int camlc_pad##n(void) { return n; }\n";
      for i = 0 to n - 1 do
        Printf.bprintf b "CAMLC_PAD(%d)%c" i (if i mod 8 = 7 then '\n' else ' ')
      done;
      Buffer.add_string
        b
        "\n__attribute__((used)) static int (*const camlc_pads[])(void) = {\n";
      for i = 0 to n - 1 do
        Printf.bprintf b "camlc_pad%d,%c" i (if i mod 8 = 7 then '\n' else ' ')
      done;
      Buffer.add_string b "};\n\n");
    Buffer.add_buffer b out.data;
    Buffer.add_char b '\n';
    (* The globals and the static data, for the runtime *)
    let table name l =
      Printf.bprintf b "static value *const %s[] = {\n" name;
      List.iter l ~f:(fun e -> Printf.bprintf b "  %s,\n" e);
      Printf.bprintf b "  0\n};\n"
    in
    table
      "camlc_globals_table"
      (List.map global_list ~f:(fun x -> "&" ^ Var.Hashtbl.find globals x));
    table "camlc_static_table" (List.rev out.static_list);
    let interface_table storage name =
      Printf.bprintf
        b
        "%sconst char *const %s[] = { %s0 };\n"
        storage
        name
        (String.concat
           ~sep:""
           (List.map interfaces ~f:(fun (n, crc) ->
                Printf.sprintf "%s, %s, " (c_string n) (c_string crc))))
    in
    (match mode with
    | `Program | `Dynlink ->
        Buffer.add_string
          b
          "const struct camlc_data camlc_program_data = { camlc_globals_table, \
           camlc_static_table };\n";
        interface_table "" "camlc_program_interfaces";
        Buffer.add_char b '\n'
    | `Plugin ->
        (* The entry point of the plugin *)
        let defined, imported = units p in
        let names name l =
          Printf.bprintf
            b
            "static const char *const %s[] = { %s0 };\n"
            name
            (String.concat ~sep:"" (List.map l ~f:(fun n -> c_string n ^ ", ")))
        in
        names "camlc_plugin_units" defined;
        names "camlc_plugin_imports" imported;
        interface_table "static " "camlc_plugin_interfaces";
        Printf.bprintf
          b
          "value camlc_plugin_program(void);\n\n\
           static const struct camlc_plugin camlc_plugin = {\n\
          \  { camlc_globals_table, camlc_static_table },\n\
          \  camlc_plugin_units,\n\
          \  camlc_plugin_imports,\n\
          \  camlc_plugin_interfaces,\n\
          \  %d,\n\
          \  %d,\n\
          \  camlc_plugin_program\n\
           };\n\n\
           __attribute__((visibility(\"default\")))\n\
           value camlc_plugin_init(void) { return camlc_load_plugin(&camlc_plugin); }\n\n"
          (if dt then 1 else 0)
          functions_max_arity);
    (* Partial applications: the closure holds the function and the
       arguments already given (after the code pointer of the CPS version,
       with effect handlers), see [camlc_make_pap]. Their application to the
       arguments left is a tail call, as in OCaml: [camlc_papM] puts all the
       arguments in [camlc_seg] and its only return calls
       [camlc_pap_enterA], A the arity of the function, through a table:
       a tail call of another signature, as [camlc_pap_enterA]'s *)
    let e = if dt then 3 else 2 in
    let seg_args n =
      String.concat
        ~sep:", "
        (List.init ~len:n ~f:(fun i -> Printf.sprintf "camlc_seg[%d].v" i) @ [ "f" ])
    in
    let enter_table name =
      Printf.bprintf
        b
        "static value (*const %s[])(value) = { 0, 0%s };\n\n"
        name
        (String.concat
           ~sep:""
           (List.init ~len:(out.max_arity - 1) ~f:(fun i ->
                Printf.sprintf ", %s%d" name (i + 2))))
    in
    if program
    then (
      for a = 2 to out.max_arity do
        Printf.bprintf
          b
          "static __attribute__((noinline)) value camlc_pap_enter%d(value f) {\n\
          \  return ((%s)Field(f, 0))(%s);\n\
           }\n\n"
          a
          (fun_type a)
          (seg_args a);
        if dt
        then
          Printf.bprintf
            b
            "static __attribute__((noinline)) value camlc_pap_enter_cps%d(value f) {\n\
            \  return ((%s)Field(f, 2))(%s);\n\
             }\n\n"
            a
            (fun_type (a + 1))
            (seg_args (a + 1))
      done;
      enter_table "camlc_pap_enter";
      if dt then enter_table "camlc_pap_enter_cps");
    for m = 1 to if program then out.max_arity - 1 else 0 do
      Printf.bprintf
        b
        "static value camlc_pap%d(%s) {\n\
        \  mlsize_t k = Camlc_wosize_val(env) - %d;\n\
        \  for (mlsize_t i = 0; i < k; i++) camlc_seg[i].v = Field(env, %d + i);\n\
         %s  return camlc_pap_enter[k + %d](Field(env, %d));\n\
         }\n\n"
        m
        (fun_params m)
        (e + 1)
        (e + 1)
        (String.concat
           ~sep:""
           (List.init ~len:m ~f:(fun i ->
                Printf.sprintf "  camlc_seg[k + %d].v = a%d;\n" i i)))
        m
        e;
      if dt
      then
        (* CPS version: the last argument is the continuation. The function
           has a CPS version, see [camlc_make_pap] *)
        Printf.bprintf
          b
          "static value camlc_pap_cps%d(%s) {\n\
          \  mlsize_t k = Camlc_wosize_val(env) - 4;\n\
          \  for (mlsize_t i = 0; i < k; i++) camlc_seg[i].v = Field(env, 4 + i);\n\
           %s  return camlc_pap_enter_cps[k + %d](Field(env, 3));\n\
           }\n\n"
          m
          (fun_params (m + 1))
          (String.concat
             ~sep:""
             (List.init ~len:(m + 1) ~f:(fun i ->
                  Printf.sprintf "  camlc_seg[k + %d].v = a%d;\n" i i)))
          m
    done;
    (* The partial applications of the functions of arity at most
       [curry_max]: once applied to the arguments left, they call the
       function directly *)
    for a = 2 to curry_max do
      for n = 1 to a - 1 do
        Printf.bprintf
          b
          "static value camlc_curry%d_%d(%s) {\n\
          \  value g = Field(env, %d);\n\
          \  return ((%s)Field(g, 0))(%s);\n\
           }\n\n"
          a
          n
          (fun_params (a - n))
          e
          (fun_type a)
          (String.concat
             ~sep:", "
             (List.init ~len:n ~f:(fun i -> Printf.sprintf "Field(env, %d)" (e + 1 + i))
             @ List.init ~len:(a - n) ~f:(fun i -> Printf.sprintf "a%d" i)
             @ [ "g" ]))
      done
    done;
    if program
    then (
      (* Exact calls with the arguments in an array. The error is in a
         function of its own: the arguments of a variadic call are on the C
         stack of the linear memory, which would give [camlc_call_exact] a
         frame there, during the call *)
      Printf.bprintf
        b
        "static __attribute__((noinline, noreturn)) void camlc_arity_error(int n) {\n\
        \  caml_fatal_error(\"c_of_ocaml: arity %%d\", n);\n\
         }\n\n\
         value camlc_call_exact(value f, int n, value *args) {\n\
        \  switch (n) {\n";
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
      Printf.bprintf b "  default: camlc_arity_error(n);\n  }\n}\n\n";
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
      Printf.bprintf b "  default: camlc_arity_error(n);\n  }\n}\n\n")
  in
  (* The application functions, in each part which may use them *)
  let helpers b =
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
          \  if (c != CAMLC_NO_CPS) CAMLC_MUSTTAIL return ((%s)c)(%s, k, f);\n\
          \  CAMLC_MUSTTAIL return camlc_direct_k%d(f, %s, k);\n\
           }\n\n\
           static __attribute__((noinline)) value camlc_apply_cps%d_slow(value f, %s, \
           value k) {\n\
          \  value *args = camlc_sp - %d, r;\n\
          \  if (CAMLC_UNLIKELY(args < camlc_stack_limit)) return camlc_stack_overflow();\n\
          \  camlc_sp = args;\n\
          \  %s args[%d] = k;\n\
          \  r = camlc_applyv_cps(f, %d, args);\n\
          \  camlc_sp = args + %d;\n\
          \  return r;\n\
           }\n\n\
           static inline value camlc_apply_cps%d(value f, %s, value k) {\n\
          \  if (Arity_closinfo(Closinfo_val(f)) == %d)\n\
          \    CAMLC_MUSTTAIL return camlc_call_cps%d(f, %s, k);\n\
          \  CAMLC_MUSTTAIL return camlc_apply_cps%d_slow(f, %s, k);\n\
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
          m
          (String.concat
             ~sep:" "
             (List.mapi args ~f:(fun i a -> Printf.sprintf "args[%d] = %s;" i a)))
          n
          n
          m
          m
          params
          n
          m
          (String.concat ~sep:", " args)
          m
          (String.concat ~sep:", " args))
      out.cps_apply_arities;
    (* The partial applications of unknown functions to [n] arguments, as
       [camlc_make_pap]; not in plugins, which do not see the tables of the
       program *)
    let partial_max =
      match mode with
      | `Program | `Dynlink -> partial_max
      | `Plugin -> 0
    in
    let e = if dt then 3 else 2 in
    IntSet.iter
      (fun n ->
        Printf.bprintf
          b
          "static value camlc_partial%d(value f, %s) {\n\
          \  int arity = Arity_closinfo(Closinfo_val(f));\n\
          \  value *fp = camlc_sp - %d, r;\n\
          \  if (CAMLC_UNLIKELY(fp < camlc_stack_limit)) return camlc_stack_overflow();\n\
          \  camlc_sp = fp;\n\
          \  fp[0] = f;\n\
           %s  r = camlc_alloc_small(%d, Closure_tag);\n\
          \  Field(r, 0) = arity <= camlc_curry_max\n\
          \    ? camlc_curry_code[(arity - 1) * (arity - 2) / 2 + %d]\n\
          \    : camlc_pap_code[arity - %d];\n\
          \  Field(r, 1) = Make_closinfo(arity - %d, %d);\n\
           %s  Field(r, %d) = fp[0];\n\
           %s  camlc_sp = fp + %d;\n\
          \  return r;\n\
           }\n\n"
          n
          (String.concat
             ~sep:", "
             (List.init ~len:n ~f:(fun i -> Printf.sprintf "value a%d" i)))
          (n + 1)
          (String.concat
             ~sep:""
             (List.init ~len:n ~f:(fun i -> Printf.sprintf "  fp[%d] = a%d;\n" (i + 1) i)))
          (e + 1 + n)
          (n - 1)
          n
          n
          e
          (if dt
           then
             Printf.sprintf
               "  Field(r, 2) = Field(fp[0], 2) != CAMLC_NO_CPS ? \
                camlc_pap_cps_code[arity - %d] : CAMLC_NO_CPS;\n"
               n
           else "")
          e
          (String.concat
             ~sep:""
             (List.init ~len:n ~f:(fun i ->
                  Printf.sprintf "  Field(r, %d) = fp[%d];\n" (e + 1 + i) (i + 1))))
          (n + 1))
      (IntSet.filter (fun n -> n <= partial_max) out.apply_arities);
    IntSet.iter
      (fun n ->
        let args = List.init ~len:n ~f:(fun i -> Printf.sprintf "a%d" i) in
        let params = String.concat ~sep:", " (List.map args ~f:(fun a -> "value " ^ a)) in
        let args = String.concat ~sep:", " args in
        (* The application to more arguments than the arity: the function
           and its arguments are on the shadow stack (see
           [camlc_apply_prefix]), in a function of its own: in an array of C,
           or inlined, they would take room on the C stack of the linear
           memory, during the whole call, which the segmented stack does not
           measure nor switch. The last application is a tail call, as in
           OCaml, through [camlc_lastN_K], of the signature of
           [camlc_applyN_slow]: a loop through such applications runs in
           constant space *)
        for k = 1 to n - 1 do
          Printf.bprintf
            b
            "static __attribute__((noinline)) value camlc_last%d_%d(value f, %s) {\n\
            \  return ((%s)Field(f, 0))(%s, f);\n\
             }\n\n"
            n
            k
            params
            (fun_type k)
            (String.concat
               ~sep:", "
               (List.init ~len:k ~f:(fun i -> Printf.sprintf "a%d" i)))
        done;
        Printf.bprintf
          b
          "static __attribute__((noinline)) value camlc_apply%d_slow(value f, %s) {\n\
          \  value *fp, r;\n\
          \  int i;\n\
           %s  fp = camlc_sp - %d;\n\
          \  if (CAMLC_UNLIKELY(fp < camlc_stack_limit)) return camlc_stack_overflow();\n\
          \  camlc_sp = fp;\n\
          \  fp[0] = f; %s\n\
           %s  i = camlc_apply_prefix(fp, %d);\n\
          \  if (CAMLC_UNLIKELY(i < 0)) {\n\
          \    camlc_sp = fp + %d;\n\
          \    return Make_exception_result(fp[0]);\n\
          \  }\n\
          \  if (Arity_closinfo(Closinfo_val(fp[0])) > %d - i) {\n\
          \    r = camlc_make_pap(fp[0], %d - i, fp + 1 + i);\n\
          \    camlc_sp = fp + %d;\n\
          \    return r;\n\
          \  }\n\
          \  camlc_sp = fp + %d;\n\
          \  switch (%d - i) {\n\
           %s  default: __builtin_trap();\n\
          \  }\n\
           }\n\n\
           static inline value camlc_apply%d(value f, %s) {\n\
          \  if (Arity_closinfo(Closinfo_val(f)) == %d)\n\
          \    CAMLC_MUSTTAIL return ((%s)Field(f, 0))(%s, f);\n\
          \  CAMLC_MUSTTAIL return camlc_apply%d_slow(f, %s);\n\
           }\n\n"
          n
          params
          (if n <= partial_max
           then
             Printf.sprintf
               "  if (Arity_closinfo(Closinfo_val(f)) > %d) return camlc_partial%d(f, %s);\n"
               n
               n
               args
           else "")
          (n + 1)
          (String.concat
             ~sep:" "
             (List.init ~len:n ~f:(fun i -> Printf.sprintf "fp[%d] = a%d;" (i + 1) i)))
          (if n <= partial_max
           then ""
           else
             Printf.sprintf
               "  if (Arity_closinfo(Closinfo_val(f)) > %d) {\n\
               \    r = camlc_make_pap(f, %d, fp + 1);\n\
               \    camlc_sp = fp + %d;\n\
               \    return r;\n\
               \  }\n"
               n
               n
               (n + 1))
          n
          (n + 1)
          n
          n
          (n + 1)
          (n + 1)
          n
          (String.concat
             ~sep:""
             (List.init ~len:(n - 1) ~f:(fun j ->
                  let k = j + 1 in
                  Printf.sprintf
                    "  case %d: CAMLC_MUSTTAIL return camlc_last%d_%d(fp[0], %s);\n"
                    k
                    n
                    k
                    (String.concat
                       ~sep:", "
                       (List.init ~len:n ~f:(fun l ->
                            if l < k
                            then Printf.sprintf "fp[%d + i]" (l + 1)
                            else "Val_unit"))))))
          n
          params
          n
          (fun_type n)
          args
          n
          args)
      out.apply_arities
  in
  let add_definitions b l =
    List.iter l ~f:(fun d ->
        (match d.def_fn with
        | Some f -> Buffer.add_string b (storage f)
        | None -> ());
        Buffer.add_string b d.def_text)
  in
  let files =
    if not split
    then (
      let b =
        Buffer.create
          (Buffer.length out.data
          + 65536
          + List.fold_left definitions ~init:0 ~f:(fun n d -> n + d.def_size))
      in
      preamble b;
      prototypes b (List.map functions ~f:(fun (f, _, _) -> f));
      tables b;
      String.Hashtbl.fold (fun name _ acc -> name :: acc) out.locations []
      |> String.Hashtbl.fold (fun name () acc -> name :: acc) out.segment_thunks
      |> List.sort ~cmp:String.compare
      |> declare_symbols b;
      helpers b;
      add_definitions b definitions;
      [ Buffer.contents b ])
    else
      let main =
        let b = Buffer.create (Buffer.length out.data + 65536) in
        preamble b;
        prototypes b (Var.Set.elements data_refs);
        tables b;
        Buffer.contents b
      in
      main
      :: List.map parts ~f:(fun l ->
          let b =
            Buffer.create (List.fold_left l ~init:65536 ~f:(fun n d -> n + d.def_size))
          in
          preamble b;
          declare_symbols b (symbols (List.map l ~f:(fun d -> d.def_text)));
          let used =
            List.fold_left l ~init:Var.Set.empty ~f:(fun s d ->
                let s = Var.Set.union s d.def_refs in
                match d.def_fn with
                | Some f -> Var.Set.add f s
                | None -> s)
          in
          prototypes b (Var.Set.elements used);
          helpers b;
          add_definitions b l;
          Buffer.contents b)
  in
  let files =
    match mode, files with
    | (`Program | `Dynlink), main :: parts ->
        (* The digest of the program, for the code pointers of marshaled
           closures (see camlc_runtime.c) *)
        let digest =
          Digest.string (String.concat ~sep:"" (List.map files ~f:Digest.string))
        in
        Printf.sprintf
          "%s\nconst unsigned char camlc_program_digest[16] = { %s };\n"
          main
          (String.concat
             ~sep:", "
             (List.init ~len:16 ~f:(fun i -> string_of_int (Char.code digest.[i]))))
        :: parts
    | _ -> files
  in
  { files
  ; primitives =
      List.sort
        ~cmp:(fun (a, _) (b, _) -> String.compare a b)
        (String.Hashtbl.fold
           (fun name n l -> (name, if n <= 5 then n else -1) :: l)
           out.primitives
           [])
  }
