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

let word_size = 4

(****)

let sanitize s =
  String.map s ~f:(fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> c
      | _ -> '_')

let tagged_int i =
  (* [Targetint] has 31 bits: the tagged representation fits in 32 bits *)
  let i = Targetint.to_int32 i in
  Printf.sprintf "((value)0x%lxu)" (Int32.add (Int32.shift_left i 1) 1l)

(****)

(* The output, and what the generated code refers to *)
type output =
  { data : Buffer.t  (** Static data *)
  ; functions : Buffer.t  (** Function definitions *)
  ; mutable data_count : int
  ; constants : (constant * string) list Int.Hashtbl.t
        (** Static data already emitted for each constant *)
  ; primitives : int String.Hashtbl.t  (** C primitives called, with their arity *)
  ; mutable apply_arities : IntSet.t  (** Arities of the [camlc_applyN] functions used *)
  ; mutable max_arity : int  (** Maximal arity of the functions *)
  ; global_data_size : int
  ; fun_names : string Var.Hashtbl.t  (** C name of each function *)
  }

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

let header ~wosize ~tag = Printf.sprintf "Make_header(%d, %d, CAMLC_STATIC_COLOR)" wosize tag

let words_of_bytes s =
  (* Little-endian words of the string [s] followed by the padding of
     OCaml strings *)
  let len = String.length s in
  let wosize = (len / word_size) + 1 in
  let bytes = Bytes.make (wosize * word_size) '\000' in
  Bytes.blit_string ~src:s ~src_pos:0 ~dst:bytes ~dst_pos:0 ~len;
  Bytes.set bytes ((wosize * word_size) - 1) (Char.chr ((wosize * word_size) - 1 - len));
  List.init ~len:wosize ~f:(fun i ->
      let w = ref 0l in
      for j = word_size - 1 downto 0 do
        w :=
          Int32.logor
            (Int32.shift_left !w 8)
            (Int32.of_int (Char.code (Bytes.get bytes ((i * word_size) + j))))
      done;
      Printf.sprintf "(value)0x%lxu" !w)

let int64_words i =
  [ Printf.sprintf "(value)0x%Lxu" (Int64.logand i 0xffffffffL)
  ; Printf.sprintf "(value)0x%Lxu" (Int64.shift_right_logical i 32)
  ]

let int32_word i = Printf.sprintf "(value)0x%lxu" i

let emit_block out ~tag fields =
  let n = out.data_count in
  out.data_count <- n + 1;
  let name = Printf.sprintf "camlc_d%d" n in
  Printf.bprintf
    out.data
    "static value %s[] = { %s };\n"
    name
    (String.concat ~sep:", " (header ~wosize:(List.length fields) ~tag :: fields));
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
  | Int64 i -> emit_block out ~tag:Obj.custom_tag ("(value)&caml_int64_ops" :: int64_words i)
  | Int32 i -> emit_block out ~tag:Obj.custom_tag [ "(value)&caml_int32_ops"; int32_word i ]
  | NativeInt i ->
      emit_block out ~tag:Obj.custom_tag [ "(value)&caml_nativeint_ops"; int32_word i ]
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
  | Plain of string
      (** A C expression which cannot raise. It may allocate only if it is a
          single call whose arguments are the arguments of the primitive *)
  | Checked of string
      (** A single call whose arguments are the arguments of the primitive,
          returning a value or an exception result *)

let c_call f args = Printf.sprintf "%s(%s)" f (String.concat ~sep:", " args)

(* C primitives which do not raise: they are called directly *)
let noraise_primitives =
  StringSet.of_list
    [ "caml_add_float"
    ; "caml_sub_float"
    ; "caml_mul_float"
    ; "caml_div_float"
    ; "caml_neg_float"
    ; "caml_abs_float"
    ; "caml_sqrt_float"
    ; "caml_exp_float"
    ; "caml_log_float"
    ; "caml_sin_float"
    ; "caml_cos_float"
    ; "caml_tan_float"
    ; "caml_floor_float"
    ; "caml_ceil_float"
    ; "caml_fmod_float"
    ; "caml_power_float"
    ; "caml_float_of_int"
    ; "caml_int_of_float"
    ; "caml_eq_float"
    ; "caml_neq_float"
    ; "caml_lt_float"
    ; "caml_le_float"
    ; "caml_gt_float"
    ; "caml_ge_float"
    ; "caml_float_compare"
    ; "caml_classify_float"
    ; "caml_int64_add"
    ; "caml_int64_sub"
    ; "caml_int64_mul"
    ; "caml_int64_neg"
    ; "caml_int64_of_int"
    ; "caml_int64_to_int"
    ; "caml_int32_add"
    ; "caml_int32_sub"
    ; "caml_int32_mul"
    ; "caml_int32_of_int"
    ; "caml_int32_to_int"
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
    ; "caml_floatarray_unsafe_set"
    ]

let float_binops =
  [ "caml_add_float", "+"; "caml_sub_float", "-"; "caml_mul_float", "*"; "caml_div_float", "/" ]

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

let primitive out name args =
  let n = List.length args in
  let arg i = List.nth args i in
  let lookup l =
    List.find_map l ~f:(fun (n, op) -> if String.equal n name then Some op else None)
  in
  match lookup float_binops, lookup float_comparisons, lookup float_functions, n with
  | Some op, _, _, 2 ->
      Plain
        (Printf.sprintf "camlc_box_double(Double_val(%s) %s Double_val(%s))" (arg 0) op (arg 1))
  | _, Some op, _, 2 ->
      Plain (Printf.sprintf "Val_bool(Double_val(%s) %s Double_val(%s))" (arg 0) op (arg 1))
  | _, _, Some f, 1 -> Plain (Printf.sprintf "camlc_box_double(%s(Double_val(%s)))" f (arg 0))
  | _ -> (
  match name, n with
  | "caml_neg_float", 1 -> Plain (Printf.sprintf "camlc_box_double(-Double_val(%s))" (arg 0))
  | "caml_float_of_int", 1 ->
      Plain (Printf.sprintf "camlc_box_double((double)Long_val(%s))" (arg 0))
  | "caml_int_of_float", 1 -> Plain (Printf.sprintf "Val_long((intnat)Double_val(%s))" (arg 0))
  | "caml_float_compare", 2 -> Plain (c_call "camlc_float_compare" args)
  | "%identity", 1 -> Plain (arg 0)
  | ("caml_bytes_of_string" | "caml_string_of_bytes"), 1 -> Plain (arg 0)
  | "%int_add", 2 -> Plain (c_call "Camlc_int_add" args)
  | "%int_sub", 2 -> Plain (c_call "Camlc_int_sub" args)
  | ("%int_mul" | "%direct_int_mul"), 2 -> Plain (c_call "Camlc_int_mul" args)
  | "%int_neg", 1 -> Plain (c_call "Camlc_int_neg" args)
  | "%int_and", 2 -> Plain (c_call "Camlc_int_and" args)
  | "%int_or", 2 -> Plain (c_call "Camlc_int_or" args)
  | "%int_xor", 2 -> Plain (c_call "Camlc_int_xor" args)
  | "%int_lsl", 2 -> Plain (c_call "Camlc_int_lsl" args)
  | "%int_lsr", 2 -> Plain (c_call "Camlc_int_lsr" args)
  | "%int_asr", 2 -> Plain (c_call "Camlc_int_asr" args)
  | "%direct_int_div", 2 -> Plain (c_call "Camlc_int_div_unsafe" args)
  | "%direct_int_mod", 2 -> Plain (c_call "Camlc_int_mod_unsafe" args)
  | "%int_div", 2 -> Checked (c_call "camlc_int_div" args)
  | "%int_mod", 2 -> Checked (c_call "camlc_int_mod" args)
  | "caml_int_compare", 2 -> Plain (c_call "Camlc_int_compare" args)
  | "%direct_obj_tag", 1 -> Plain (Printf.sprintf "Val_long(Tag_val(%s))" (arg 0))
  | ("caml_ml_string_length" | "caml_ml_bytes_length"), 1 ->
      Plain (Printf.sprintf "Val_long(caml_string_length(%s))" (arg 0))
  | ("caml_string_unsafe_get" | "caml_bytes_unsafe_get"), 2 ->
      Plain (Printf.sprintf "Val_int(Byte_u(%s, Long_val(%s)))" (arg 0) (arg 1))
  | ("caml_string_unsafe_set" | "caml_bytes_unsafe_set"), 3 ->
      Plain
        (Printf.sprintf
           "(Byte_u(%s, Long_val(%s)) = (unsigned char)Long_val(%s), Val_unit)"
           (arg 0)
           (arg 1)
           (arg 2))
  | ("caml_string_get" | "caml_bytes_get"), 2 -> Checked (c_call "camlc_string_get" args)
  | ("caml_string_set" | "caml_bytes_set"), 3 -> Checked (c_call "camlc_bytes_set" args)
  | "caml_check_bound", 2 -> Checked (c_call "camlc_check_bound" args)
  | "caml_check_bound_float", 2 -> Checked (c_call "camlc_check_bound_float" args)
  | "caml_check_bound_gen", 2 -> Checked (c_call "camlc_check_bound_gen" args)
  | "caml_array_unsafe_get", 2 -> Plain (c_call "camlc_array_unsafe_get" args)
  | "caml_floatarray_unsafe_get", 2 ->
      Plain
        (Printf.sprintf "caml_copy_double(Double_flat_field(%s, Long_val(%s)))" (arg 0) (arg 1))
  | "caml_array_unsafe_set_addr", 3 ->
      Plain
        (Printf.sprintf
           "(camlc_modify(&Field(%s, Long_val(%s)), %s), Val_unit)"
           (arg 0)
           (arg 1)
           (arg 2))
  | "caml_string_concat", 2 -> Plain (c_call "camlc_string_concat" args)
  | ("caml_register_global" | "caml_register_global_predef"), _ -> Plain "Val_unit"
  | "caml_register_global_by_index", 2 ->
      Plain
        (Printf.sprintf
           "(caml_modify(&Field(caml_global_data, Long_val(%s)), %s), Val_unit)"
           (arg 1)
           (arg 0))
  | "caml_get_global_data", 0 -> Plain "caml_global_data"
  | "caml_oo_cache_id", 0 -> Plain "camlc_oo_cache_id()"
  | "caml_get_cached_method", 3 -> Plain (c_call "camlc_get_cached_method" args)
  | ("%perform" | "%reperform" | "%resume" | "%with_stack" | "%with_stack_bind"), _ ->
      Checked "camlc_raise_unhandled_effect()"
  | _ ->
      if String.length name > 0 && Char.equal name.[0] '%'
      then failwith (Printf.sprintf "c_of_ocaml: unsupported primitive %s" name);
      (match String.Hashtbl.find_opt out.primitives name with
      | Some n' when n <> n' ->
          failwith
            (Printf.sprintf "c_of_ocaml: primitive %s called with %d and %d arguments" name n n')
      | _ -> String.Hashtbl.replace out.primitives name n);
      if StringSet.mem name noraise_primitives && n <= 5
      then Plain (c_call name args)
      else if n <= 5
      then Checked (c_call (Printf.sprintf "camlc_ccall%d" n) (name :: args))
      else (* Handled by the caller: the arguments go on the shadow stack *)
        Checked "")

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
  ; static_env : bool  (** The closure is static data *)
  ; locations : location Var.Hashtbl.t
  ; slots : int Var.Hashtbl.t  (** The variables in the frame *)
  ; traps : trap list Addr.Hashtbl.t  (** The handlers in scope in each block *)
  ; handler_ids : int Addr.Hashtbl.t  (** The handler of the [Pushtrap] ending a block *)
  ; buf : Buffer.t
  }

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
  | Some (Sibling d) -> Printf.sprintf "((value)((value *)%s + %d))" (local ctx ctx.env_var) d
  | None -> failwith (Format.asprintf "c_of_ocaml: unbound variable %a" Var.print x)

let slot ctx x =
  match Var.Hashtbl.find_opt ctx.locations x with
  | Some Local -> local ctx x
  | Some (Env _ | Sibling _) | None ->
      failwith (Format.asprintf "c_of_ocaml: variable %a is not local" Var.print x)

let prim_arg ctx a =
  match a with
  | Pv x -> var ctx x
  | Pc c -> code_constant ctx.out c

let line ctx fmt = Printf.bprintf ctx.buf ("  " ^^ fmt ^^ "\n")

let return_stmt ctx e =
  if Var.Hashtbl.length ctx.slots = 0
  then Printf.sprintf "return %s;" e
  else Printf.sprintf "{ value res = %s; camlc_sp = fp + FRAME_SIZE; return res; }" e

(* Jump to the handler of the innermost trap, or return the exception
   result [r] *)
let raise_stmt ctx traps r =
  match traps with
  | [] -> return_stmt ctx r
  | { exn_var; id; _ } :: _ ->
      Printf.sprintf "{ %s = Extract_exception(%s); goto H%d; }" (slot ctx exn_var) r id

let branch ctx (pc, args) =
  let params = (Addr.Map.find pc ctx.blocks).params in
  let moves =
    List.filter_map (List.combine params args) ~f:(fun (p, a) ->
        if Var.equal p a then None else Some (p, a))
  in
  match moves with
  | [] -> Printf.sprintf "goto L%d;" pc
  | [ (p, a) ] -> Printf.sprintf "{ %s = %s; goto L%d; }" (slot ctx p) (var ctx a) pc
  | _ ->
      let temps =
        List.mapi moves ~f:(fun i (_, a) -> Printf.sprintf "value t%d = %s;" i (var ctx a))
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

let closure_layout functions =
  (* Position of the code pointer of each function and start of the
     environment *)
  let n = List.length functions in
  (3 * n) - 1, List.mapi functions ~f:(fun k _ -> 3 * k)

let translate_closure ctx f =
  let info = Var.Map.find f ctx.closures in
  match info.functions with
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
               if pos = 0
               then code
               else Printf.sprintf "Make_header(%d, Infix_tag, 0)" pos :: code))
      in
      if List.is_empty info.free_variables
      then (
        (* A closed closure is static data *)
        let n = ctx.out.data_count in
        ctx.out.data_count <- n + 1;
        let name = Printf.sprintf "camlc_d%d" n in
        Printf.bprintf
          ctx.out.data
          "static value %s[] = { %s };\n"
          name
          (String.concat
             ~sep:", "
             (header ~wosize:(List.length prefix) ~tag:Obj.closure_tag :: prefix));
        List.iter2 functions positions ~f:(fun (g, _) pos ->
            line ctx "%s = (value)&%s[%d];" (slot ctx g) name (pos + 1)))
      else (
        alloc_block
          ctx
          ~dst:(slot ctx f)
          ~tag:Obj.closure_tag
          ~prefix
          (List.map info.free_variables ~f:(fun x -> var ctx x));
        List.iter2 functions positions ~f:(fun (g, _) pos ->
            if pos > 0
            then line ctx "%s = (value)((value *)b + %d);" (slot ctx g) pos))

let apply_arity out n = out.apply_arities <- IntSet.add n out.apply_arities

let translate_instr ctx traps i =
  match i with
  | Let (x, e) -> (
      let dst = slot ctx x in
      let checked call =
        line ctx "r = %s;" call;
        line ctx "if (CAMLC_UNLIKELY(Is_exception_result(r))) %s" (raise_stmt ctx traps "r");
        line ctx "%s = r;" dst
      in
      match e with
      | Apply { f; args; exact } -> (
          let n = List.length args in
          let fe = var ctx f in
          let args = List.map args ~f:(fun x -> var ctx x) in
          let unique =
            if exact
            then
              match Global_flow.get_unique_closure ctx.global_flow_info ~call:x f with
              | Some (g, _, false) when Var.Map.mem g ctx.closures -> Some g
              | Some _ | None -> None
            else None
          in
          match unique with
          | Some g -> checked (c_call (fun_name ctx.out g) (args @ [ fe ]))
          | None ->
              if exact
              then
                checked
                  (Printf.sprintf
                     "((%s)Field(%s, 0))(%s)"
                     (fun_type n)
                     fe
                     (String.concat ~sep:", " (args @ [ fe ])))
              else (
                apply_arity ctx.out n;
                checked (c_call (Printf.sprintf "camlc_apply%d" n) (fe :: args))))
      | Block (tag, a, _, _, _) ->
          if Array.length a = 0
          then line ctx "%s = Atom(%d);" dst tag
          else if tag = Obj.double_array_tag
          then (
            let wosize = Array.length a * 2 in
            if wosize <= 256
            then line ctx "b = camlc_alloc_small(%d, Double_array_tag);" wosize
            else line ctx "b = caml_alloc_shr(%d, Double_array_tag);" wosize;
            Array.iteri a ~f:(fun i x ->
                line ctx "Store_double_flat_field(b, %d, Double_val(%s));" i (var ctx x));
            line ctx "%s = b;" dst)
          else
            alloc_block
              ctx
              ~dst
              ~tag
              ~prefix:[]
              (Array.to_list (Array.map a ~f:(fun x -> var ctx x)))
      | Field (y, i, (Non_float | Immediate)) -> line ctx "%s = Field(%s, %d);" dst (var ctx y) i
      | Field (y, i, Float) ->
          line ctx "%s = caml_copy_double(Double_flat_field(%s, %d));" dst (var ctx y) i
      | Closure _ -> translate_closure ctx x
      | Constant c -> line ctx "%s = %s;" dst (code_constant ctx.out c)
      | Special (Alias_prim name) ->
          failwith (Printf.sprintf "c_of_ocaml: unsupported alias primitive %s" name)
      | Prim (p, args) -> (
          let args = List.map args ~f:(fun a -> prim_arg ctx a) in
          let plain e = line ctx "%s = %s;" dst e in
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
          | Le, [ a; b ] -> plain (Printf.sprintf "Val_bool((intnat)%s <= (intnat)%s)" a b)
          | Ult, [ a; b ] -> plain (Printf.sprintf "Val_bool((uintnat)%s < (uintnat)%s)" a b)
          | Extern (name, _), _ -> (
              let n = List.length args in
              match primitive ctx.out name args with
              | Plain e -> plain e
              | Checked "" ->
                  (* More than 5 arguments: the C function takes an array *)
                  line ctx "argv = camlc_sp - %d;" n;
                  line
                    ctx
                    "if (CAMLC_UNLIKELY(argv < camlc_stack_limit)) %s"
                    (raise_stmt ctx traps "camlc_stack_overflow()");
                  line ctx "camlc_sp = argv;";
                  List.iteri args ~f:(fun i a -> line ctx "argv[%d] = %s;" i a);
                  line ctx "r = camlc_ccallv((camlc_primv)%s, argv, %d);" name n;
                  line ctx "camlc_sp = argv + %d;" n;
                  line
                    ctx
                    "if (CAMLC_UNLIKELY(Is_exception_result(r))) %s"
                    (raise_stmt ctx traps "r");
                  line ctx "%s = r;" dst
              | Checked e -> checked e)
          | (Vectlength _ | Array_get _ | Not | IsInt _ | Eq _ | Neq _ | Lt | Le | Ult), _ ->
              assert false))
  | Assign (x, y) -> line ctx "%s = %s;" (slot ctx x) (var ctx y)
  | Set_field (x, i, (Non_float | Immediate), y) ->
      line ctx "camlc_modify(&Field(%s, %d), %s);" (var ctx x) i (var ctx y)
  | Set_field (x, i, Float, y) ->
      line ctx "Store_double_flat_field(%s, %d, Double_val(%s));" (var ctx x) i (var ctx y)
  | Offset_ref (x, n) ->
      line
        ctx
        "Field(%s, 0) = (value)((uintnat)Field(%s, 0) + %d);"
        (var ctx x)
        (var ctx x)
        (2 * n)
  | Array_set (x, i, y) ->
      line ctx "camlc_modify(&Field(%s, Long_val(%s)), %s);" (var ctx x) (var ctx i) (var ctx y)
  | Event _ -> ()

let translate_last ctx traps last =
  match last with
  | Return x -> line ctx "%s" (return_stmt ctx (var ctx x))
  | Raise (x, _) -> (
      match traps with
      | [] ->
          line
            ctx
            "%s"
            (return_stmt ctx (Printf.sprintf "Make_exception_result(%s)" (var ctx x)))
      | { exn_var; id; _ } :: _ ->
          line ctx "%s = %s; goto H%d;" (slot ctx exn_var) (var ctx x) id)
  | Stop -> line ctx "%s" (return_stmt ctx "Val_unit")
  | Branch cont -> line ctx "%s" (branch ctx cont)
  | Cond (x, c1, c2) ->
      line ctx "if (%s != Val_false) %s" (var ctx x) (branch ctx c1);
      line ctx "else %s" (branch ctx c2)
  | Switch (x, conts) ->
      line ctx "switch (Long_val(%s)) {" (var ctx x);
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
  | Prim (_, args) -> use_prim_args ctx args acc

(* How an instruction may trigger a garbage collection: [`Call] if its
   operands are read before, [`Alloc] if they are read after *)
let gc_point ctx i =
  match i with
  | Let (_, Apply _) -> `Call
  | Let (_, Block (_, [||], _, _, _)) -> `No
  | Let (_, Block _) -> `Alloc
  | Let (x, Closure _) -> (
      match Var.Map.find_opt x ctx.closures with
      | Some { functions = (f0, _) :: _; free_variables = _ :: _; _ } when Var.equal x f0
        -> `Alloc
      | Some _ | None -> `No)
  | Let (_, Field (_, _, Float)) -> `Call
  | Let (_, Prim (Extern (name, _), _)) ->
      if StringSet.mem name nogc_primitives then `No else `Call
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
  use_list ctx args (Var.Set.union acc (List.fold_left params ~init:l ~f:(fun l x -> Var.Set.remove x l)))

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
        not
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
    ~env
    ~static_env
    (pc, args) =
  let ctx =
    { out
    ; facts
    ; global_flow_info
    ; blocks
    ; closures
    ; env_var = Var.fresh ()
    ; static_env
    ; locations = Var.Hashtbl.create 64
    ; slots = Var.Hashtbl.create 16
    ; traps = Addr.Hashtbl.create 16
    ; handler_ids = Addr.Hashtbl.create 16
    ; buf = Buffer.create 4096
    }
  in
  List.iter env ~f:(fun (x, loc) -> Var.Hashtbl.replace ctx.locations x loc);
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
  Var.Set.iter (fun x -> Var.Hashtbl.add ctx.slots x (Var.Hashtbl.length ctx.slots)) rooted;
  let frame_size = Var.Hashtbl.length ctx.slots in
  (* Body *)
  List.iteri params ~f:(fun i x ->
      if Var.Hashtbl.mem ctx.slots x then line ctx "%s = a%d;" (local ctx x) i);
  if Var.Hashtbl.mem ctx.slots ctx.env_var then line ctx "%s = env;" (local ctx ctx.env_var);
  line ctx "%s" (branch ctx (pc, args));
  List.iter pcs ~f:(fun pc ->
      let block = Addr.Map.find pc blocks in
      let traps = Addr.Hashtbl.find ctx.traps pc in
      Printf.bprintf ctx.buf " L%d:;\n" pc;
      List.iter block.body ~f:(fun i -> translate_instr ctx traps i);
      translate_last ctx traps block.branch;
      match block.branch with
      | Pushtrap (_, _, handler) ->
          Printf.bprintf ctx.buf " H%d:;\n" (Addr.Hashtbl.find ctx.handler_ids pc);
          line ctx "%s" (branch ctx handler)
      | Return _ | Raise _ | Stop | Branch _ | Cond _ | Switch _ | Poptrap _ -> ());
  let param_set = Var.Set.of_list params in
  let decls =
    List.filter_map (List.rev !locals) ~f:(fun x ->
        if Var.Hashtbl.mem ctx.slots x || Var.Set.mem x param_set
        then None
        else Some (local_name ctx x))
  in
  let params_c =
    String.concat
      ~sep:", "
      (List.map params ~f:(fun x ->
           if Var.Hashtbl.mem ctx.slots x
           then
             Printf.sprintf
               "value a%d"
               (let rec index i l =
                  match l with
                  | y :: r -> if Var.equal x y then i else index (i + 1) r
                  | [] -> assert false
                in
                index 0 params)
           else "value " ^ local_name ctx x)
      @ if List.is_empty env then [] else [ "value env" ])
  in
  Printf.bprintf out.functions "%s(%s) {\n" name (if String.equal params_c "" then "void" else params_c);
  if not (List.is_empty decls)
  then
    List.iteri decls ~f:(fun i d ->
        if i mod 16 = 0 then Buffer.add_string out.functions (if i = 0 then "  value " else ";\n  value ");
        if i mod 16 <> 0 then Buffer.add_string out.functions ", ";
        Buffer.add_string out.functions d);
  if not (List.is_empty decls) then Buffer.add_string out.functions ";\n";
  Buffer.add_string out.functions "  value r, b, *argv;\n  (void)r; (void)b; (void)argv;\n";
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
          | Let
              ( _
              , Prim (Extern ("caml_register_global_by_index", _), [ _; Pc (Int i) ]) ) ->
              max n (Targetint.to_int_exn i + 1)
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

let f (p : program) ~global_flow_info closures =
  let facts = compute_facts p closures in
  let out =
    { data = Buffer.create 65536
    ; functions = Buffer.create 65536
    ; data_count = 0
    ; constants = Int.Hashtbl.create 1024
    ; primitives = String.Hashtbl.create 128
    ; apply_arities = IntSet.empty
    ; max_arity = 1
    ; global_data_size = global_data_size p
    ; fun_names = Var.Hashtbl.create 1024
    }
  in
  let functions = collect_functions p in
  List.iter functions ~f:(fun (f, params, _) ->
      out.max_arity <- max out.max_arity (List.length params);
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
        @ List.map2 info.functions positions ~f:(fun (g, _) pos -> g, Sibling (pos - k))
      in
      translate_function
        out
        facts
        global_flow_info
        p.blocks
        closures
        ~name:(Printf.sprintf "static value %s" (fun_name out f))
        ~params
        ~env
        ~static_env:(List.is_empty info.free_variables)
        cont);
  translate_function
    out
    facts
    global_flow_info
    p.blocks
    closures
    ~name:"value camlc_program"
    ~params:[]
    ~env:[]
    ~static_env:true
    (p.start, []);
  (* Assemble the output *)
  let b = Buffer.create (Buffer.length out.data + Buffer.length out.functions + 65536) in
  Buffer.add_string b "/* Generated by c_of_ocaml */\n#include \"camlc.h\"\n\n";
  Printf.bprintf
    b
    "_Static_assert(sizeof(value) == %d, \"c_of_ocaml: wrong word size\");\n\n"
    word_size;
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
  List.iter functions ~f:(fun (f, params, _) ->
      Printf.bprintf
        b
        "static value %s(%s);\n"
        (fun_name out f)
        (fun_params (List.length params)));
  for m = 1 to out.max_arity - 1 do
    Printf.bprintf b "static value camlc_pap%d(%s);\n" m (fun_params m)
  done;
  Buffer.add_char b '\n';
  Printf.bprintf b "const int camlc_global_data_size = %d;\n" out.global_data_size;
  Printf.bprintf b "const int camlc_max_arity = %d;\n" out.max_arity;
  Printf.bprintf
    b
    "const value camlc_pap_code[] = { 0%s };\n\n"
    (String.concat
       ~sep:""
       (List.init ~len:(out.max_arity - 1) ~f:(fun i ->
            Printf.sprintf ", (value)&camlc_pap%d" (i + 1))));
  Buffer.add_buffer b out.data;
  Buffer.add_char b '\n';
  (* Partial applications: the closure holds the function and the
     arguments already given *)
  for m = 1 to out.max_arity - 1 do
    Printf.bprintf
      b
      "static value camlc_pap%d(%s) {\n\
      \  mlsize_t k = Wosize_val(env) - 3;\n\
      \  value *argv = camlc_sp - (k + %d);\n\
      \  value r;\n\
      \  if (CAMLC_UNLIKELY(argv < camlc_stack_limit)) return camlc_stack_overflow();\n\
      \  camlc_sp = argv;\n\
      \  for (mlsize_t i = 0; i < k; i++) argv[i] = Field(env, 3 + i);\n\
       %s\
      \  r = camlc_applyv(Field(env, 2), k + %d, argv);\n\
      \  camlc_sp = argv + k + %d;\n\
      \  return r;\n\
       }\n\n"
      m
      (fun_params m)
      m
      (String.concat
         ~sep:""
         (List.init ~len:m ~f:(fun i -> Printf.sprintf "  argv[k + %d] = a%d;\n" i i)))
      m
      m
  done;
  (* Exact calls with the arguments in an array *)
  Printf.bprintf b "value camlc_call_exact(value f, int n, value *args) {\n  switch (n) {\n";
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
  IntSet.iter
    (fun n ->
      let args = List.init ~len:n ~f:(fun i -> Printf.sprintf "a%d" i) in
      Printf.bprintf
        b
        "static inline value camlc_apply%d(value f, %s) {\n\
        \  if (Arity_closinfo(Closinfo_val(f)) == %d)\n\
        \    return ((%s)Field(f, 0))(%s, f);\n\
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
