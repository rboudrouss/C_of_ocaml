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

(* The output of the translation: the C definitions, the static data and
   their names, the constants *)

open! Stdlib
open Code
open! C_repr

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
