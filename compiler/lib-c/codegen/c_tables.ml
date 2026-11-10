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

(* The static data and the tables of the runtime, and the application
   functions of the code (camlc_applyN, the partial applications) *)

open! Stdlib
open Code
open! C_repr
open! C_output
open! C_primitives
open! C_facts
open! C_translate
open! C_frame
open! C_summary
open! C_function
open! C_naming
open! C_toplevel

(* The maximal arity of the functions whose partial applications call them
   directly, see [camlc_curry_code] *)
let curry_max = 8

(* The maximal number of arguments of the partial applications allocated
   by [camlc_partial<n>] (small blocks) *)
let partial_max = 64

(* The static data and the tables of the runtime *)
let tables
    ~out
    ~mode
    ~dt
    ~globals
    ~global_list
    ~definitions
    ~interfaces
    ~units
    ~p
    ~functions_max_arity
    b =
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
        "__attribute__((used)) static void (*const camlc_plugin_c_functions[])(void) = { \
         %s };\n"
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

(* The application functions, in each part which may use them *)
let helpers ~out ~mode ~dt b =
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
        \  r = Camlc_resolve(((%s)Field(f, 0))(%s, f));\n\
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
             "  Field(r, 2) = Field(fp[0], 2) != CAMLC_NO_CPS ? camlc_pap_cps_code[arity \
              - %d] : CAMLC_NO_CPS;\n"
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
