# The C backend of c_of_ocaml

The library `c_of_ocaml_compiler`: the translation of the intermediate code
of js_of_ocaml (after its optimizations, see `compiler/lib/driver.ml`) into C,
for the runtime of `runtime/c`. The subdirectories are only for reading: the
modules form a single library (`include_subdirs unqualified`).

- `generate.ml`: the entry point (`Generate.f`), from the program to the C
  files: the globals, the static data, the analyses, the functions, then the
  output split into parts.
- `passes/`: the transformations of the code before its translation.
  - `closure_conversion.ml`: the free variables of the closures.
  - `globalize.ml`: the toplevel values held in C globals.
- `analysis/`: analyses of the whole program.
  - `typing.ml`: the numbers which can stay unboxed.
  - `call_graph_analysis.ml`: the known calls of each function.
- `codegen/`: the translation into C, each module using the previous ones.
  - `c_repr.ml`: the representations of the values in C (boxed or not).
  - `c_output.ml`: the output, the static data, the constants, their names.
  - `c_primitives.ml`: the primitives, their C, whether they raise or
    trigger a GC, their types.
  - `c_facts.ml`: program-wide facts about variables.
  - `c_translate.ml`: the translation of the instructions of a function
    (closures, applications, tail calls).
  - `c_frame.ml`: liveness, the points where the GC may run, the polls, the
    frame on the shadow stack and its slots.
  - `c_summary.ml`: the summaries of the functions (whether they raise or
    trigger a GC, their recursive calls).
  - `c_function.ml`: the translation of a function (frame, segments of the
    stack, body).
  - `c_naming.ml`: the tables of the program and the stable C names of its
    globals and functions.
  - `c_toplevel.ml`: the toplevel code split into functions.
  - `c_tables.ml`: the tables of the runtime and the application functions
    of the code (`camlc_applyN`, partial applications).

The passes of `compiler/lib` used for C only (`tuple_args.ml`,
`apply_fusion.ml`, `loop_reentry.ml`) run before, in `Driver.optimize`.
