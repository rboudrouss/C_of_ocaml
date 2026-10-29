# c_of_ocaml

c_of_ocaml compiles an OCaml bytecode executable to C, to be compiled to
WebAssembly by emscripten and linked with the OCaml runtime and the C stubs
of the program, as built by [ocaml-emcc](https://github.com/rboudrouss/ocaml-emcc).
The program is then compiled rather than interpreted by an `ocamlrun`
compiled to wasm.

It reuses the front-end and the optimizer of js_of_ocaml: the bytecode is
parsed into the intermediate code (`compiler/lib/code.mli`), optimized by
`Driver.optimize_for_wasm`, and translated into C by `compiler/lib-c`.

## Usage

```sh
ocamlfind ocamlc -package unix -linkpkg main.ml -o main.bc
c_of_ocaml main.bc -o main.c          # --opt 2 or 3 for more optimizations,
                                      # --debug-info for backtraces (ocamlc -g)

SYSROOT=$OPAM_SWITCH_PREFIX/emcc-sysroot/wasm32   # built by ocaml-emcc
CFLAGS="-O2 -mtail-call -fno-strict-aliasing -fwrapv -fno-strict-overflow \
  -sSUPPORT_LONGJMP=wasm -fwasm-exceptions \
  -I runtime/c -I $SYSROOT/lib/ocaml"
emcc $CFLAGS -c runtime/c/camlc_runtime.c
emcc $CFLAGS -c main.c
emcc $CFLAGS main.o camlc_runtime.o $SYSROOT/lib/libcamlrun.a -lm \
  -sALLOW_MEMORY_GROWTH=1 -sEXIT_RUNTIME=1 -o main.js
node main.js
```

`libcamlrun.a` is linked without its interpreter: `camlc_runtime.c` defines
`main` and replaces the runtime's `callback.c`. `c_of_ocaml --runtime-files
DIR` writes `camlc.h` and `camlc_runtime.c` to `DIR`.

With ocaml-emcc, `ocaml-emcc build main.js --compiled` does all this.

## Design

See `runtime/c/camlc.h` for the conventions of the generated code.

- **Values** have the representation of the bytecode runtime, with 31-bit
  integers (wasm32). Constants are static data, with headers that the GC
  never marks, in a single structure (registered as static data in the
  page table of OCaml 4.14).
- **Runtimes**: OCaml 4.14 and 5.x (`camlc_runtime.c` follows the startup
  and callbacks of each).
- **Functions**: each function of the program is a C function taking its
  arguments then its closure. Closures have the layout of the bytecode
  runtime (with `Infix_tag` blocks for mutually recursive functions), with C
  function pointers as code pointers, so that `compare`, `hash` and `Obj`
  work. Calls to statically known functions are direct C calls; other
  exact calls go through the code pointer, and the others through
  `camlc_applyN`, which handles partial and over-applications. With the
  tail call extension of WebAssembly (`-mtail-call`), an application whose
  result is returned is a tail call (`musttail`) when the callee takes as
  many arguments as the caller.
- **GC roots**: the variables live across a point where the GC may run (an
  allocation or a call), unless they hold integers or static data, are kept
  in the frame of the function on a shadow stack, scanned by the GC through
  `caml_scan_roots_hook`. The other variables are C locals.
- **Exceptions** are returned: raising returns
  `Make_exception_result(exn)`, and each call is followed by a check which
  jumps to the innermost handler of the function or returns the exception.
  Nothing calls `setjmp` in the generated code (emscripten would wrap each
  call of such a function in an exception handler). C primitives, which
  raise with `longjmp`, are called through wrappers which return the
  exception, except the ones known not to raise.
- **Unboxed numbers**: the type analysis of wasm_of_ocaml
  (`compiler/lib-c/typing.ml`) finds the floats and boxed integers that are
  never needed boxed: they are C `double`s and integers, and the arithmetic
  on them is inlined. The parameters of the functions only called directly
  can be unboxed too.
- **Callbacks** from C (`caml_callback*`) apply the closure with
  `camlc_applyv`.
- **Backtraces** (`--debug-info`): a raise, and each call an exception goes
  through, record a static description of their location (from the debug
  events) in the backtrace buffer of the runtime, only when backtraces are
  active. `camlc_runtime.c` replaces the runtime's `backtrace_byt.c`. The
  call stack itself cannot be walked: `Printexc.get_callstack` is empty.

## C stubs

The generated code calls the primitives directly, and declares them with
the arity of their call sites. A stub defined with another signature
(`ml_z_init(void)` called with one argument) would trap when called, so
the generated file contains a function, never called, that makes wasm-ld
report these mismatches (`function signature mismatch: NAME`, as for the
primitive table of ocaml-emcc). Such a primitive must then go through an
adapter: compile the generated file with `-DNAME=ADAPTER`, `ADAPTER` taking
the arguments of the call site and calling the stub with its own signature.

## Testing

`tools/c_of_ocaml_tests.sh` runs the tests of `compiler/tests-ocaml` (see
the script for its environment variables). All the tests pass, except:

- effect handlers and domains, which are not supported;
- the tests that need arguments or files (`basic-io`, `basic-io-2`,
  `lib-channels/close_out`), and those which expect the behavior of the
  JavaScript runtime (`lib-digest/blake2_long_key`, `lib-obj/new_obj`,
  `lib-bytes/test_bytes`);
- `misc/gcwords`, `misc/sieve` and `misc/pr7168` (deep recursion), which are
  also disabled for js_of_ocaml and wasm_of_ocaml;
- `runtime-objects/Tests`, which marshals closures.

## Status

Not supported yet:

- effect handlers (`--effects=disabled` only) and domains;
- wasm64 (63-bit integers): `Targetint` has at most 32 bits;
- marshaling closures (`Marshal.Closures`);
- separate compilation: whole programs only;
- deep recursion: the C stack of the wasm engine (about 1 MB in V8)
  limits the depth of non-tail calls to some tens of thousands, and its
  exhaustion is a `RangeError` rather than `Stack_overflow`; the
  bytecode interpreter has its own, much larger, stack (under node,
  ocaml-emcc runs compiled programs in a worker thread with a large
  stack);
- signals and finalisers only run when the runtime polls by itself.

## Next steps

- **Effect handlers.** The way to go is the double translation of
  js_of_ocaml (`--effects=double-translation`, `compiler/lib/effects.ml`):
  the functions that may perform an effect get a CPS version, the others
  keep their direct style, so programs without effects do not pay for
  them. It needs, in the C backend, closures with the code pointers of
  both versions (as `Gc_target.double_translation` in lib-wasm), and in
  the runtime support the CPS primitives (`%perform`, `%resume`,
  `%reperform`, `%with_stack`, `caml_push_trap`/`caml_pop_trap`, the
  trampolines), as `runtime/wasm/effect.wat` does for wasm. CPS calls are
  tail calls of various signatures, which `musttail` cannot always make:
  they need a trampoline, or a uniform signature.
- **wasm64.** `Targetint` has at most 32 bits, and nativeint constants are
  32-bit in the intermediate code (`Code.constant`): both need 64-bit
  variants. The C backend itself only has to know the word size
  (`word_size`, tagged constants, string padding).
- Shrink-wrapping: allocate the frame on the shadow stack only in the
  branches that need it.

