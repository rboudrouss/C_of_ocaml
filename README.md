# c_of_ocaml

c_of_ocaml compiles OCaml bytecode executables to C. The C code is then
compiled to WebAssembly with emscripten and linked against the OCaml runtime
and the program's C stubs, as built by
[ocaml-emcc](https://github.com/rboudrouss/ocaml-emcc). The program runs as
native wasm code instead of being interpreted by an `ocamlrun` compiled to
wasm.

This repository is a fork of [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml).
c_of_ocaml reuses its front-end and optimizer: the bytecode is parsed into the
intermediate representation (`compiler/lib/code.mli`), optimized by
`Driver.optimize_for_wasm`, and then translated to C by `compiler/lib-c`.

## Usage

### With ocaml-emcc

```sh
ocaml-emcc build main.js --compiled
```

This runs every step below.

### By hand

Any bytecode executable works as input, whether built with `ocamlc`,
ocamlfind or dune.

```sh
ocamlc main.ml -o main.bc
c_of_ocaml main.bc -o main.c

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

`libcamlrun.a` is linked without its interpreter. `camlc_runtime.c` defines
`main` and replaces the runtime's `callback.c`, `backtrace_byt.c`, `meta.c`
and `dynlink.c`. `c_of_ocaml --runtime-files DIR` writes `camlc.h` and
`camlc_runtime.c` to `DIR`, for use outside this tree.

### Options

| Option | Effect |
| --- | --- |
| `-o FILE` | Write the C code to `FILE`. |
| `--split DIR` | Write the C code to several files (see below). |
| `--part-size KB` | With `--split`, the size of each part (default 2048). |
| `--opt {1,2,3}` | Optimization profile (default 1). |
| `--debug-info` | Record backtraces. The bytecode must be built with `ocamlc -g`. |
| `--word-size {32,64}` | Target wasm32 (31-bit integers, default) or wasm64 (63-bit integers). |
| `--effects {disabled,double-translation}` | Effect handler support. Defaults to double translation when the program performs effects. |
| `--runtime-files DIR` | Write the C runtime support files to `DIR`. |

### wasm64

Pass `--word-size 64` to c_of_ocaml, use the wasm64 sysroot, and add `-m64`
to the emcc flags (plus `-sMAXIMUM_MEMORY=16GB` when linking). Node 24 or later
is needed to run the result.

### Large programs

clang's memory use and compile time grow with the size of a translation unit,
so large programs should be split: `c_of_ocaml main.bc --split DIR` writes
`DIR/part0.c` (static data and tables) and `DIR/part1.c`, `DIR/part2.c`, ...
(the functions), which are compiled separately.

The generated C names do not depend on the rest of the program: functions are
named after their source location, constants after their contents. After an
edit, only `part0.c` and the parts whose code changed differ, and unchanged
files are not rewritten, so a build system only recompiles what changed.

### C stubs

The generated code calls primitives directly and declares them with the arity
of their call sites. A stub defined with a different signature (say
`ml_z_init(void)`, called with one argument) would trap at run time. To catch
this at link time, the generated file contains an uncalled function that makes
wasm-ld report each mismatch as `function signature mismatch: NAME`.

Such a primitive needs an adapter: compile the generated file with
`-DNAME=ADAPTER`, where `ADAPTER` takes the arguments of the call site and
calls the stub with its real signature.

## Design

The conventions of the generated code are documented in `runtime/c/camlc.h`.
Both OCaml 4.14 and 5.x runtimes are supported, and `camlc_runtime.c` follows the
startup and callback code of each.

**Values** use the representation of the bytecode runtime. Constants are
static data in their own section, `camlc_static`, with headers the GC never
marks (on 4.14 the section is registered as static data in the page table).
Toplevel values used by functions are C globals in the section
`camlc_globals`, which the GC scans.

**Functions.** Each OCaml function becomes a C function taking its arguments
followed by its closure. Closures keep the bytecode layout (including
`Infix_tag` blocks for mutually recursive functions), with C function
pointers as code pointers, so `compare`, `hash` and `Obj` keep working.

- Calls to statically known functions are direct C calls.
- Other calls with the right number of arguments go through the code pointer.
- All remaining calls go through `camlc_applyN`, which handles partial and
  over-application.

With the WebAssembly tail call extension (`-mtail-call`), a call in tail
position is emitted with `musttail` when the callee takes as many arguments as
the caller.

**GC roots.** Variables that are live across a point where the GC may run (an
allocation or a call) are stored in the function's frame on a shadow stack,
which the GC scans through `caml_scan_roots_hook`. Variables holding integers
or static data are exempt. Everything else is a plain C local.

**Exceptions** are return values. Raising returns
`Make_exception_result(exn)`, and every call is followed by a check that jumps
to the innermost handler or propagates the exception to the caller. The
generated code never calls `setjmp`, because emscripten would wrap every call
in such a function in an exception handler.

C primitives raise with `longjmp`, so they are called through wrappers that
catch the exception with a `sigsetjmp` and return it, except for primitives
known not to raise. A few frequent primitives avoid the wrapper:
`camlc_runtime.c` has versions of them that return their exceptions, or a fast
path that cannot raise before falling back to the wrapper. This covers
polymorphic comparison (on integers, strings, floats and small blocks of
those), `int_of_string` and `Bytes.create`. Other primitives are inlined in
the generated code (`camlc.h`): the 16, 32 and 64-bit accesses to strings and
bytes (`Bytes.get_int32_le`...), byte swaps, string lengths, and bigarray
accesses, directly on the elements when the type analysis knows the kind and
layout of the bigarray. One `setjmp` per `try` instead of
one per primitive call was tried: each `try` then costs about as much as a
wrapper, and a loop around `try ... with Not_found` became 70% slower.

**Unboxed numbers.** The type analysis of wasm_of_ocaml
(`compiler/lib-c/typing.ml`) finds floats and boxed integers that never need
to be boxed. They become C `double`s and integers, and their arithmetic is
inlined. Parameters of functions that are only called directly can be unboxed
as well. A number used as a value is boxed once, where it is defined, unless
it is only passed to calls outside of loops, and either defined in a loop (the
accumulator of a loop printed at its end) or passed to a single call: it is
then boxed at these calls.
Boxed integers are allocated inline, like floats.

**Effect handlers** (OCaml 5) use the double translation of js_of_ocaml
(`compiler/lib/effects.ml`). Functions that may perform an effect get a second,
CPS version, whose code pointer follows the closure info in the closure. The
rest of the program is unchanged. Running a computation under a handler
(`caml_resume` in `camlc_runtime.c`, modelled on the CPS runtime of
wasm_of_ocaml) switches to the CPS versions. Fibers are OCaml blocks. CPS
calls are tail calls when `musttail` allows. Otherwise they track the C stack
depth and bounce through a trampoline past a threshold.

**Callbacks** from C (`caml_callback*`) apply the closure with
`camlc_applyv`.

**Backtraces** (`--debug-info`). When backtraces are active, a raise and
every call an exception passes through record a static description of their
location, taken from the debug events, in the runtime's backtrace buffer. The
call stack itself cannot be walked, so `Printexc.get_callstack` returns an
empty stack.

## Limitations

- No domains.
- Bytecode loading and shared libraries (`Dynlink`) are not supported.
- Whole programs only: there is no separate compilation.
- `Marshal.Closures` is not supported.
- Effects cannot be performed across a callback from C: they raise
  `Effect.Unhandled`, as with js_of_ocaml. `Effect.Deep.get_callstack` is
  empty.
- Recursion depth is limited by the wasm engine's C stack (about 1 MB in V8),
  i.e. some tens of thousands of non-tail calls, whereas the bytecode
  interpreter has its own much larger stack. Exhausting it raises a JavaScript
  `RangeError` rather than `Stack_overflow`. Under node, ocaml-emcc runs
  compiled programs in a worker thread with a large stack to mitigate this.
- Signals and finalisers only run when the runtime polls on its own.

## Testing

`tools/c_of_ocaml_tests.sh` runs the tests of `compiler/tests-ocaml` through
c_of_ocaml, emcc and node. Its header documents the environment variables
(`WORD_SIZE=64` for wasm64). `make tests-c` builds c_of_ocaml and runs it for
wasm32.

All tests pass on both wasm32 and wasm64, except:

- tests that use domains
- tests that need arguments or files (`basic-io`, `basic-io-2`,
  `lib-channels/close_out`)
- tests that expect the behaviour of the JavaScript runtime
  (`lib-digest/blake2_long_key`, `lib-obj/new_obj`, `lib-bytes/test_bytes`)
- deep recursion tests (`misc/gcwords`, `misc/sieve`, `misc/pr7168`), also
  disabled for js_of_ocaml and wasm_of_ocaml
- `runtime-objects/Tests`, which marshals closures.

## Source layout

- `compiler/bin-c_of_ocaml/`: the `c_of_ocaml` command.
- `compiler/lib-c/`: the C backend (closure conversion, call graph
  analyses, unboxing, and code generation).
- `runtime/c/`: `camlc.h` and `camlc_runtime.c`, the runtime support linked
  with the generated code.
- `tools/c_of_ocaml_tests.sh`: the test driver.

Everything else comes from js_of_ocaml.

## Next steps

- Shrink-wrapping: allocate the shadow stack frame only on the paths that need
  it.
