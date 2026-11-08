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

## Building

```sh
dune build --profile release compiler/bin-c_of_ocaml/c_of_ocaml.exe
```

It needs the dependencies of `js_of_ocaml-compiler.opam`, including
`ppx_devtools-optcomp` from its `pin-depends`. OCaml 4.14 or later is needed.

## Usage

### With ocaml-emcc

```sh
ocaml-emcc build main.js
```

ocaml-emcc finds `c_of_ocaml` in `PATH` (or `$OCAML_EMCC_C_OF_OCAML`).

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

### The host

c_of_ocaml translates bytecode to C, and its runtime support only depends on
the OCaml runtime and on wasm, not on emscripten. What depends on the
platform is left to the host which compiles and links the C code
(ocaml-emcc):

- The host may start the program from its own `main`: `camlc_main(argv)`
  starts the runtime and runs the program, and `camlc_runtime.c` compiled
  with `-DCAMLC_NO_MAIN` has no `main`.
- The host may run functions on new stacks of the engine, for the segmented
  stack (see below), by defining `camlc_host_new_stacks` and
  `camlc_host_on_new_stack` (`camlc.h`; `camlc_runtime.c` has weak definitions
  which do not). `compiler/tests-c/segments/jspi_host.c` does it with
  emscripten's JSPI.
- The settings of the OCaml runtime are the host's: the default number of
  domains, for instance, which OCaml 5.3 sets to 128 (ocaml-emcc's runtime
  defaults to 1 without threads).
- The manifest (see below) gives what the host needs to know about the C code.

### Options

| Option | Effect |
| --- | --- |
| `-o FILE` | Write the C code to `FILE`. |
| `--split DIR` | Write the C code to several files (see below). |
| `--part-size KB` | With `--split`, the size of each part (default 2048). |
| `--manifest FILE` | Write the manifest of the C code to `FILE` (with `--split`, always written to `DIR/manifest`). |
| `--toplevel-chunk-size N` | Split the toplevel code into functions of about `N` instructions of the intermediate code (default 2000, 0: a single function). |
| `--opt {1,2,3}` | Optimization profile (default 1). |
| `--debug-info`, `--no-debug-info` | Record backtraces, or not. By default, they are recorded when the bytecode has debug events (`ocamlc -g`). |
| `--word-size {32,64}` | Target wasm32 (31-bit integers, default) or wasm64 (63-bit integers). |
| `--effects {disabled,double-translation}` | Effect handler support. Defaults to double translation when the program performs effects out of its dead code (with `--dynlink` too, not counting those its plugins perform through its functions) or with `--cps-stack`. Disabled for plugins, unless `--cps-stack`: they need the option of the program. |
| `--cps-stack` | Deep recursions go on in CPS, on the heap (see below). |
| `--dynlink` | Compile a program which can load plugins (see below). |
| `--runtime-files DIR` | Write the C runtime support files to `DIR`. |
| `--disable small-functions` | No small functions for V8's inlining budget (see Design). |

### wasm64

Pass `--word-size 64` to c_of_ocaml, use the wasm64 sysroot, and add `-m64`
to the emcc flags (plus `-sMAXIMUM_MEMORY=16GB` when linking). Node 24 or later
is needed to run the result.

### Large programs

clang's memory use and compile time grow with the size of a translation unit,
so large programs should be split: `c_of_ocaml main.bc --split DIR` writes
`DIR/part0.c` (static data and tables) and `DIR/part1.c`, `DIR/part2.c`, ...
(the functions), which are compiled separately.

The time clang takes to optimize a function grows faster than its size, so
the toplevel code, which initializes all the modules, is split into functions
of about 2000 instructions of the intermediate code (`--toplevel-chunk-size`),
which the toplevel function `camlc_program` calls in turn. A chunk starts at a
point where no local variable is live (the values which live long are C
globals), outside any `try` and loop, through which the rest of the program
goes.

The generated C names do not depend on the rest of the program: functions are
named after their source location, constants after their contents, and the
chunks of the toplevel code start at points chosen by a hash of their names. After an
edit, only `part0.c` and the parts whose code changed differ, and unchanged
files are not rewritten, so a build system only recompiles what changed.

### Manifest

With `--split DIR` (or `--manifest FILE`), c_of_ocaml writes the facts about
the C code its host needs to compile and link it, so that the host never reads
the C code. One `KEY VALUE` per line:

```
format 1
word_size 32
mode program
effects disabled
uses_effects false
debug_info false
primitive caml_array_make 2
primitive caml_ml_open_descriptor_in 1
...
```

`mode` is `program`, `dynlink` or `plugin`. `effects` is `disabled` or
`double-translation`: the plugins of a `--dynlink` program must be compiled
with the same. `uses_effects` is `true` when the code performs effects
(`%perform`, `%resume`...) out of its dead code, whatever `effects`: for a
`--dynlink` program, its compilation units are not counted as used by its
plugins; for a plugin, they are, by the plugins loaded after it. A host can
refuse a plugin which performs effects for a program compiled without them.
`primitive NAME ARITY` lists the C primitives the code calls,
with the number of arguments of their calls, or `-1` for those called with an
array of arguments and its length (`value *argv, int argc`, more than 5
arguments). A host uses them to adapt stubs whose C signature differs, or to
provide the missing ones. More keys may be added; unknown ones should be
ignored.

### C stubs

The generated code calls primitives directly and declares them with the arity
of their call sites. A stub defined with a different signature (say
`ml_z_init(void)`, called with one argument) would trap at run time. To catch
this at link time, the generated file contains an uncalled function that makes
wasm-ld report each mismatch as `function signature mismatch: NAME`.

Such a primitive needs an adapter: compile the generated file with
`-DNAME=ADAPTER`, where `ADAPTER` takes the arguments of the call site and
calls the stub with its real signature.

### Plugins

A program compiled with `--dynlink` can load plugins compiled ahead of time,
as emscripten side modules. The program is linked with `-linkall`, so that
its units are all there, and with `-sMAIN_MODULE=2`, exporting the symbols
the plugins use (the primitives, and the functions and data of
`camlc_runtime.c` and of the OCaml runtime):

```sh
ocamlc -linkall shared.cmo main.ml -o main.bc
c_of_ocaml --dynlink main.bc -o main.c
emcc $CFLAGS main.o camlc_runtime.o ... -sMAIN_MODULE=2 -sEXPORTED_FUNCTIONS=...
```

A `.cmo` or `.cma` file given to c_of_ocaml is compiled to a plugin, with the
same `--word-size` and `--effects` options as the program:

```sh
c_of_ocaml plugin.cma -o plugin.c          # or --split DIR
emcc $CFLAGS -fPIC -c plugin.c
emcc $CFLAGS -sSIDE_MODULE=1 plugin.o -o plugin.so
```

The program registers its compilation units by name, and keeps them whole
(a plugin may use any of their fields). A plugin refers to the units of the
program and of the plugins loaded before it by name. Its symbols are hidden,
except its entry point, `value camlc_plugin_init(void)`, which the loader
finds with `dlsym` and calls (as a primitive would: it raises the exceptions
with `caml_raise`). It checks that the units the plugin needs are loaded and
the ones it defines are not, and that the interfaces it was compiled against
are those of the loaded units (their CRCs), as `Dynlink` does, and that the
program and the plugin are compiled with the same effects option, raising
`Failure` otherwise. Then it initializes the units of the plugin, letting their
exceptions through. The API is documented in `runtime/c/camlc.h`
(`camlc_load_plugin`, `camlc_find_unit`). The functions of a plugin have at
most 64 arguments.

## Design

The conventions of the generated code are documented in `runtime/c/camlc.h`.
Both OCaml 4.14 and 5.x runtimes are supported, and `camlc_runtime.c` follows the
startup and callback code of each.

**Values** use the representation of the bytecode runtime. Constants are
static data, with headers the GC never marks (on 4.14 they are registered as
static data in the page table). Toplevel values used by functions are C
globals, which the GC scans. The generated code lists both in a table,
`camlc_program_data` (sections with bounds defined by the linker would not
work with the dynamic linking of emscripten).

**Functions.** Each OCaml function becomes a C function taking its arguments
followed by its closure. Closures keep the bytecode layout (including
`Infix_tag` blocks for mutually recursive functions), with C function
pointers as code pointers, so `compare`, `hash` and `Obj` keep working.

- Calls to statically known functions are direct C calls.
- Other calls with the right number of arguments go through the code pointer.
- All remaining calls go through `camlc_applyN`, which handles partial and
  over-application. A partial application allocates a closure whose code
  calls the function directly once given the arguments left (generated for
  the functions of arity up to 8, like the `caml_curry` functions of
  ocamlopt).

With the WebAssembly tail call extension (`-mtail-call`), a call in tail
position is emitted with `musttail` when the callee has the signature of the
caller. Otherwise, when the call may be recursive (an unknown function, a
function of the strongly connected component of the caller in the graph of the
known calls, or a function which may make an unknown tail call, by itself or
through known tail calls: a loop of tail calls through closures, as the
continuations of code in CPS), it goes through a function of the signature of the caller
(`camlc_t_...`), called with `musttail`, which finds the callee and its
arguments in `camlc_seg` and calls it in tail position: clang would
otherwise make it a plain call, as soon as the caller has another return (an
exception), and mutually recursive functions of different signatures would
grow the stack of the engine. The other calls end, and clang may inline
them. An application of a function to more arguments than its arity
applies it, then its results, to the first ones (`camlc_apply_prefix`), and
makes the last application in tail position (`camlc_lastN_K`), as OCaml
does.

**GC roots.** Variables that are live across a point where the GC may run (an
allocation or a call) are stored in the function's frame on a shadow stack,
which the GC scans through `caml_scan_roots_hook`. Variables holding integers
or static data are exempt. All variables are C locals: those in the frame are
also written to it where they are defined, and reloaded from it after each
point where the GC may run, so that loops without such points do not read
the frame.

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
as well, and so can their results if they cannot raise (exceptions are
returned as values). A number used as a value is boxed once, where it is defined, unless
it is only passed to calls outside of loops, and either defined in a loop (the
accumulator of a loop printed at its end) or passed to a single call: it is
then boxed at these calls.
Boxed integers are allocated inline, like floats.

**Effect handlers** (OCaml 5) use the double translation of js_of_ocaml
(`compiler/lib/effects.ml`). Functions that may perform an effect get a second,
CPS version, whose code pointer follows the closure info in the closure
(`camlc_no_cps` for the other functions). The
rest of the program is unchanged. Running a computation under a handler
(`caml_resume` in `camlc_runtime.c`, modelled on the CPS runtime of
wasm_of_ocaml) switches to the CPS versions. Fibers are OCaml blocks. CPS
calls are tail calls when `musttail` allows. Otherwise they track the C stack
depth and bounce through a trampoline past a threshold.

**Callbacks** from C (`caml_callback*`) apply the closure with
`camlc_applyv`.

**Segmented stack.** The generated functions run on the stack of the wasm
engine, which browsers keep small (984 KB in V8, 500 KB in Chrome's workers):
tens of thousands of nested calls, where the bytecode interpreter has a stack
of its own. When the host can run a function on a new stack of the engine
(`camlc_host_on_new_stack`; with emscripten's JSPI, `-sJSPI`, JavaScript
Promise Integration, the program runs in a context which can suspend),
`camlc_runtime.c` uses it to chain stacks: the shadow stack measures the depth,
and a function whose frame would go below `camlc_segment_limit`, about 2048
words below the start of the current segment, calls itself again on a new
stack of the engine (`camlc_segment`): the current one is suspended until it
returns. Its arguments go through `camlc_seg`, and a thunk per signature
(`camlc_s_...`) calls it. A frame then stands for the frame of the function on
the engine's stack: a function which calls functions gets at least a word for
100 bytes of it (about 80 bytes, and 2 per C variable), and a function without
frame gets one if it makes one call which may be recursive (the strongly
connected components of the known calls, and the unknown calls) on a path.
Functions making several (`fib`) are not measured: the depth of their
recursion is that of a tree. A function restarts only from its start, or from
where it allocates its frame if what it did before has no effect; otherwise it
goes on on the current stack. There is no new segment in a callback from C,
where JavaScript may be on the stack, which JSPI cannot suspend, nor in the
threads other than the one which starts the program (pthreads, in workers
which do not run in such a context). Without JSPI
(node runs compiled programs in a worker thread with a large stack), the limit
is that of the shadow stack. The C stack of emscripten (`__stack_pointer`, in
the linear memory, `-sSTACK_SIZE`) does not change with the stack of the
engine. The generated functions take the address of none of their variables,
and make no variadic call, so they have no frame there: the slow paths of the
applications put the function and its arguments on the shadow stack
(`camlc_applyN_slow`, `camlc_apply_frame`). A recursion makes it grow only
through C functions (callbacks): a callback raises `Stack_overflow` when
less than an eighth of it (at most 64 KB) is left, for the C functions it may
call. Give it 1 MB at least (ocaml-emcc gives 8 MB). An overflow of it by C
functions alone is silent unless the program is linked with
`-sSTACK_OVERFLOW_CHECK`.

**CPS stack** (`c_of_ocaml --cps-stack`). Without JSPI, deep recursions can go
on in continuation-passing style, on the heap. The functions which may recurse
(in a cycle of known calls, or making unknown calls) get a CPS version, as with
effect handlers (the double translation, which this option turns on), and so do
their calls which may be recursive (`recursive_functions` in
`compiler/lib/partial_cps_analysis.ml`), but not their callers. Past the
segment limit, such a function calls its CPS version rather than itself on a
new stack (`camlc_cps_stack`, through `camlc_c_N`): on a fresh fiber, with a
continuation returning the result, whose CPS calls are tail calls or bounce on
the trampoline. The stack of the engine no longer grows, and the continuations
are closures on the heap. This allocates: the linear recursions (one call
which may be recursive on each path) then count as triggering a GC, and the
others switch only if they trigger one anyway, having a frame which measures
their depth. The functions it calls without CPS version run in direct style,
with a new segment of a quarter of the size; the recursions through functions
with unboxed arguments or results, or making several recursive calls without
frame (`fib`), or through callbacks from C, stay on the stack of the engine (or
go on new stacks with JSPI). This works in all browsers, with a synchronous
`main`, and with OCaml 4.14. The price:

- the code is larger: wasm +0 to +10% for small numeric programs, +50% for the
  programs using `Printf` or `Format` (24 benchmarks of the Ocsigen suite), +56%
  for camlboy, +61% for MOPSA (22 to 36 MB), against +4% with JSPI. The
  closures are a word larger (double translation);
- the code which does not go deep is as fast (23 benchmarks out of 24: within
  4%), but not that of the programs whose recursions often cross the limit
  (`kb`: 39,200 times, ×2): the code past it runs in CPS, about 2 to 3 times
  slower than direct style when it does not allocate (a recursion of depth
  1,000,000: 0.45 s against 0.10 s on a large stack and 0.25 s with JSPI), but
  faster when it does, the GC scanning a short shadow stack;
- an infinite recursion exhausts the memory rather than raising
  `Stack_overflow`.

The limit is `CAMLC_SEGMENT_WORDS` (2048 words, about 200 KB of the stack of
the engine), which can be raised when compiling `camlc_runtime.c` when the
program runs on the main thread (984 KB in V8): `kb` no longer switches with
6144.

With `--dynlink`, the program and its plugins are compiled with `--cps-stack`
(and so with double translation, which the loader checks). The plugins then
call `camlc_cps_stack` and `camlc_cps_stack_noexc`, which the program exports
with the other functions of `camlc_runtime.c`. A plugin compiled without it,
but with double translation, may still be loaded: its own recursions stay on
the stack of the engine, but the functions of the program it calls may go
deep. With JSPI, the code of the plugins goes on new segments too, whatever
their flags.

**Threads and domains.** Each thread running OCaml code (the systhreads, and
the domains of OCaml 5) has its own shadow stack and its own state of the
effect handlers, in a thread-local structure (`camlc_thread`) set up when it
first runs OCaml code. The threads are registered by domain: for each domain,
the GC scans the shadow stacks of its threads, and the main domain scans the
globals. When the program uses threads or domains (it calls `Thread.create`
or `Domain.spawn`), the generated code polls, at the start of each loop and of
each function that makes a tail call, as the native code of OCaml does: a
poll compares the allocation pointer with its limit, and runs the pending
actions (the stop-the-world collections of the other domains, a switch to
another thread, signal handlers, finalisers). The values held in C variables
are saved on the shadow stack around the actions, which may raise. The
allocations of the generated code also run the pending actions in their
slow path (the callbacks of `Gc.Memprof` too, at the allocation after the
one sampled): an exception they raise is kept, and raised at the next poll
or call of a C function which may raise. Without threads, domains nor
`Gc.Memprof`, the code does not poll. With OCaml 5, the loops whose first
block allocates do not poll either, but in the programs which use
`Gc.Memprof`, whose callbacks may raise to interrupt a computation
(memprof-limits).

**Backtraces** (with debug events, or `--debug-info`). When backtraces are active, a raise and
every call an exception passes through record a static description of their
location, taken from the debug events, in the runtime's backtrace buffer. The
call stack itself cannot be walked, so `Printexc.get_callstack` returns an
empty stack.

**Marshaling closures** (`Marshal.Closures`). The marshaler of OCaml expects
the closures of native code, with a second code pointer for the functions of
arity 2 or more. With `Marshal.Closures`, the runtime then marshals the
values itself, in the same format, which `input_value` reads. A code pointer
is an index in the table of functions of wasm, written as an offset in a code
fragment covering the table. Its digest combines the digest of the C code,
computed by c_of_ocaml, and the indices of some functions: only the same
program, linked in the same way, reads the closures back. With OCaml 4.14,
whose marshaler takes the code pointers for naked pointers and the odd ones
for integers, the runtime marshals all values.

**Inlining by V8.** V8 gives a wasm module a tenth of its inlining budget when
less than a quarter of its functions are small (under 50 bytes), and all of
it above half; few compiled functions are that small (a sixth of those of
ocamlc). The generated code therefore defines functions returning a constant,
three quarters as many as its own functions plus 500, kept in the table of
functions so that the linker and binaryen do not remove them. On the
benchmarks of the bench directory (44 programs), this makes the code 2.4 %
faster (`fib` 23 %, `effects` 22 %, `exn_deep` 19 %, `taku` 9 %), the largest
programs as fast or a little slower (js_of_ocaml compiling camlboy 12 %, which
spends more time compiling the inlined code), for wasm 3 % larger. `--disable
small-functions` leaves them out.

## Limitations

- Threads and domains need a runtime built with pthreads. The shadow stack
  of the threads other than the main one is smaller (1M words,
  `-DCAMLC_THREAD_STACK_WORDS=` when compiling `camlc_runtime.c`).
- The code of a program polls when it links `Thread` or `Domain`, as does
  a program loading plugins with OCaml 5 (with `-linkall`, it links
  `Domain`). Otherwise, threads which a plugin would start could not run
  while the program loops without allocating.
- With emscripten, a program loading plugins and linked with JSPI cannot
  start threads (nor domains) once it has loaded one: emscripten loads the
  plugins in the new thread asynchronously, outside a context which can
  suspend.
- Bytecode loading and shared libraries (`Dynlink`) are not supported: the
  plugins are compiled ahead of time (see above).
- Whole programs only, apart from plugins: there is no separate compilation.
- Effects cannot be performed across a callback from C: they raise
  `Effect.Unhandled`, as with js_of_ocaml. `Effect.Deep.get_callstack` is
  empty.
- Without JSPI nor `--cps-stack`, recursion depth is limited by the wasm
  engine's C stack (about 1 MB in V8), i.e. some tens of thousands of non-tail
  calls, whereas the bytecode interpreter has its own much larger stack.
  Exhausting it raises a JavaScript `RangeError` rather than `Stack_overflow`.
  Under node, ocaml-emcc runs compiled programs in a worker thread with a large
  stack to mitigate this. With JSPI or `--cps-stack` (see above), only
  recursions through functions making several calls each, or through
  callbacks from C, are limited (and with `--cps-stack`, through functions with
  unboxed arguments or results).
- With JSPI, the program is asynchronous: `main` returns a promise, settled
  when the program ends. JSPI is in Chrome and Edge since version 137, is
  announced for Firefox 153, needs `--experimental-wasm-jspi` with node 24,
  and is not in Safari.
- The exceptions of the signal handlers, finalisers and callbacks of
  `Gc.Memprof` are raised at the next poll or call of a C function which may
  raise, not at the allocation which ran them.
- An allocation whose result is not used is removed, even under
  `Sys.opaque_identity`, which the bytecode does not keep.

## Testing

`tools/c_of_ocaml_tests.sh` runs the tests of `compiler/tests-ocaml` through
c_of_ocaml, emcc and node. Its header documents the environment variables
(`WORD_SIZE=64` for wasm64). `make tests-c` builds c_of_ocaml and runs it for
wasm32. `tools/c_of_ocaml_dynlink_test.sh` tests the plugins
(`compiler/tests-c/dynlink`). `tools/c_of_ocaml_threads_test.sh` tests the
systhreads and the domains (`compiler/tests-c/threads`), natively, with the
runtime of the OCaml switch. `tools/c_of_ocaml_segments_test.sh` tests the
segmented stack (`compiler/tests-c/segments`), with JSPI and node 24, or
`MODE=cps` with `--cps-stack`, and the recursions through callbacks from C. `CPS_STACK=1 tools/c_of_ocaml_dynlink_test.sh`
tests plugins with `--cps-stack`, `JSPI=1` (node 24) with JSPI and the
segmented stack, `DOMAINS=1` a plugin starting a domain (linked with
`-sPROXY_TO_PTHREAD`). `COOC_FLAGS=--cps-stack
tools/c_of_ocaml_tests.sh` runs the tests with `--cps-stack`: the same pass,
and `misc/sieve` too.
`COOC_FLAGS='--toplevel-chunk-size 1' tools/c_of_ocaml_tests.sh` splits the
toplevel code of the tests at every point it can: the same pass.
`tools/c_of_ocaml_tests.sh compiler/tests-c/marshal/*.ml` tests
`Marshal.Closures`. `tools/c_of_ocaml_tests.sh compiler/tests-c/gc/*.ml` tests `Gc.Memprof`
and `Gc.create_alarm`. `tools/c_of_ocaml_tests.sh compiler/tests-c/tailcalls/*.ml`
tests tail calls between functions of different signatures, and loops of
tail calls through closures. `dune build @compiler/tests-c/runtest` checks the
primitives called by a program using `Js_of_ocaml.Js.Unsafe`.

All tests pass on both wasm32 and wasm64, except:

- tests that use domains
- tests that need arguments or files (`basic-io`, `basic-io-2`,
  `lib-channels/close_out`)
- tests that expect the behaviour of the JavaScript runtime
  (`lib-digest/blake2_long_key`, `lib-obj/new_obj`, `lib-bytes/test_bytes`)
- deep recursion tests (`misc/gcwords`, `misc/sieve`, `misc/pr7168`), also
  disabled for js_of_ocaml and wasm_of_ocaml.

## Source layout

- `compiler/bin-c_of_ocaml/`: the `c_of_ocaml` command.
- `compiler/lib-c/`: the C backend (closure conversion, call graph
  analyses, unboxing, and code generation).
- `runtime/c/`: `camlc.h` and `camlc_runtime.c`, the runtime support linked
  with the generated code.
- `tools/c_of_ocaml_tests.sh`, `tools/c_of_ocaml_dynlink_test.sh`,
  `tools/c_of_ocaml_threads_test.sh`, `tools/c_of_ocaml_segments_test.sh`:
  the test drivers.

Everything else comes from js_of_ocaml.

## Next steps

- Shrink-wrapping: allocate the shadow stack frame only on the paths that need
  it.
