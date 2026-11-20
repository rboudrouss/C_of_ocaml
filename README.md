# c_of_ocaml

c_of_ocaml compiles OCaml bytecode executables to C. Used with
[ocaml-emcc](https://github.com/rboudrouss/ocaml-emcc), the C code is compiled
to WebAssembly by emscripten together with the OCaml runtime and the C stubs
of the program. The program then runs as compiled wasm, about as fast as with
wasm_of_ocaml, instead of being interpreted by an `ocamlrun` built for wasm.

c_of_ocaml is a fork of [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml).

## Installation

c_of_ocaml needs OCaml 4.14 or later and is not in opam. Build it in the
switch of your project.

```sh
git clone https://github.com/rboudrouss/C_of_ocaml.git && cd C_of_ocaml
opam install -y --deps-only ./js_of_ocaml-compiler.opam
dune build --profile release compiler/bin-c_of_ocaml/c_of_ocaml.exe
export OCAML_EMCC_C_OF_OCAML=$PWD/_build/default/compiler/bin-c_of_ocaml/c_of_ocaml.exe
```

ocaml-emcc looks for c_of_ocaml in `$OCAML_EMCC_C_OF_OCAML`, then in `PATH`.

## Usage

In a dune project, ocaml-emcc does everything.

```sh
ocaml-emcc build main.js
node main.js
```

See the [ocaml-emcc README](https://github.com/rboudrouss/ocaml-emcc) for
browsers, WASI, plugins and threads. To run c_of_ocaml without it, see
[Without ocaml-emcc](#without-ocaml-emcc).

### Options

| Option | Effect |
| --- | --- |
| `-o FILE` | Write the C code to `FILE`. |
| `--split DIR` | Write the C code to several files (see [Large programs](#large-programs)). |
| `--part-size KB` | With `--split`, the size of each part (default 2048). |
| `--manifest FILE` | Write the [manifest](#manifest) to `FILE` (always written to `DIR/manifest` with `--split`). |
| `--toplevel-chunk-size N` | Split the toplevel code into functions of about `N` instructions (default 2000, 0 for a single function). |
| `--opt {1,2,3}` | Optimization profile (default 1). |
| `--debug-info`, `--no-debug-info` | Record backtraces or not. By default they are recorded when the bytecode has debug events (`ocamlc -g`). |
| `--word-size {32,64}` | Target wasm32 (31-bit integers, the default) or wasm64 (63-bit integers). |
| `--effects {disabled,double-translation}` | Effect handler support. By default double translation is used only when the program performs effects, or with `--cps-stack`. Plugins must be compiled with the option of their program. |
| `--cps-stack` | Run deep recursions in CPS on the heap (see [CPS stack](#cps-stack)). |
| `--dynlink` | Compile a program which can load [plugins](#plugins). |
| `--runtime-files DIR` | Write the C runtime support files to `DIR`. |
| `--disable small-functions` | Do not add the small functions meant for V8's inlining budget (see [Inlining by V8](#inlining-by-v8)). |
| `--check` | Check the options without compiling anything. The exit code is 2 for an unknown option, 1 for incompatible options and 0 otherwise. |

## Without ocaml-emcc

Any bytecode executable works as input, whether it is built with `ocamlc`,
ocamlfind or dune. The OCaml runtime for wasm comes from the sysroot built by
ocaml-emcc.

```sh
ocamlc main.ml -o main.bc
c_of_ocaml main.bc -o main.c

SYSROOT=$OPAM_SWITCH_PREFIX/emcc-sysroot/wasm32
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
`main` and replaces the `callback.c`, `backtrace_byt.c`, `meta.c` and
`dynlink.c` files of the runtime. Outside this tree, `c_of_ocaml
--runtime-files DIR` writes `camlc.h` and `camlc_runtime.c` to `DIR`.

For wasm64, pass `--word-size 64` to c_of_ocaml, use the wasm64 sysroot, add
`-m64` to the emcc flags and `-sMAXIMUM_MEMORY=16GB` when linking. Running the
result needs node 24 or later.

### The host

The generated code and its runtime support depend only on the OCaml runtime
and on wasm, not on emscripten. Everything that depends on the platform is
left to the host which compiles and links the C code, ocaml-emcc or another.

A host can start the program from its own `main` by calling
`camlc_main(argv)` and compiling `camlc_runtime.c` with `-DCAMLC_NO_MAIN`. It
can also run functions on new stacks of the engine, for the [segmented
stack](#segmented-stack), by defining `camlc_host_new_stacks` and
`camlc_host_on_new_stack` (see `camlc.h`). The weak definitions in
`camlc_runtime.c` do not. `compiler/tests-c/segments/jspi_host.c` does it with
emscripten's JSPI.

The settings of the OCaml runtime belong to the host. For instance OCaml 5.3
defaults to 128 domains, and the runtime of ocaml-emcc defaults to 1 without
threads. Everything else the host needs to know about the C code is in the
[manifest](#manifest).

### Large programs

clang's memory use and compile time grow with the size of a translation unit.
`c_of_ocaml main.bc --split DIR` writes the static data and tables to
`DIR/part0.c` and the functions to `DIR/part1.c`, `DIR/part2.c` and so on,
which are compiled separately.

The time clang takes to optimize a function also grows faster than its size.
The toplevel code, which initializes all the modules, is therefore split into
functions of about 2000 instructions of the intermediate code
(`--toplevel-chunk-size`), called in turn by `camlc_program`. A chunk starts
at a point where no local variable is live (long-lived values are C globals)
and outside any `try` or loop.

Generated C names do not depend on the rest of the program. Functions are
named after their source location, constants after their contents, and the
toplevel chunks start at points chosen by a hash of their names. After an
edit, only `part0.c` and the parts whose code changed differ. Unchanged files
are not rewritten, so a build system only recompiles what changed.

### Manifest

With `--split DIR` or `--manifest FILE`, c_of_ocaml writes what the host needs
to compile and link the C code, so that the host never has to read it. Each
line is a `KEY VALUE` pair.

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

`mode` is `program`, `dynlink` or `plugin`.

`effects` is `disabled` or `double-translation`. The plugins of a `--dynlink`
program must be compiled with the same value.

`uses_effects` is `true` when the code performs effects (`%perform`,
`%resume`...) outside of its dead code, whatever `effects` says. For a
`--dynlink` program, compilation units are not counted as used by its plugins.
For a plugin, they are counted as used by the plugins loaded after it. A host
can use it to refuse a plugin which performs effects in a program compiled
without them.

`primitive NAME ARITY` lists the C primitives the code calls with the number
of arguments at their call sites, or `-1` for those called with an array of
arguments (`value *argv, int argc`, more than 5 arguments). A host uses this
to adapt stubs whose C signature differs, or to provide missing ones.

More keys may be added later, and unknown keys should be ignored.

### C stubs

The generated code calls primitives directly and declares them with the arity
of their call sites. A stub defined with a different signature, say
`ml_z_init(void)` called with one argument, would trap at run time. To catch
this at link time, the generated file contains an uncalled function which
makes wasm-ld report each mismatch as `function signature mismatch: NAME`.

Such a primitive needs an adapter. Compile the generated file with
`-DNAME=ADAPTER`, where `ADAPTER` takes the arguments of the call site and
calls the stub with its real signature.

### Plugins

A program compiled with `--dynlink` can load plugins compiled ahead of time
as emscripten side modules. The program is linked with `-linkall`, so that all
its units are present, and with `-sMAIN_MODULE=2`, exporting the symbols the
plugins use (the primitives and the functions and data of `camlc_runtime.c`
and of the OCaml runtime).

```sh
ocamlc -linkall shared.cmo main.ml -o main.bc
c_of_ocaml --dynlink main.bc -o main.c
emcc $CFLAGS main.o camlc_runtime.o ... -sMAIN_MODULE=2 -sEXPORTED_FUNCTIONS=...
```

A `.cmo` or `.cma` file given to c_of_ocaml is compiled to a plugin, with the
same `--word-size` and `--effects` as the program.

```sh
c_of_ocaml plugin.cma -o plugin.c          # or --split DIR
emcc $CFLAGS -fPIC -c plugin.c
emcc $CFLAGS -sSIDE_MODULE=1 plugin.o -o plugin.so
```

The program registers its compilation units by name and keeps them whole,
since a plugin may use any of their fields. A plugin refers by name to the
units of the program and of the plugins loaded before it. All its symbols are
hidden except its entry point, `value camlc_plugin_init(void)`, which the
loader finds with `dlsym` and calls like a primitive (it raises with
`caml_raise`).

Like `Dynlink`, the loader checks that the units the plugin needs are loaded,
that the ones it defines are not, and that the interfaces it was compiled
against match the loaded units (their CRCs). It also checks that the program
and the plugin use the same effects option. It raises `Failure` if a check
fails, then initializes the units of the plugin and lets their exceptions
through. The API (`camlc_load_plugin`, `camlc_find_unit`) is documented in
`runtime/c/camlc.h`. Functions of a plugin take at most 64 arguments.

## Limitations

- Bytecode loading and `Dynlink` of shared libraries are not supported. Only
  plugins compiled ahead of time can be loaded (see [Plugins](#plugins)).
- Whole programs only, apart from plugins. There is no separate compilation.
- Without JSPI or `--cps-stack`, recursion depth is limited by the stack of
  the wasm engine (about 1 MB in V8), some tens of thousands of non-tail
  calls. Exhausting it raises a JavaScript `RangeError` rather than
  `Stack_overflow`. ocaml-emcc runs programs under node in a worker thread
  with a large stack to work around this.
- With JSPI or `--cps-stack`, only recursions through functions making several
  calls each, or through callbacks from C, are limited. With `--cps-stack`,
  so are those through functions with unboxed arguments or results.
- With JSPI, the program is asynchronous and `main` returns a promise settled
  when the program ends. JSPI is in Chrome and Edge since version 137, is
  announced for Firefox 153, needs `--experimental-wasm-jspi` with node 24 and
  is not in Safari.
- Effects cannot be performed across a callback from C. They raise
  `Effect.Unhandled`, as with js_of_ocaml. `Effect.Deep.get_callstack` and
  `Printexc.get_callstack` return empty stacks.
- Threads and domains need a runtime built with pthreads. Threads other than
  the main one have a smaller shadow stack (1M words, set with
  `-DCAMLC_THREAD_STACK_WORDS=` when compiling `camlc_runtime.c`).
- With emscripten, a program which loads plugins and is linked with JSPI
  cannot start threads or domains once it has loaded one, because emscripten
  loads the plugins in the new thread asynchronously, outside a context which
  can suspend.
- The exceptions raised by signal handlers, finalisers and `Gc.Memprof`
  callbacks are raised at the next poll or call to a C function which may
  raise, not at the allocation which ran them.
- An allocation whose result is unused is removed, even under
  `Sys.opaque_identity`, which the bytecode does not keep.

## Design

c_of_ocaml reuses the front-end and the optimizer of js_of_ocaml. The bytecode
is parsed into the intermediate representation (`compiler/lib/code.mli`),
optimized by `Driver.optimize_for_wasm`, and translated to C by
`compiler/lib-c`. The conventions of the generated code are documented in
`runtime/c/camlc.h`. Both OCaml 4.14 and 5.x runtimes are supported, and
`camlc_runtime.c` follows the startup and callback code of each.

### Values

Values use the representation of the bytecode runtime. Constants are static
data whose headers the GC never marks (on 4.14 they are registered as static
data in the page table). Toplevel values used by functions are C globals,
which the GC scans. The generated code lists both in a table,
`camlc_program_data`, because sections bounded by the linker would not work
with the dynamic linking of emscripten.

### Functions

Each OCaml function becomes a C function taking its arguments followed by its
closure. Closures keep the bytecode layout, including `Infix_tag` blocks for
mutually recursive functions, with C function pointers as code pointers, so
`compare`, `hash` and `Obj` keep working.

Calls to known functions are direct C calls. Other calls with the right
number of arguments go through the code pointer, and the rest go through
`camlc_applyN`, which handles partial application and over-application. A
partial application allocates a closure whose code calls the function
directly once it has all its arguments. These closures are generated for
functions of arity up to 8, like the `caml_curry` functions of ocamlopt.
Callbacks from C (`caml_callback*`) apply the closure with `camlc_applyv`.

### Tail calls

With the WebAssembly tail call extension (`-mtail-call`), a call in tail
position is emitted with `musttail` when the callee has the signature of the
caller.

When the signatures differ and the call may be recursive, it goes through a
function with the signature of the caller (`camlc_t_...`), called with
`musttail`, which finds the callee and its arguments in `camlc_seg` and calls
it in tail position. A call may be recursive when the callee is unknown, is in
the strongly connected component of the caller in the graph of known calls,
or may itself make an unknown tail call, directly or through known tail calls.
The last case covers loops of tail calls through closures, such as the
continuations of code in CPS. Without this, clang would turn the call into a
plain call as soon as the caller has another return path (an exception), and
mutually recursive functions with different signatures would grow the stack
of the engine. Other tail calls are plain calls, which clang may inline.

An over-application applies the function, then its results, to the first
arguments (`camlc_apply_prefix`), and makes the last application in tail
position (`camlc_lastN_K`), as OCaml does.

Without the tail call extension (with asyncify, or `ocaml-emcc
--no-tail-calls` for browsers which lack it), tail calls are plain calls. The
ones which may loop count how many are in progress (`camlc_tail_depth`). Past
`CAMLC_TAIL_DEPTH` (256), such a call is left pending. Its callee and
arguments go to `camlc_bounce_args`, and it returns `CAMLC_PENDING`, an
exception result which the frames of the tail calls return unchanged. The
first call which is not a tail call, in the generated code or in the runtime,
then makes it (`camlc_run_pending`). A loop of tail calls through closures
therefore still runs in constant space. Only calls to functions which may
return a pending call check for it (`pending` in
`compiler/lib-c/codegen/c_summary.ml`). None of this is compiled with
`-mtail-call`.

### GC roots

Variables live across a point where the GC may run (an allocation or a call)
are stored in the frame of the function on a shadow stack, which the GC scans
through `caml_scan_roots_hook`. Variables holding integers or static data are
exempt. All variables are C locals. Those in the frame are also written to it
where they are defined and reloaded from it after each point where the GC may
run, so loops without such points never touch the frame.

### Exceptions

Exceptions are return values. Raising returns `Make_exception_result(exn)`,
and every call is followed by a check which jumps to the innermost handler or
returns the exception to the caller. The generated code never calls `setjmp`,
because emscripten would wrap every call in such a function in an exception
handler.

C primitives raise with `longjmp`. They are called through wrappers which
catch the exception with `sigsetjmp` and return it, except for the primitives
known not to raise. `camlc_runtime.c` has versions of a few frequent
primitives which return their exceptions, or have a fast path which cannot
raise before falling back to the wrapper. This covers polymorphic comparison
(on integers, strings, floats and small blocks of those), `int_of_string` and
`Bytes.create`. Other primitives are inlined in the generated code through
`camlc.h`. These are the 16, 32 and 64-bit accesses to strings and bytes
(`Bytes.get_int32_le`...), byte swaps, string lengths and bigarray accesses,
done directly on the elements when the type analysis knows the kind and
layout of the bigarray.

One `setjmp` per `try` instead of one per primitive call was tried. Each
`try` then cost about as much as a wrapper, and a loop around `try ... with
Not_found` became 70% slower.

### Unboxed numbers

The type analysis of wasm_of_ocaml (`compiler/lib-c/analysis/typing.ml`)
finds the floats and boxed integers which never need to be boxed. They become
C `double`s and integers, with inlined arithmetic. Parameters of functions
which are only called directly can be unboxed too, and so can their results
when they cannot raise, since exceptions are return values.

A number used as a value is boxed once, where it is defined. A number only
passed to calls outside of loops is instead boxed at these calls, when it is
defined in a loop (an accumulator printed after the loop) or passed to a
single call. Boxed integers are allocated inline, like floats.

### Effect handlers

Effect handlers (OCaml 5) use the double translation of js_of_ocaml
(`compiler/lib/effects.ml`). Functions which may perform an effect get a
second CPS version, whose code pointer follows the closure info in the
closure (`camlc_no_cps` for the other functions). The rest of the program is
unchanged.

Running a computation under a handler (`caml_resume` in `camlc_runtime.c`,
modelled on the CPS runtime of wasm_of_ocaml) switches to the CPS versions.
Fibers are OCaml blocks. CPS calls are tail calls when `musttail` allows it.
Otherwise they track the depth of the C stack and bounce through a trampoline
past a threshold.

### Segmented stack

The generated functions run on the stack of the wasm engine, which browsers
keep small (984 KB in V8, 500 KB in the workers of Chrome). That is tens of
thousands of nested calls, while the bytecode interpreter has a stack of its
own.

When the host can run a function on a new stack of the engine
(`camlc_host_on_new_stack`), `camlc_runtime.c` uses it to chain stacks. With
emscripten, this is done with JSPI (`-sJSPI`, JavaScript Promise Integration),
where the program runs in a context which can suspend. The shadow stack
measures the depth. A function whose frame would go below
`camlc_segment_limit`, about 2048 words below the start of the current
segment, calls itself again on a new stack of the engine (`camlc_segment`),
and the current stack is suspended until it returns. The arguments go through
`camlc_seg`, and a thunk per signature (`camlc_s_...`) makes the call.

A frame on the shadow stack then also stands for the frame of the function on
the stack of the engine. A function which calls other functions gets at least
one word per 100 bytes of its engine frame, which is about 80 bytes plus 2 per
C variable. A function without a frame gets one if a path in it makes a single
call which may be recursive (a call in the same strongly connected component
of known calls, or an unknown call). Functions making several such calls, like
`fib`, are not measured, since the depth of their recursion is that of a tree.
A function restarts on a new stack only from its start, or from where it
allocates its frame if nothing before has an effect. Otherwise it goes on on
the current stack.

No new segment is started in a callback from C, where JavaScript may be on
the stack and JSPI cannot suspend, nor in threads other than the one which
starts the program (pthreads run in workers which are not in such a context).
Without JSPI the limit is that of the shadow stack, and node runs compiled
programs in a worker thread with a large stack.

The C stack of emscripten (`__stack_pointer` in the linear memory, sized by
`-sSTACK_SIZE`) is separate from the stack of the engine. The generated
functions never take the address of their variables and make no variadic
call, so they have no frame there. The slow paths of applications put the
function and its arguments on the shadow stack instead (`camlc_applyN_slow`,
`camlc_apply_frame`). A recursion only makes the C stack grow through C
functions (callbacks). A callback raises `Stack_overflow` when less than an
eighth of it (at most 64 KB) is left, keeping room for the C functions it may
call. Give it at least 1 MB (ocaml-emcc gives 8 MB). An overflow caused by C
functions alone goes unnoticed unless the program is linked with
`-sSTACK_OVERFLOW_CHECK`.

### CPS stack

With `c_of_ocaml --cps-stack`, deep recursions go on in continuation-passing
style on the heap, without JSPI. This works in all browsers, with a
synchronous `main`, and with OCaml 4.14.

The functions which may recurse (in a cycle of known calls, or making unknown
calls) get a CPS version, as with effect handlers, since this option turns on
the double translation. So do their calls which may be recursive
(`recursive_functions` in `compiler/lib/partial_cps_analysis.ml`), but not
their callers. Past the segment limit, such a function calls its CPS version
instead of calling itself on a new stack (`camlc_cps_stack`, through
`camlc_c_N`). The CPS version runs on a fresh fiber, with a continuation
returning the result, and its CPS calls are tail calls or bounce on the
trampoline. The stack of the engine stops growing and the continuations are
closures on the heap.

This allocates, so linear recursions (one call which may be recursive on each
path) count as triggering a GC. The other functions switch only if they
trigger one anyway and have a frame measuring their depth. The functions they
call which have no CPS version run in direct style, with a new segment a
quarter of the size. Recursions through functions with unboxed arguments or
results, through functions making several recursive calls without a frame
(`fib`), or through callbacks from C stay on the stack of the engine (or go
on new stacks with JSPI).

The code is larger. The wasm grows by 0 to 10% for small numeric programs, by
50% for programs using `Printf` or `Format` (24 benchmarks of the Ocsigen
suite), 56% for camlboy and 61% for MOPSA (22 to 36 MB), against 4% with
JSPI. Closures are also one word larger because of the double translation.

Code which does not recurse deeply is as fast (23 benchmarks out of 24 are
within 4%). Programs whose recursions often cross the limit are slower. `kb`
crosses it 39,200 times and takes twice as long. Past the limit, the code runs
in CPS, 2 to 3 times slower than direct style when it does not allocate. A
recursion of depth 1,000,000 takes 0.45 s, against 0.10 s on a large stack
and 0.25 s with JSPI. When the code allocates, CPS is faster, since the GC
scans a short shadow stack.

An infinite recursion exhausts the memory instead of raising
`Stack_overflow`.

The limit is `CAMLC_SEGMENT_WORDS`, 2048 words or about 200 KB of the stack of
the engine. It can be raised when compiling `camlc_runtime.c` if the program
runs on the main thread (984 KB in V8). With 6144, `kb` no longer switches.

When a `--dynlink` program and its plugins are compiled with `--cps-stack`,
and so with double translation, which the loader checks, the plugins call
`camlc_cps_stack` and `camlc_cps_stack_noexc`, which the program exports with
the other functions of `camlc_runtime.c`. A plugin compiled with double
translation but without `--cps-stack` can still be loaded. Its own recursions
stay on the stack of the engine, but the functions of the program it calls
may go deep. With JSPI, the code of plugins uses new segments too, whatever
their flags.

### Threads and domains

Each thread running OCaml code (systhreads, and the domains of OCaml 5) has
its own shadow stack and its own state of the effect handlers, in a
thread-local structure (`camlc_thread`) set up when it first runs OCaml code.
Threads are registered by domain. For each domain, the GC scans the shadow
stacks of its threads, and the main domain also scans the globals.

When the program uses threads or domains (it calls `Thread.create` or
`Domain.spawn`), the generated code polls at the start of each loop and of
each function making a tail call, as the native code of OCaml does. A poll
compares the allocation pointer with its limit and runs the pending actions,
such as stop-the-world collections of other domains, thread switches, signal
handlers and finalisers. The values held in C variables are saved on the
shadow stack around these actions, which may raise.

Allocations of the generated code also run the pending actions in their slow
path, including the callbacks of `Gc.Memprof` at the allocation after the
sampled one. An exception raised there is kept and raised at the next poll or
call to a C function which may raise.

Without threads, domains or `Gc.Memprof`, the code does not poll. With OCaml
5, loops whose first block allocates do not poll either, except in programs
using `Gc.Memprof`, whose callbacks may raise to interrupt a computation
(memprof-limits).

The code of a program loading plugins also polls with OCaml 5, since
`-linkall` links `Domain`. Otherwise, threads started by a plugin could not
run while the program loops without allocating.

### Backtraces

With debug events or `--debug-info`, when backtraces are active, a raise and
every call an exception passes through record a static description of their
location, taken from the debug events, in the backtrace buffer of the runtime.
The call stack itself cannot be walked, so `Printexc.get_callstack` returns an
empty stack.

### Marshaling closures

The marshaler of OCaml expects the closures of native code, with a second
code pointer for functions of arity 2 or more. With `Marshal.Closures`, the
runtime marshals the values itself, in the same format, which `input_value`
reads. A code pointer is an index in the function table of wasm, written as an
offset in a code fragment covering the table. Its digest combines the digest
of the C code, computed by c_of_ocaml, with the indices of some functions, so
only the same program, linked the same way, can read the closures back. With
OCaml 4.14, whose marshaler takes code pointers for naked pointers and odd
ones for integers, the runtime marshals all values.

Since OCaml 5.5, `input_value` checks that closures have the layout of native
code, and refuses the closures of the compiled code. When it does, the runtime
unmarshals the value itself. Custom blocks are still unmarshaled by OCaml, one
at a time.

### Inlining by V8

V8 gives a wasm module a tenth of its inlining budget when less than a quarter
of its functions are small (under 50 bytes), and all of it above half. Few
compiled functions are that small, a sixth of those of ocamlc. The generated
code therefore defines functions returning a constant, three quarters as many
as its own functions plus 500, kept in the function table so that the linker
and binaryen do not remove them.

On the 44 programs of the bench directory, this makes the code 2.4% faster
(`fib` 23%, `effects` 22%, `exn_deep` 19%, `taku` 9%) for a wasm 3% larger.
The largest programs are as fast or a little slower (js_of_ocaml compiling
camlboy is 12% slower, since it spends more time compiling the inlined code).
`--disable small-functions` leaves these functions out.

## Testing

The tests need emcc, node and the sysroot of ocaml-emcc. `make tests-c` builds
c_of_ocaml and runs `tools/c_of_ocaml_tests.sh` for wasm32. The header of each
script documents its environment variables.

| Command | What it tests |
| --- | --- |
| `tools/c_of_ocaml_tests.sh` | The tests of `compiler/tests-ocaml` (`WORD_SIZE=64` for wasm64). |
| `COOC_FLAGS=--cps-stack tools/c_of_ocaml_tests.sh` | The same with `--cps-stack`, plus `misc/sieve`. |
| `COOC_FLAGS='--toplevel-chunk-size 1' tools/c_of_ocaml_tests.sh` | The same with the toplevel code split at every possible point. |
| `TAIL_CALLS=no tools/c_of_ocaml_tests.sh` | The same without wasm tail calls (add `COOC_FLAGS='--effects=double-translation --cps-stack'` for the stack of node). |
| `tools/c_of_ocaml_tests.sh compiler/tests-c/tailcalls/*.ml` | Tail calls between functions of different signatures, and loops of tail calls through closures. |
| `tools/c_of_ocaml_tests.sh compiler/tests-c/marshal/*.ml` | `Marshal.Closures`. |
| `tools/c_of_ocaml_tests.sh compiler/tests-c/gc/*.ml` | `Gc.Memprof` and `Gc.create_alarm`. |
| `tools/c_of_ocaml_dynlink_test.sh` | Plugins (`compiler/tests-c/dynlink`). `CPS_STACK=1` with `--cps-stack`, `JSPI=1` with JSPI and the segmented stack (node 24), `DOMAINS=1` a plugin starting a domain. |
| `tools/c_of_ocaml_threads_test.sh` | Systhreads and domains (`compiler/tests-c/threads`), natively with the runtime of the switch. |
| `tools/c_of_ocaml_segments_test.sh` | The segmented stack with JSPI and node 24 (`compiler/tests-c/segments`), or `MODE=cps` for `--cps-stack`, including recursions through callbacks from C. |
| `dune build @compiler/tests-c/runtest` | The primitives called by a program using `Js_of_ocaml.Js.Unsafe`. |

The tests of `compiler/tests-ocaml` which fail with c_of_ocaml are listed in
`compiler/tests-c/known_failures`, with the reason. They need arguments or
files, expect the behaviour of the JavaScript runtime, recurse deeply, start
domains, or expect the output of OCaml 5. `tools/c_of_ocaml_tests.sh` exits
with 1 when another test fails, and so do the other scripts when one of
their tests fails.

The CI (`.github/workflows/c_of_ocaml.yml`) runs these commands on OCaml
4.14 and 5.5, the suite of `compiler/tests-ocaml` on wasm32 and wasm64, and
its other modes on OCaml 5.5 and wasm32. The sysroot is built by the `main`
branch of ocaml-emcc.

## Source layout

The `c_of_ocaml` command is in `compiler/bin-c_of_ocaml/`. The C backend
(closure conversion, call graph analyses, unboxing and code generation) is in
`compiler/lib-c/`, described in its own README. The runtime support linked
with the generated code, `camlc.h` and `camlc_runtime.c`, is in `runtime/c/`.
The test drivers are the `tools/c_of_ocaml_*.sh` scripts. Everything else
comes from js_of_ocaml.

## Next steps

Shrink-wrapping, which would allocate the shadow stack frame only on the
paths that need it.
