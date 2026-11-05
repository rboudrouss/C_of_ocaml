/* The new stacks of the segmented stack (camlc.h) with emscripten's JSPI
   (-sJSPI), for tools/c_of_ocaml_segments_test.sh: the host of the
   generated code provides them (ocaml-emcc's share/runtime/jspi.c).

   Main runs in a context which can suspend. [jspi_switch] suspends it,
   until [jspi_run] has called the function on a new stack, in a new
   context. The call is delayed to a microtask, so that the JS function
   returns and its frame does not stay on the stack of the engine. */

#include <emscripten/em_js.h>

EM_JS(int, jspi_available, (void), {
  return typeof Asyncify == 'object' &&
    typeof Asyncify.makeAsyncFunction == 'function' &&
    typeof WebAssembly.promising == 'function';
});

EM_ASYNC_JS(void, jspi_switch, (void), {
  await 0;
  if (!Module.jspiRun)
    Module.jspiRun = WebAssembly.promising(wasmExports['jspi_run']);
  await Module.jspiRun();
});

static _Thread_local void (*jspi_f)(void);

EMSCRIPTEN_KEEPALIVE void jspi_run(void) { jspi_f(); }

int camlc_host_new_stacks(void) { return jspi_available(); }

void camlc_host_on_new_stack(void (*f)(void))
{
  jspi_f = f;
  jspi_switch();
}
