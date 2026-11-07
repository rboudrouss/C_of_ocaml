/* A C function calling OCaml back, with a frame on the C stack of
   emscripten, for compiler/tests-c/segments/callbacks.ml */

#include <string.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

value callbacks_through_c(value f, value n)
{
  volatile char frame[256];
  memset((char *)frame, 1, sizeof(frame));
  value r = caml_callback(f, n);
  return Val_long(Long_val(r) + frame[0] - 1);
}
