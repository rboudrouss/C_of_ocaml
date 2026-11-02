/* The loading of a plugin by the test program */

#include <dlfcn.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>

value test_loadfile(value path)
{
  void *h = dlopen(String_val(path), RTLD_NOW | RTLD_LOCAL);
  value (*init)(void);
  if (h == NULL) caml_failwith(dlerror());
  init = (value (*)(void))dlsym(h, "camlc_plugin_init");
  if (init == NULL) caml_failwith("no camlc_plugin_init");
  return init();
}
