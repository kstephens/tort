#include "tort/tort.h"
#include "tort/init.h"

#include <assert.h>

int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v s, p, v;
  tort_v st;

  tort_runtime_create();
  st = tort_send(tort_s(get), tort_(root), tort_s(core_symtab));
  assert(st);

  io = tort_stdout;
  
  s = tort_s(tort_runtime_initialize_dynlib);
  p = tort_ptr_new(&tort_runtime_initialize_dynlib);

  tort_printf(io, "  (size _symtab) => %T\n", tort_send(tort__s(size), st));

  v = tort_send(tort__s(get), st, s);
#if 0
  // FIXME
  if ( v != tort_nil ) {
    assert(v == p);

    v = tort_send(tort__s(get), st, p);
    assert(v == s);
  }
#endif

  printf("\nDONE\n");

  return 0;
}

