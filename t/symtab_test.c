#include "tort/tort.h"
#include "tort/init.h"

#include <assert.h>


int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v s, p, v;
  tort_v st;

  tort_runtime_create();

  st = _tort->_symtab;

  io = tort_stdout;
  
  s = tort_s(tort_runtime_initialize_symtab);
  p = tort_i((long) &tort_runtime_initialize_symtab);

  tort_printf(io, "  (size _symtab) => %T\n", tort_send(tort__s(size), st));

  v = tort_send(tort__s(get), st, s);
  if ( v != tort_nil ) {
    assert(v == p);

    v = tort_send(tort__s(get), st, p);
    assert(v == s);
  }

  printf("\nDONE\n");

  return 0;
}

