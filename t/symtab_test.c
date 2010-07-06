#include "tort/tort.h"
#include "tort/init.h"

#include <assert.h>


int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v s, p, v;

  tort_runtime_create();

  io = tort_stdout;
  
  s = tort_s(tort_runtime_initialize_symtab);
  p = tort_i((long) &tort_runtime_initialize_symtab);

  v = tort_send(tort__s(get), _tort->_symtab, s);
  assert(v == p);

  v = tort_send(tort__s(get), _tort->_symtab, p);
  assert(v == s);

  printf("\nDONE\n");

  return 0;
}

