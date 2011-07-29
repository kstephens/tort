#include "tort/tort.h"
#include <stdio.h>

int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v a;
  tort_runtime_create();
  io = tort_stdout;
  tort_printf(io, "HELLO!\n");
  a = tort_send(tort__s(new), tort__mt(pair), tort_true, tort_false);
  // fprintf(stderr, "a = @%p\n", a);
  a = tort_sprintf(tort_nil, "{{%T}}", a);
  tort_printf(io, "printf(...) => %T\n", a);
  printf("\nDONE\n");
  return 0;
}

