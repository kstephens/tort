#include "tort/tort.h"
#include <stdio.h>

int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_runtime_create();
  io = tort_stdout;
  tort_printf(io, "HELLO!\n");
  tort_printf(io, "printf(...) => %T\n", tort_sprintf(tort_nil, "{{%T}}", tort_send(tort__mt(pair), tort__s(new), tort_true, tort_false)));
  printf("\nDONE\n");
  return 0;
}

