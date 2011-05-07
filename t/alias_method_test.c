#include "tort/tort.h"

#include <stdio.h>

int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v v;

  tort_runtime_create();

  io = tort_stdout;

  v = tort_send(tort__s(_new), tort__mt(vector), 0, 10);

  tort_printf(io, "(size v) => %T\n", tort_send(tort__s(size), v));

  tort_send(tort_s(alias_method), tort__mt(vector), tort_s(size_other), tort_s(size));

  tort_printf(io, "(size_other v) => %T\n", tort_send(tort_s(size_other), v));

  /* eq? is defined in mt(object). */
  tort_send(tort_s(alias_method), tort__mt(vector), tort_s(EQ), tort_s(eqQ));

  tort_printf(io, "(= v v) => %T\n", tort_send(tort_s(EQ), v, v));

  printf("\nDONE\n");

  return 0;
}

