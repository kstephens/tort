#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>


int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v v;

  tort_runtime_create();

  io = tort_stdout;

  v = tort_vector_new(0, 10);

  tort_printf(io, "v => %T\n", v);
  tort_printf(io, "(size v) => %T\n", tort_send(tort__s(size), v));

  tort_printf(io, "v[1] = %T\n", tort_send(tort__s(get), v, tort_i(1)));

  tort_send(tort__s(set), v, tort_i(1), tort_i(1));
  tort_printf(io, "v => %T\n", v);

  tort_printf(io, "v[1] => %T\n", tort_send(tort__s(get), v, tort_i(1)));
  tort_printf(io, "v => %T\n", v);

  printf("\n  (clone v) => ");
  tort_inspect(io, tort_send(tort__s(clone), v));

  printf("\nDONE\n");

  return 0;
}

