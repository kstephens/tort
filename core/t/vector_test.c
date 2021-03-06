#include "tort/tort.h"

#include <stdio.h>

int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v v;

  tort_runtime_create();

  io = tort_stdout;

  v = tort_send(tort__s(_new), tort__mt(vector), 0, 10);

  tort_printf(io, "v => %T\n", v);
  tort_printf(io, "(size v) => %T\n", tort_send(tort__s(size), v));

  tort_printf(io, "v[1] = %T\n", tort_send(tort__s(get), v, tort_i(1)));

  tort_send(tort__s(set), v, tort_i(1), tort_i(1));
  tort_printf(io, "v => %T\n", v);
  {
    int i;
    for ( i = 0; i < tort_vector_base_size(v); ++ i ) {
      tort_send(tort__s(set), v, tort_i(i), tort_i(i));
    }
  }
  tort_printf(io, "v => %T\n", v);

  tort_printf(io, "v[1] => %T\n", tort_send(tort__s(get), v, tort_i(1)));
  tort_printf(io, "v => %T\n", v);

  tort_printf(io, "(clone v) => %T\n", v = tort_send(tort__s(clone), v));

  tort_printf(io, "(_delete_n v 2 3) => %T\n", tort_send(tort__s(_delete_n), v, tort_i(2), tort_i(3)));

  printf("\nDONE\n");

  return 0;
}

