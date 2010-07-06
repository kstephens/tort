#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_v io;
  tort_v v, c;

  tort_runtime_create();

  io = tort_stdout;

  printf("\n  (v = \"new\") =>");
  tort_inspect(io, v = tort_string_new_cstr("new"));
  
  printf("\n  (clone v) => ");
  tort_inspect(io, v = tort_send(tort__s(clone), v));

  printf("\n  (size v) => ");
  tort_inspect(io, c = tort_send(tort__s(size), v));

  printf("\n  (alloc_size v) => ");
  tort_inspect(io, c = tort_send(tort__s(alloc_size), v));

  printf("\n  (get v 2) => ");
  tort_inspect(io, c = tort_send(tort__s(get), v, tort_i(1)));

  printf("\n  (set v 2 +1) => ");
  tort_inspect(io, tort_send(tort__s(set), v, tort_i(1), tort_i(tort_I(c) + 1)));

  printf("\nDONE\n");

  return 0;
}

