#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_v io;
  tort_v v;

  tort_runtime_create();

  io = tort_stdout;

  v = tort_send(tort__s(get), _tort->symbols, tort_string_new_cstr("new"));

  tort_printf(io, "\n  (get symbols \"new\") => %T\n", v);

  tort_printf(io, "\n  symbols => %T\n", _tort->symbols);

  printf("\nDONE\n");

  return 0;
}

