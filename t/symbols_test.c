#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_v io;
  tort_v v;

  tort_runtime_create();

  io = tort_stdout;

  printf("\n  (get symbols \"new\") => ");
  tort_write(io, tort_send(tort__s(get), _tort->symbols, v = tort_string_new_cstr("new")));

  printf("\n  symbols => ");
  tort_write(io, _tort->symbols);

  printf("\nDONE\n");

  return 0;
}

