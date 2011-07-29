#include "tort/tort.h"

#include <stdio.h>

int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v v;

  tort_runtime_create();

  io = tort_stdout;

  v = tort_send(tort__s(get), tort_(symbols), tort_string_new_cstr("size"));

  tort_printf(io, "  v = %T\n", v);

  tort_printf(io, "  (size symbols) => %T\n", 
	      tort_send(tort__s(size), tort_(symbols)));

  printf("\nDONE\n");

  return 0;
}

