#include "tort/tort.h"

#include <stdio.h>

int main(int argc, char **argv, char **environ)
{
  tort_v io;
  //  tort_v v, c;

  tort_runtime_create();

  io = tort_stdout;

  tort_printf(io, "\n  true => %T\n", tort_true);
  
  tort_printf(io, "\n  false => %T\n", tort_false);

  printf("\nDONE\n");

  return 0;
}

