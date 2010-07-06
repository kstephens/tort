#include "tort/tort.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_v io;

  tort_runtime_create();

  io = tort_stdout;

  printf("\n  123 => ");
  tort_inspect(io, tort_i(123));

  printf("\nDONE\n");

  return 0;
}

