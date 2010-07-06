#include "tort/tort.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_val io;

  tort_runtime_create();

  io = tort_stdout;

  printf("\n  123 => ");
  tort_write(io, tort_i(123));

  printf("\nDONE\n");

  return 0;
}

