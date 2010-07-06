#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_v io;

  tort_runtime_create();

  io = tort_stdout;

  printf("\n  nil => ");
  tort_write(io, tort_nil);

  printf("\nDONE\n");

  return 0;
}

