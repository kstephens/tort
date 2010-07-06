#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_v io;
  tort_v v;

  tort_runtime_create();

  io = tort_stdout;

  v = tort_send(tort_s(__message), io);
  tort_printf(io, "Some object ==> ( %T ) <== is in here!\n", v);

  printf("\nDONE\n");

  return 0;
}

