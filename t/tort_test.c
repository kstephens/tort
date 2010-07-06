#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_v io;

  tort_runtime_create();

  io = tort_stdout;

  tort_printf(io, "HELLO!\n");

  tort_send(tort_s(__debugger), io);

  printf("\nDONE\n");

  return 0;
}

