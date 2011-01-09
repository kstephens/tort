#include "tort/tort.h"
#include <stdio.h>

int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_runtime_create();
  io = tort_stdout;
  tort_send(tort_s(foobar), io);
  printf("\nDONE\n");
  return 0;
}

