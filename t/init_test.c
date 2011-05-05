#include "tort/tort.h"

#include <stdio.h>

int main(int argc, char **argv, char **environ)
{
  tort_v io;

  tort_runtime_create();

  io = tort_stdout;

  printf("\nmtable:");
  tort_send(tort_s(_inspect), tort_(m_mtable), io);

  printf("\nroot:");
  tort_send(tort_s(_inspect), tort_(root), io);

  printf("\nDONE\n");

  return 0;
}

