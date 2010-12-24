#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>


int main(int argc, char **argv, char **environ)
{
  tort_v io;

  tort_runtime_create();

  io = tort_stdout;

  printf("\nclass:");
  tort_send(tort_s(_inspect), _tort->_m_class, io);

  printf("\nroot:");
  tort_send(tort_s(_inspect), _tort->root, io);

  printf("\nDONE\n");

  return 0;
}

