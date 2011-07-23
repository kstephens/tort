#include "tort/tort.h"
#include <stdio.h>

int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v v;

  tort_runtime_create();

  io = tort_stdout;

  tort_printf(io, "  tort_i(123) => %lld\n", (long long) (ssize_t) (v = tort_i(123)));
  tort_printf(io, "  tort_I(%lld) => %lld\n", (long long) (ssize_t) v, (long long) tort_I(v));

  printf("\nDONE\n");

  return 0;
}

