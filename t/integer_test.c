#include "tort/tort.h"
#include <stdio.h>

int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v a, b;

  tort_runtime_create();

  io = tort_stdout;
  a = tort_i(123);
  b = tort_i(4);

#define UOP(N,OP) \
  tort_printf(io, "\n %s %lld => %lld\n", #OP, (long long) tort_I(a), (long long) tort_I(tort_send(tort__s(N), a)));

#define BOP(N,OP) \
  tort_printf(io, "\n %lld %s %lld => %lld\n", (long long) tort_I(a), #OP, (long long) tort_I(b), (long long) tort_I(tort_send(tort__s(N), a, b)));

#define ROP(N,OP) \
  tort_printf(io, "\n %lld %s %lld => %lld\n", (long long) tort_I(a), #OP, (long long) tort_I(b), (long long) tort_I(tort_send(tort__s(N), a, b)));

#include "tort/ops.h"

  printf("\nDONE\n");

  return 0;
}

