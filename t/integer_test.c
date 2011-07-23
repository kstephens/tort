#include "tort/tort.h"
#include <stdio.h>

int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v a, b, c, d, e;

  tort_runtime_create();

  io = tort_stdout;
  a = tort_i(123);
  b = tort_i(4);
  c = tort_i(-123);
  d = tort_i(0);
  e = tort_i(1);

#define UOP(N,OP) \
  tort_printf(io, "\n %s %lld => tort_send(tort__s(%s), %p) => %lld\n", #OP, (long long) tort_I(a), #N, (void*) a, (long long) tort_I(tort_send(tort__s(N), a))); \
  tort_printf(io, "\n %s %lld => tort_send(tort__s(%s), %p) => %lld\n", #OP, (long long) tort_I(b), #N, (void*) b, (long long) tort_I(tort_send(tort__s(N), b))); \
  tort_printf(io, "\n %s %lld => tort_send(tort__s(%s), %p) => %lld\n", #OP, (long long) tort_I(c), #N, (void*) c, (long long) tort_I(tort_send(tort__s(N), c)));

#define BOP(N,OP) \
  tort_printf(io, "\n %lld %s %lld => %lld\n", (long long) tort_I(a), #OP, (long long) tort_I(b), (long long) tort_I(tort_send(tort__s(N), a, b)));

#define _LUP(N,OP,a)							\
  tort_printf(io, "\n %s %lld => %T\n", #OP, (long long) tort_I(a), tort_send(tort__s(N), a))
#define LUP(N,OP) \
  _LUP(N,OP,a); \
  _LUP(N,OP,d); \
  _LUP(N,OP,e);

#define _ROP(N,OP,a,b)							\
  tort_printf(io, "\n %lld %s %lld => %T\n", (long long) tort_I(a), #OP, (long long) tort_I(b), tort_send(tort__s(N), a, b))
#define ROP(N,OP) \
  _ROP(N,OP,a,b); \
  _ROP(N,OP,d,d); \
  _ROP(N,OP,d,e); \
  _ROP(N,OP,e,e); 

#define LOP(N,OP) ROP(N,OP)

#include "tort/ops.h"

  printf("\nDONE\n");

  return 0;
}

