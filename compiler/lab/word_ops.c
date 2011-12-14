#include "tort/tort.h"

#define UOP(N,OP) \
  tort_vi _tort_##N(tort_vi a) { return OP a; }
#define BOP(N,OP) \
  tort_vi _tort_##N(tort_vi a, tort_vi b) { return a OP b; }
#define LUP(N,OP)UOP(N,OP)
#define LOP(N,OP)BOP(N,OP)
#define ROP(N,OP)BOP(N,OP)

#include "tort/ops.h"

