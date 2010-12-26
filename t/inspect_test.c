#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>


int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v v;

  tort_runtime_create();

  io = tort_stdout;

#define P(X) \
  tort_printf(io, "\n  %s => %T\n", #X, v = X); \
  tort_printf(io, "  tort_h_mtable(%s) => %T\n", #X, tort_h_mtable(v))

  P(tort_nil);
  P(tort_true);
  P(tort_false);
  P(tort_eos);
  P(tort_i(123));
  P(tort_string_new_cstr("cstring"));
  P(tort__s(new));
  P(tort_object_make());

  P(_tort);
  P(tort_(root));

#undef P

  printf("\nDONE\n");

  return 0;
}

