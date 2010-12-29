#include "tort/core.h"

#include <stdio.h>

int main(int argc, char **argv)
{
#define T(X) \
  printf("%s => %s\n", #X, tort_symbol_encode(#X))
  T(_);
  T(ADDADD);
  T(DOT);
  T(MOD);
  T(NEG);
  T(SUB);
  T(NOT);
  T(set_carE);
  T(asdfasd);
  T(string_TO_symbol);
  T(__fooE);
  T(PPfooQ);

  return 0;
}

