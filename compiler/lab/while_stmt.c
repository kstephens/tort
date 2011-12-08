#include "tort/tort.h"
#include <stdarg.h>

const char * _tort_while_stmt(tort_pair *x)
{
  while ( x != tort_false ) {
    printf("true %p\n", x);
    x = x->second;
  }
  return (char*) x;
}

