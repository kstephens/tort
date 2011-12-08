#include "tort/tort.h"
#include <stdarg.h>

const char * _tort_if_stmt(tort_v x)
{
  if ( x != tort_false ) {
    printf("true %p\n", x);
  } else {
    printf("false %p\n", x);
  }
  return x;
}

