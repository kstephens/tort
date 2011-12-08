#include "tort/tort.h"
#include <stdarg.h>

tort_v _tort_plain_c(void *arg0, tort_v arg1, tort_v arg2, tort_v arg3, tort_v arg4, tort_v arg5, tort_v arg6)
{
  extern void a7(void *arg0, tort_v arg1, tort_v arg2, tort_v arg3, tort_v arg4, tort_v arg5, tort_v arg6);
  extern void ip(long long arg0, void *arg1);
  extern void pp(void *arg0, void *arg1);
  a7(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
  ip(0, arg0);
  ip(1, arg1);
  ip(2, arg2);
  ip(3, arg3);
  ip(4, arg4);
  ip(6, arg6);
  pp((void*) 0, arg0);
  pp((void*) 1, arg1);
  pp((void*) 2, arg2);
  pp((void*) 3, arg3);
  pp((void*) 4, arg4);
  pp((void*) 5, arg5);
  pp((void*) 6, arg6);
  return arg0;
}

