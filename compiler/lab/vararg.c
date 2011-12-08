#include "tort/tort.h"
#include <stdarg.h>

tort_v _tort_rest_arg(int n, va_list *vap)
{
  tort_v l = tort_nil, *lp = &l;
  extern tort_v tort_cons(tort_v, tort_v);
  while ( n -- > 0 ) {
    *lp = tort_cons(va_arg(*vap, tort_v), tort_nil);
    lp = ((tort_v*) lp) + 1; /* car + 0, cdr + 1 */
  }
  return l;
}

int _tort_argc;
tort_v _tort_vararg_1(tort_v a1, ...)
{
  va_list val;
  tort_v l;
  va_start(val, a1);
  l = _tort_rest_arg(_tort_argc - 1, &val);
  va_end(val);
  return l;
}

tort_v _tort_vararg_2(tort_v a1, tort_v a2, ...)
{
  va_list val;
  tort_v l;
  va_start(val, a2);
  l = _tort_rest_arg(_tort_argc - 2, &val);
  va_end(val);
  return l;
}

tort_v _tort_vararg_3(tort_v a1, tort_v a2, tort_v a3, ...)
{
  va_list val;
  tort_v l;
  va_start(val, a3);
  l = _tort_rest_arg(_tort_argc - 3, &val);
  va_end(val);
  return l;
}

