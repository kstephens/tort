#include "tort/tort.h"

tort_v _tort_m_symbol___sendv(tort_tp tort_v sel, const tort_v *a, size_t n)
{
  switch ( n ) {
  case 0:
    return_tort_sendn(sel, 0, tort_nil);
  case 1:
    return_tort_sendn(sel, n, a[0]);
  case 2:
    return_tort_sendn(sel, n, a[0], a[1]);
  case 3:
    return_tort_sendn(sel, n, a[0], a[1], a[2]);
  case 4:
    return_tort_sendn(sel, n, a[0], a[1], a[2], a[3]);
  case 5:
    return_tort_sendn(sel, n, a[0], a[1], a[2], a[3], a[4]);
  case 6:
    return_tort_sendn(sel, n, a[0], a[1], a[2], a[3], a[4], a[5]);
  case 7:
    return_tort_sendn(sel, n, a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
  case 8:
    return_tort_sendn(sel, n, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
  case 9:
    return_tort_sendn(sel, n, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
  case 10:
    return_tort_sendn(sel, n, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9]);
  default: 
    abort();
  }
  return tort_nil;
}

