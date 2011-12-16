#include "tort/core.h"

tort_v _tort_M_pair__new(tort_tp tort_mtable *mtable, tort_v first, tort_v second)
{
  tort_pair *e = tort_allocate(mtable, sizeof(tort_pair));
  e->first = first;
  e->second = second;
  return e;
}
tort_SLOT(pair,tort_v,first);
tort_SLOT(pair,tort_v,second);
