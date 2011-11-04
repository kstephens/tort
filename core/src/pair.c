#include "tort/core.h"

tort_v _tort_M_pair__new(tort_tp tort_mtable *mtable, tort_v first, tort_v second)
{
  tort_pair *e = tort_allocate(mtable, sizeof(tort_pair));
  e->first = first;
  e->second = second;
  return e;
}
tort_GETTER(pair,tort_v,first);
tort_SETTER(pair,tort_v,first);
tort_GETTER(pair,tort_v,second);
tort_SETTER(pair,tort_v,second);

tort_v _tort_m_pair___gc_mark(tort_tp tort_pair *o)
{
  tort_gc_mark(o, o->first);
  return o->second;
}

tort_v _tort_m_pair___gc_free(tort_tp tort_pair *o)
{
  return 0;
}
