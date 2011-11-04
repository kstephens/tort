#include "tort/core.h"

tort_v _tort_m_method___gc_mark(tort_tp tort_method *o)
{
  // tort_gc_mark(o, o->applyf); // when compiler is on-line.
  tort_gc_mark(o, o->name);
  return o->data;
}

tort_v _tort_m_method___gc_free(tort_tp tort_method *o)
{
  return 0;
}
tort_ACCESSOR(method,voidP,applyf);
tort_ACCESSOR(method,tort_v,name);
tort_ACCESSOR(method,tort_v,data);

tort_method* tort_method_make(void *applyf)
{
  tort_method *meth = tort_allocate(tort__mt(method), sizeof(tort_method));
  meth->applyf = applyf;
  meth->name = 0;
  meth->data = 0;
  return meth;
}

tort_v tort_runtime_initialize_method()
{
#define tort_d_m(MT, S, F) \
  tort_send(tort__s(add_method), MT, S, tort_method_make(F)); 
#include "tort/d_m.h"
  return 0;
}

