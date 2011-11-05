#include "tort/core.h"

tort_ACCESSOR(method,voidP,applyf);
tort_ACCESSOR(method,tort_v,name);
tort_ACCESSOR(method,tort_v,data);

tort_method* tort_method_make(void *applyf, tort_v data)
{
  tort_method *meth = tort_allocate(tort__mt(method), sizeof(tort_method));
  meth->applyf = applyf;
  meth->data = data;
  meth->name = 0;
  return meth;
}

tort_v tort_runtime_initialize_method()
{
#define tort_d_m(MT, S, F) \
  tort_send(tort__s(add_method), MT, S, tort_method_make(F, 0)); 
#include "tort/d_m.h"
  return 0;
}

