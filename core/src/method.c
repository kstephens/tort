#include "tort/core.h"

tort_method* tort_method_make(void *applyf)
{
  tort_method *meth = tort_allocate(tort__mt(method), sizeof(tort_method));
  meth->applyf = applyf;
  meth->data = 0;
  meth->name = 0;
  return meth;
}

tort_v tort_runtime_initialize_method()
{
#define tort_d_m(MT, S, F) \
  tort_send(tort__s(add_method), MT, S, tort_method_make(F)); 
#include "tort/d_m.h"

  return 0;
}

