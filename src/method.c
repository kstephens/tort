#include "tort/core.h"

tort_method* tort_method_make(tort_apply_decl((*applyf)))
{
  tort_method *meth = tort_allocate(tort__mt(method), sizeof(tort_method));
  meth->name = 0;
  tort_h_ref(meth)->applyf = applyf;
  return meth;
}


tort_v tort_runtime_initialize_method()
{
#define tort_d_m(MT, S, F) \
  _tort_m_mtable__add_method(tort_thread_arg MT, S, tort_i(F)); 
#include "tort/d_m.h"

  return 0;
}

