#include "tort/core.h"

tort_v tort_runtime_initialize_method()
{
#define tort_d_m(MT, S, F) \
  _tort_m_mtable__add_method(tort_thread_arg MT, S, tort_i(F)); 
#include "tort/d_m.h"

  /* force references for extensions. */
  (void) tort__mt(block);

  return 0;
}

