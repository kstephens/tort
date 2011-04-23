#include "tort/tort.h"
#include <assert.h>

tort_v _tort_m_object__eqQ (tort_tp tort_v rcvr, tort_v val)
{
  return rcvr == val ? tort_true : tort_false;
}

tort_v _tort_m_object__not (tort_tp tort_v rcvr)
{
  return rcvr == tort_false ? rcvr : tort_true;
}

tort_v _tort_m_object___mtable (tort_thread_param tort_v rcvr)
{
  return tort_h_ref(rcvr)->mtable;
}

tort_v _tort_m_object___alloc_size (tort_thread_param tort_v rcvr)
{
  return tort_i(tort_h_ref(rcvr)->alloc_size);
}

tort_v _tort_m_object__clone (tort_thread_param tort_v rcvr)
{
  size_t alloc_size = sizeof(tort_header) + tort_h_ref(rcvr)->alloc_size;
  void *ptr = tort_malloc(alloc_size);
  memcpy(ptr, tort_h_ref(rcvr), alloc_size);
  ptr += sizeof(tort_header);
  return ptr;
}

tort_v _tort_m_object__identity (tort_thread_param tort_v rcvr)
{
  return rcvr;
}

/********************************************************************/

tort_v tort_object_make()
{
  tort_v obj = tort_allocate(tort__mt(object), sizeof(tort_object));
  return obj;
}


