#include "tort/tort.h"
#include <assert.h>

tort_v _tort_m_object__not (tort_tp tort_v rcvr)
{
  return rcvr == tort_false ? rcvr : tort_true;
}

tort_v _tort_m_object___mtable (tort_tp tort_v rcvr)
{
  return tort_h(rcvr)->mtable;
}

tort_v _tort_m_object___set_mtable (tort_tp tort_v rcvr, tort_v mtable)
{
  tort_h(rcvr)->mtable = mtable;
  return rcvr;
}

tort_v _tort_m_object__clone (tort_tp tort_v rcvr)
{
  size_t alloc_size = tort_h(rcvr)->mtable->instance_size;
  void *ptr = tort_object_alloc(tort_h(rcvr)->mtable, alloc_size);
  // tort_h(ptr)->applyf = tort_h(rcvr)->applyf;
  memcpy(ptr, rcvr, alloc_size);
  return ptr;
}

tort_v _tort_m_object__identity (tort_tp tort_v rcvr)
{
  return rcvr;
}

tort_v _tort_m_object___slot_at (tort_tp tort_v rcvr, tort_v offset)
{
  return ((tort_v*) rcvr)[tort_I(offset)];
}

tort_v _tort_m_object___set_slot_at (tort_tp tort_v rcvr, tort_v offset, tort_v value)
{
  return ((tort_v*) rcvr)[tort_I(offset)] = value;
}

tort_v _tort_m_object___gc_mark(tort_tp tort_v o)
{
  // Avoid sends to objects that do not have a _gc_mark method.  See gc.c.
  tort_h_mtable(o)->gc_mark_method = tort_true;
  return 0;
}

tort_v _tort_m_object___gc_free(tort_tp tort_v o)
{
  // Avoid sends to objects that do not have a _gc_free method.  See gc.c.
  tort_h_mtable(o)->gc_free_method = tort_true;
  return 0;
}

tort_v tort_object_new()
{
  tort_v obj = tort_allocate(tort__mt(object), sizeof(tort_object));
  return obj;
}

