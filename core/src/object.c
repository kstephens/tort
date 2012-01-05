#include "tort/tort.h"

tort_v _tort_m_object__not (tort_tp tort_v rcvr)
{
  return rcvr == tort_false ? rcvr : tort_true;
}

tort_v _tort_m_object___mtable (tort_tp tort_v rcvr)
{
  return tort_h_mtable(rcvr);
}

tort_v _tort_m_object___mtableSET (tort_tp tort_v rcvr, tort_v mtable)
{
  tort_h_mtable(rcvr) = mtable;
  return rcvr;
}

tort_v _tort_m_object__clone (tort_tp tort_v rcvr)
{
  tort_mtable *mtable = tort_h_mtable(rcvr);
  void *ptr = tort_object_alloc(mtable, mtable->instance_size);
  tort_h(ptr)->applyf = tort_h(rcvr)->applyf;
  return memcpy(ptr, rcvr, mtable->instance_size);
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

tort_v _tort_m_object__initialize (tort_tp tort_v o)
{
  return o;
}

tort_v _tort_M_object__new (tort_tp tort_mtable *mtable)
{
  tort_v o = tort_send(tort__s(allocate), mtable);
  return_tort_send(tort__s(initialize), o);
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
  return tort_allocate(tort__mt(object), sizeof(tort_object));
}

#undef tort_v_mtable
tort_mtable *tort_v_mtable(tort_v x)
{
  return tort_h_mtable(x);
}

