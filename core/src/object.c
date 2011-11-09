#include "tort/tort.h"
#include <assert.h>

tort_v _tort_m_object__not (tort_tp tort_v rcvr)
{
  return rcvr == tort_false ? rcvr : tort_true;
}

tort_v _tort_m_object___object_header_size (tort_tp tort_v rcvr)
{
  return tort_i(sizeof(tort_header));
}

tort_v _tort_m_object___mtable (tort_tp tort_v rcvr)
{
  return tort_h(rcvr)->mtable;
}

tort_v _tort_m_object___set_mtable (tort_tp tort_v rcvr, tort_v mtable)
{
  assert(rcvr != tort_nil);      // dont change nil's mtable.
  assert(! tort_taggedQ(rcvr));  // dont change any tagged object's mtable, they are all constants!
  tort_h(rcvr)->mtable = mtable;
  return rcvr;
}

tort_v _tort_m_object___alloc_size (tort_tp tort_v rcvr)
{
  return tort_i(tort_h(rcvr)->alloc_size);
}

tort_v _tort_m_object__clone (tort_tp tort_v rcvr)
{
  size_t alloc_size = sizeof(tort_header) + tort_h(rcvr)->alloc_size;
  void *ptr = tort_malloc(alloc_size);
  memcpy(ptr, tort_h(rcvr), alloc_size);
  ptr += sizeof(tort_header);
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

tort_v tort_object_new()
{
  tort_v obj = tort_allocate(tort__mt(object), sizeof(tort_object));
  return obj;
}

