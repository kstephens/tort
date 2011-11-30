#include "tort/core.h"

#ifdef tort_l
#undef tort_l
#define tort_l(P) _tort_l(P)
#endif
tort_v tort_locative_new(tort_v *vp)
{
  assert(tort_L(tort_l(vp)) == vp);
  // fprintf(stderr, "  tort_l(%p) => %p\n", vp, tort_l(vp));
  return tort_l(vp);
}

tort_v _tort_m_locative__value(tort_tp tort_v l)
{
  return *tort_L(l);
}

tort_v _tort_m_locative__valueSET(tort_tp tort_v l, tort_v v)
{
  *tort_L(l) = v;
  return l;
}

tort_v _tort_m_locative___ptr_data(tort_tp tort_v l, void **pptr)
{
  *pptr = tort_L(l);
  return l;
}

tort_v _tort_m_locative___ptr_object(tort_tp tort_v l, void **pptr)
{
  *pptr = tort_L(l);
  return l;
}

tort_v _tort_m_locative___to_string(tort_tp tort_v l)
{
  char buf[64];
  snprintf(buf, sizeof(buf) - 1, "%llx", (unsigned long long) (size_t) tort_L(l));
  return tort_string_new(buf, strlen(buf));
}

/* Behaves as method. */
tort_v _tort_m_locative___applyf(tort_tp tort_v v)
{
  tort_v l = _tort_message->method;
  assert(tort_h_mtable(l) == tort__mt(locative));
  return *tort_L(l);
}

tort_v _tort_m_object___slot_locative_at (tort_tp tort_v rcvr, tort_v offset)
{
  return tort_l(&((tort_v*) rcvr)[tort_I(offset)]);
}
