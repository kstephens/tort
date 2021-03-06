#include "tort/tort.h"

tort_v _tort_m_object___to_literal(tort_tp tort_v p)
{
  char buf[64];
  snprintf(buf, sizeof(buf) - 1, "0x%llx", (unsigned long long) (size_t) p);
  return tort_string_new(buf, strlen(buf));
}

tort_v _tort_m_tagged___to_literal(tort_tp tort_v p)
{
  char buf[64];
  snprintf(buf, sizeof(buf) - 1, "%lld", (long long) (ssize_t) p);
  return tort_string_new(buf, strlen(buf));
}

tort_v _tort_m_word___to_literal(tort_tp tort_v p)
{
  char buf[64];
  snprintf(buf, sizeof(buf) - 1, "%lld", (unsigned long long) tort_W(p));
  return tort_string_new(buf, strlen(buf));
}

tort_v _tort_m_ptr___to_literal(tort_tp tort_v p)
{
  char buf[64];
  snprintf(buf, sizeof(buf) - 1, "0x%llx", (unsigned long long) (size_t) tort_P(p));
  return tort_string_new(buf, strlen(buf));
}

tort_v _tort_m_object___to_c_ptr(tort_tp tort_v p)
{
  return tort_p(p);
}
tort_v _tort_m_fixnum___to_c_ptr(tort_tp tort_v p)
{
  return tort_p((void*) tort_I(p));
}
tort_v _tort_m_ptr___to_c_ptr(tort_tp tort_v p)
{
  return p;
}
tort_v _tort_m_locative___to_c_ptr(tort_tp tort_v p)
{
  return tort_p(tort_L(p));
}
tort_v _tort_m_vector_base___to_c_ptr(tort_tp tort_vector *p)
{
  return tort_p(p->data);
}
tort_v _tort_m_nil___to_c_ptr(tort_tp tort_v p)
{
  return tort_p((void*) 0);
}


