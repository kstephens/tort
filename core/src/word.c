#include "tort/core.h"

tort_v tort_word_new(tort_vi word)
{
  tort_v val = tort_allocate(tort__mt(word), sizeof(tort_word));
  tort_W(val) = word;
  return val;
}

tort_v _tort_m_word___value(tort_tp tort_v p)
{
  return (tort_v) tort_W(p);
}

tort_v _tort_m_object___word(tort_tp tort_v p)
{
  return tort_ptr_new((void*) p);
}

tort_v _tort_m_tagged___word(tort_tp tort_v p)
{
  return tort_word_new((tort_vi) p);
}

tort_v _tort_m_word___word(tort_tp tort_v p)
{
  return p;
}

tort_v _tort_m_ptr___word(tort_tp tort_v p)
{
  return p;
}

tort_v _tort_m_object___word_data(tort_tp tort_v p, tort_vi *pword)
{
  *pword = (tort_vi) p;
  return p;
}

tort_v _tort_m_word___word_data(tort_tp tort_v p, tort_vi *pword)
{
  *pword = tort_W(p);
  return p;
}

tort_v _tort_m_word___to_string(tort_tp tort_v p)
{
  char buf[64];
  snprintf(buf, sizeof(buf) - 1, "%llx", (unsigned long long) tort_W(p));
  return tort_string_new(buf, strlen(buf));
}

tort_vi tort_word_data(tort_v v)
{
  tort_vi word = 0;
  tort_send(tort__s(_word_data), v, &word);
  return word;
}

#define UOP(N,OP) tort_v _tort_m_word__##N(tort_tp tort_v a);
#define BOP(N,OP) tort_v _tort_m_word__##N(tort_tp tort_v a, tort_v b);
#define ROP(N,OP) tort_v _tort_m_word__##N(tort_tp tort_v a, tort_v b);
#define LOP(N,OP) ROP(N,OP)
#define LUP(N,OP) tort_v _tort_m_word__##N(tort_tp tort_v a);
#include "tort/ops.h"

#define UOP(N,OP)							\
  tort_v _tort_m_word__##N(tort_tp tort_v a)				\
  { return tort_w(OP tort_W(a)); } 
#define BOP(N,OP)							\
  tort_v _tort_m_word__##N(tort_tp tort_v a, tort_v b)			\
  { return tort_w(tort_W(a) OP tort_word_data(b)); } 
#define ROP(N,OP)							\
  tort_v _tort_m_word__##N(tort_tp tort_v a, tort_v b)			\
  { return (tort_W(a) OP tort_word_data(b)) ? tort_true : tort_false; } 
#define LOP(N,OP) ROP(N,OP)
#define LUP(N,OP)						    \
  tort_v _tort_m_word__##N(tort_tp tort_v a)			    \
  { return (OP tort_P(a)) ? tort_true : tort_false; } 
#include "tort/ops.h"

