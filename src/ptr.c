#include "tort/core.h"

tort_v tort_ptr_new(void *ptr)
{
  tort_v val = tort_allocate(tort__mt(ptr), sizeof(tort_ptr));
  tort_P(val) = ptr;
  return val;
}

tort_v _tort_m_ptr___ptr_data(tort_tp tort_v p, void **pptr)
{
  *pptr = tort_P(p);
  return p;
}

void *tort_ptr_data(tort_v v)
{
  void *ptr = 0;
  tort_send(tort__s(_ptr_data), v, &ptr);
  return ptr;
}

#define ROP(N,OP) tort_v _tort_m_ptr__##N(tort_tp tort_v a, tort_v b);
#define LOP(N,OP) ROP(N,OP)
#define LUP(N,OP) tort_v _tort_m_ptr__##N(tort_tp tort_v a);
#include "tort/ops.h"

#define ROP(N,OP)							\
  tort_v _tort_m_ptr__##N(tort_tp tort_v a, tort_v b)			\
  { return (tort_P(a) OP tort_ptr_data(b)) ? tort_true : tort_false; } 
#define LOP(N,OP) ROP(N,OP)
#define LUP(N,OP)						    \
  tort_v _tort_m_ptr__##N(tort_tp tort_v a)			    \
  { return (OP tort_P(a)) ? tort_true : tort_false; } 

#include "tort/ops.h"
