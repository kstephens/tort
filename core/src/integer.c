/* -*- c -*- */

#include "tort/tort.h"

#define UOP(N,OP) tort_v _tort_m_fixnum__##N(tort_tp tort_v a);
#define LUP(N,OP) UOP(N,OP)
#define BOP(N,OP) tort_v _tort_m_fixnum__##N(tort_tp tort_v a, tort_v b);
#define ROP(N,OP) BOP(N,OP)
#define LOP(N,OP) BOP(N,OP)
#include "tort/ops.h"

#define UOP(N,OP)						   \
  tort_v _tort_m_fixnum__##N(tort_tp tort_v a)			   \
  { return tort_i(OP tort_I(a)); } 
#define LUP(N,OP)						    \
  tort_v _tort_m_fixnum__##N(tort_tp tort_v a)			    \
  { return (OP tort_I(a)) ? tort_true : tort_false; } 
#define BOP(N,OP)						     \
  tort_v _tort_m_fixnum__##N(tort_tp tort_v a, tort_v b)	     \
  { return tort_i(tort_I(a) OP tort_I(b)); }
#define ROP(N,OP)						    \
  tort_v _tort_m_fixnum__##N(tort_tp tort_v a, tort_v b)	    \
  { return (tort_I(a) OP tort_I(b)) ? tort_true : tort_false; } 
#define LOP(N,OP) ROP(N,OP)
#include "tort/ops.h"

tort_v _tort_M_fixnum___ops(tort_tp tort_mtable *mtable)
{
  tort_v result = tort_vector_new(0, 0);
  tort_v op[4];

#define X(NA,N,OP,K) op[2] = tort_i(NA); op[3] = tort_s(K); op[1] = tort_symbol_new(#N); op[0] = tort_s(N); tort_send(tort_s(add), result, tort_vector_new(op, 4));
#define UOP(N,OP) X(1,N,OP,uop)
#define BOP(N,OP) X(2,N,OP,bop)
#define LUP(N,OP) X(2,N,OP,lup)
#define LOP(N,OP) X(2,N,OP,lop)
#define ROP(N,OP) X(2,N,OP,rop)
#include "tort/ops.h"
  return result;
}

