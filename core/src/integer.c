/* -*- c -*- */

#include "tort/tort.h"

#define UOP(N,OP) tort_v _tort_m_tagged__##N(tort_tp tort_v a);
#define LUP(N,OP) UOP(N,OP)
#define BOP(N,OP) tort_v _tort_m_tagged__##N(tort_tp tort_v a, tort_v b);
#define ROP(N,OP) BOP(N,OP)
#define LOP(N,OP) BOP(N,OP)
#include "tort/ops.h"

#define UOP(N,OP)						   \
  tort_v _tort_m_tagged__##N(tort_tp tort_v a)			   \
  { return tort_i(OP tort_I(a)); } 
#define LUP(N,OP)						    \
  tort_v _tort_m_tagged__##N(tort_tp tort_v a)			    \
  { return (OP tort_I(a)) ? tort_true : tort_false; } 
#define BOP(N,OP)						     \
  tort_v _tort_m_tagged__##N(tort_tp tort_v a, tort_v b)	     \
  { return tort_i(tort_I(a) OP tort_I(b)); }
#define ROP(N,OP)						    \
  tort_v _tort_m_tagged__##N(tort_tp tort_v a, tort_v b)	    \
  { return (tort_I(a) OP tort_I(b)) ? tort_true : tort_false; } 
#define LOP(N,OP) ROP(N,OP)
#include "tort/ops.h"

