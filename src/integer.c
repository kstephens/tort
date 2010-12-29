/* -*- c -*- */

#include "tort/tort.h"

#define UOP(N,OP) tort_v _tort_m_tagged__##N(tort_thread_param tort_v a);
#define BOP(N,OP) tort_v _tort_m_tagged__##N(tort_thread_param tort_v a, tort_v b);
#define ROP(N,OP) tort_v _tort_m_tagged__##N(tort_thread_param tort_v a, tort_v b);
#include "tort/ops.h"

#define UOP(N,OP)						   \
  tort_v _tort_m_tagged__##N(tort_thread_param tort_v a)	   \
  {								   \
    return tort_i(OP tort_I(a));				   \
  } 

#define BOP(N,OP)						     \
  tort_v _tort_m_tagged__##N(tort_thread_param tort_v a, tort_v b)   \
  {								     \
    return tort_i(tort_I(a) OP tort_I(b));			     \
  } 

#define ROP(N,OP)						    \
  tort_v _tort_m_tagged__##N(tort_thread_param tort_v a, tort_v b)   \
  {								    \
    return tort_i(tort_I(a) OP tort_I(b));		   \
  } 

#include "tort/ops.h"
