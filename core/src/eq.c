#include "tort/tort.h"
#include <assert.h>

#define EQUALQ(X,Y) (tort_sendn(tort__s(equalQ), 2, (X), (Y)) != tort_false)
#define return_EQUALQ(X,Y) return_tort_sendn(tort__s(equalQ), 2, (X), (Y))
tort_v _tort_m_object__eqQ (tort_tp tort_v rcvr, tort_v val)
{
  return rcvr == val ? tort_true : tort_false;
}

tort_v _tort_m_object__equalQ (tort_tp tort_v rcvr, tort_v val)
{
  return rcvr == val ? tort_true : tort_false;
}

tort_v _tort_m_vector_base__equalQ (tort_tp tort_vector_base *rcvr, tort_vector_base *val)
{
  if ( tort_h_mtable(rcvr) != tort_h_mtable(val) ) return tort_false;
  if ( rcvr->element_size != val->element_size ) return tort_false;
  if ( rcvr->size != val->size ) return tort_false;
  return memcmp(rcvr->data, val->data, rcvr->element_size * rcvr->size) == 0 ? tort_true : tort_false;
}

tort_v _tort_m_vector__equalQ (tort_tp tort_vector *rcvr, tort_vector *val)
{
  if ( tort_h_mtable(rcvr) != tort_h_mtable(val) ) return tort_false;
  if ( rcvr->element_size != val->element_size ) return tort_false;
  if ( rcvr->size != val->size ) return tort_false;
  tort_vector_loop(rcvr, x) {
    if ( ! EQUALQ(x, tort_vector_data(val)[x_i]) ) return tort_false;
  } tort_vector_loop_end(rcvr);
  return tort_true; // NOT TAIL-RECURSIVE
}

tort_v _tort_m_pair__equalQ (tort_tp tort_pair *rcvr, tort_pair *val)
{
  if ( tort_h_mtable(rcvr) != tort_h_mtable(val) ) return tort_false;
  if ( ! EQUALQ(rcvr->first, val->first) ) return tort_false;
  return_EQUALQ(rcvr->second, val->second);
}

#undef EQUALQ
#undef return_EQUALQ
