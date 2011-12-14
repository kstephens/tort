#include "tort/tort.h"

#define EQUALQ(X,Y) (tort_sendn(tort__s(equalQ), 2, (X), (Y)) != tort_false)
#define return_EQUALQ(X,Y) return_tort_sendn(tort__s(equalQ), 2, (X), (Y))

tort_v _tort_m_object__eqQ (tort_tp tort_v rcvr, tort_v val)
{
  return rcvr == val ? tort_true : tort_false;
}

tort_v _tort_m_word__eqvQ (tort_tp tort_ptr *rcvr, tort_ptr *val)
{
  if ( rcvr == val ) return tort_true;
  if ( tort_h_mtable(rcvr) != tort_h_mtable(val) ) return tort_false;
  return tort_W(rcvr) == tort_W(val) ? tort_true : tort_false;
}

tort_v _tort_m_vector_base__equalQ (tort_tp tort_vector_base *rcvr, tort_vector_base *val)
{
  if ( rcvr == val ) return tort_true;
  if ( tort_h_mtable(rcvr) != tort_h_mtable(val) ) return tort_false;
  if ( rcvr->element_size != val->element_size ) return tort_false;
  if ( rcvr->size != val->size ) return tort_false;
  return memcmp(rcvr->data, val->data, rcvr->element_size * rcvr->size) == 0 ? tort_true : tort_false;
}

tort_v _tort_m_vector__equalQ (tort_tp tort_vector *rcvr, tort_vector *val)
{
  if ( rcvr == val ) return tort_true;
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
  if ( rcvr == val ) return tort_true;
  if ( tort_h_mtable(rcvr) != tort_h_mtable(val) ) return tort_false;
  if ( ! EQUALQ(rcvr->first, val->first) ) return tort_false;
  return_EQUALQ(rcvr->second, val->second);
}

tort_v _tort_m_map__equalQ (tort_tp tort_map *rcvr, tort_map *val)
{
  if ( rcvr == val ) return tort_true;
  if ( tort_h_mtable(rcvr) != tort_h_mtable(val) ) return tort_false;
  if ( tort_map_size(rcvr) != tort_map_size(val) ) return tort_false;
  if ( rcvr->equality != val->equality ) return tort_false;
  tort_map_EACH(rcvr, e) {
    tort_pair *e2 = tort_send(tort__s(get_entry), val, e->first);
    if ( ! (e2 && EQUALQ(e->second, e2->second)) ) return tort_false;
  } tort_map_EACH_END();
  return tort_true; // NOT TAIL-RECURSIVE
}

#undef EQUALQ
#undef return_EQUALQ

tort_v tort_runtime_initialize_eq()
{
  tort_send(tort__s(alias_method), tort__mt(object), tort__s(eqvQ),   tort__s(eqQ));
  tort_send(tort__s(alias_method), tort__mt(object), tort__s(equalQ), tort__s(eqQ));
  tort_send(tort__s(alias_method), tort__mt(word),   tort__s(equalQ), tort__s(eqvQ));
  return 0;
}
