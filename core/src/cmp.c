#include "tort/tort.h"

#define SGN(X) tort_i((X) < 0 ? -1 : (X) == 0 ? 0 : 1)
#define CMP(X,Y) tort_I(CMP_v = tort_sendn(tort__s(CMP), 2, (X), (Y)))
#define eqCMP(X,Y) tort_I(CMP_v = tort_sendn(tort__s(eqCMP), 2, (X), (Y)))
#define return_CMP(X,Y) return_tort_sendn(tort__s(CMP), 2, (X), (Y))

tort_v _tort_m_object__CMP (tort_tp tort_v rcvr, tort_v val)
{
  return tort_i(rcvr < val ? -1 : rcvr == val ? 0 : 1);
}

tort_v _tort_m_word__CMP (tort_tp tort_ptr *rcvr, tort_ptr *val)
{
  tort_v CMP_v;
  if ( rcvr == val ) return tort_i(0);
  if ( eqCMP(tort_h_mtable(rcvr), tort_h_mtable(val)) ) return CMP_v;
  return tort_i(tort_W(rcvr) < tort_W(val) ? -1 : tort_W(rcvr) == tort_W(val) ? 0 : 1);
}

tort_v _tort_m_fixnum__CMP (tort_tp tort_ptr *rcvr, tort_ptr *val)
{
  tort_v CMP_v;
  if ( eqCMP(tort_h_mtable(rcvr), tort_h_mtable(val)) ) return CMP_v;
  return tort_i(tort_I(rcvr) < tort_I(val) ? -1 : tort_I(rcvr) == tort_I(val) ? 0 : 1);
}

tort_v _tort_m_vector_base__CMP (tort_tp tort_vector_base *rcvr, tort_vector_base *val)
{
  ssize_t d; tort_v CMP_v;
  /* FIXME */
  if ( rcvr == val ) return tort_i(0);
  if ( eqCMP(tort_h_mtable(rcvr), tort_h_mtable(val)) ) return CMP_v;
  if ( (d = rcvr->element_size - val->element_size) ) return SGN(d);
  if ( (d = rcvr->size - val->size) ) return SGN(d);
  return tort_i(memcmp(rcvr->data, val->data, rcvr->element_size * rcvr->size));
}

tort_v _tort_m_vector__CMP (tort_tp tort_vector *rcvr, tort_vector *val)
{
  /* FIXME */
  ssize_t d; tort_v CMP_v;
  if ( rcvr == val ) return tort_i(0);
  if ( eqCMP(tort_h_mtable(rcvr), tort_h_mtable(val)) ) return CMP_v;
  if ( (d = rcvr->element_size - val->element_size) ) return SGN(d);
  if ( (d = rcvr->size - val->size) ) return SGN(d);
  tort_vector_loop(rcvr, x) {
    if ( CMP(x, tort_vector_data(val)[x_i]) ) return CMP_v;
  } tort_vector_loop_end(rcvr);
  return tort_i(0); // NOT TAIL-RECURSIVE
}

tort_v _tort_m_pair__CMP (tort_tp tort_pair *rcvr, tort_pair *val)
{
  tort_v CMP_v;
  if ( rcvr == val ) return tort_i(0);
  if ( eqCMP(tort_h_mtable(rcvr), tort_h_mtable(val)) ) return CMP_v;
  if ( CMP(rcvr->first, val->first) ) return CMP_v;
  return_CMP(rcvr->second, val->second);
}

tort_v _tort_m_map__CMP (tort_tp tort_map *rcvr, tort_map *val)
{
  tort_v CMP_v; ssize_t d;
  if ( rcvr == val ) return tort_i(0);
  if ( eqCMP(tort_h_mtable(rcvr), tort_h_mtable(val)) ) return CMP_v;
  if ( (d = tort_map_size(rcvr) - tort_map_size(val)) ) return SGN(d);
  if ( CMP(rcvr->equality, val->equality) ) return CMP_v;
  tort_map_EACH(rcvr, e) {
    tort_pair *e2 = tort_send(tort__s(get_entry), val, e->first);
    if ( ! e2 ) return tort_i(1);
    if ( CMP(e->second, e2->second) ) return CMP_v;
  } tort_map_EACH_END();
  return tort_i(0); // NOT TAIL-RECURSIVE
}

#undef CMP
#undef return_CMP

tort_v _tort_m_initializer__cmp(tort_tp tort_v init)
{
  tort_send(tort__s(alias_method), tort__mt(object), tort__s(eqCMP),   tort__s(CMP));
  tort_send(tort__s(alias_method), tort__mt(object), tort__s(eqvCMP),  tort__s(CMP));
  return init;
}
