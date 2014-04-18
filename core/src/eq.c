#include "tort/tort.h"

// http://en.wikipedia.org/wiki/Jenkins_hash_function
static
tort_vw jenkins_hash(tort_vw hash, unsigned char *key, size_t len)
{
  size_t i;
  for ( i = 0; i < len; ++ i ) {
    hash += key[i];
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);
  return hash;
}

tort_v _tort_m_object__hash_mix(tort_tp tort_v a, tort_v b)
{
  tort_vw r = (tort_vw) a + (tort_vw) b;
  r = jenkins_hash(r, (void*) &a, sizeof(a));
  r = jenkins_hash(r, (void*) &b, sizeof(b));
  return tort_i(r >> (TORT_TAG_BITS + 1));
}

#define EQUALQ(X,Y) (tort_sendn(tort__s(equalQ), 2, (X), (Y)) != tort_false)
#define return_EQUALQ(X,Y) return_tort_sendn(tort__s(equalQ), 2, (X), (Y))

tort_v _tort_m_object__eqQ (tort_tp tort_v rcvr, tort_v val)
{
  return rcvr == val ? tort_true : tort_false;
}

tort_v _tort_m_object__eqQ_hash (tort_tp tort_v rcvr)
{
#define HSH_INIT() tort_vw h = 0xfeedface
  HSH_INIT();
#define HSH(x) ({ tort_vw _tmp = (tort_vw) x; h = jenkins_hash(h, (void*) &(_tmp), sizeof(_tmp)); _tmp; })
  HSH(rcvr);
#define HSH_VAL() tort_i(h >> (TORT_TAG_BITS + 1))
  return HSH_VAL();
}

tort_v _tort_m_word__eqvQ (tort_tp tort_ptr *rcvr, tort_ptr *val)
{
  if ( rcvr == val ) return tort_true;
  if ( tort_h_mtable(rcvr) != tort_h_mtable(val) ) return tort_false;
  return tort_W(rcvr) == tort_W(val) ? tort_true : tort_false;
}

tort_v _tort_m_word__eqvQ_hash (tort_tp tort_ptr *rcvr)
{
  HSH_INIT();
  HSH(tort_W(rcvr));
  return HSH_VAL();
}

tort_v _tort_m_vector_base__equalQ (tort_tp tort_vector_base *rcvr, tort_vector_base *val)
{
  if ( rcvr == val ) return tort_true;
  if ( tort_h_mtable(rcvr) != tort_h_mtable(val) ) return tort_false;
  if ( rcvr->element_size != val->element_size ) return tort_false;
  if ( rcvr->size != val->size ) return tort_false;
  return memcmp(rcvr->data, val->data, rcvr->element_size * rcvr->size) == 0 ? tort_true : tort_false;
}

tort_v _tort_m_vector_base__equalQ_hash (tort_tp tort_vector_base *rcvr)
{
  HSH_INIT();
  h = jenkins_hash(h, (unsigned char *) rcvr->data, rcvr->element_size * rcvr->size);
  return HSH_VAL();
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

tort_v _tort_m_vector__equalQ_hash (tort_tp tort_vector_base *rcvr)
{
  HSH_INIT();
  tort_vector_EACH(rcvr, v) {
    HSH(tort_sendn(tort__s(equalQ_hash), 1, v));
  } tort_vector_EACH_END();
  return HSH_VAL();
}

tort_v _tort_m_pair__equalQ (tort_tp tort_pair *rcvr, tort_pair *val)
{
  if ( rcvr == val ) return tort_true;
  if ( tort_h_mtable(rcvr) != tort_h_mtable(val) ) return tort_false;
  if ( ! EQUALQ(rcvr->first, val->first) ) return tort_false;
  return_EQUALQ(rcvr->second, val->second);
}

tort_v _tort_m_pair__equalQ_hash (tort_tp tort_pair *rcvr)
{
  HSH_INIT();
  HSH(tort_sendn(tort__s(equalQ_hash), 1, rcvr->first));
  HSH(tort_sendn(tort__s(equalQ_hash), 1, rcvr->second));
  return HSH_VAL();
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

tort_v _tort_m_map__equalQ_hash (tort_tp tort_map *rcvr)
{
  HSH_INIT();
  HSH(tort_sendn(tort__s(equalQ_hash), 1, rcvr->equality));
  HSH(tort_sendn(tort__s(equalQ_hash), 1, rcvr->hash));
  tort_map_EACH(rcvr, e) {
    tort_pair *e2 = e;
    HSH(tort_sendn(tort__s(equalQ_hash), 1, e2));
  } tort_map_EACH_END();
  return HSH_VAL();
}

#undef EQUALQ
#undef return_EQUALQ

tort_v _tort_m_initializer__eq(tort_tp tort_v init)
{
  tort_send(tort__s(alias_method), tort__mt(object), tort__s(eqvQ),   tort__s(eqQ));
  tort_send(tort__s(alias_method), tort__mt(object), tort__s(equalQ), tort__s(eqQ));
  tort_send(tort__s(alias_method), tort__mt(word),   tort__s(equalQ), tort__s(eqvQ));
  tort_send(tort__s(alias_method), tort__mt(object), tort__s(eqvQ_hash),   tort__s(eqQ_hash));
  tort_send(tort__s(alias_method), tort__mt(object), tort__s(equalQ_hash), tort__s(eqQ_hash));
  tort_send(tort__s(alias_method), tort__mt(word),   tort__s(equalQ_hash), tort__s(eqvQ_hash));
  return init;
}
