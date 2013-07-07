#include "tort/core.h"
#include <stdarg.h>

tort_SLOT(map,tort_v,equality);
#define EQ(X,Y) (rcvr->equality != tort_nil ? tort_sendn(rcvr->equality, 2, (X), (Y)) != tort_false : (X) == (Y))

tort_v _tort_M_map__new(tort_tp tort_v mtable, ...)
{
  tort_map *o = tort_allocate(mtable, sizeof(tort_map));
  int i;
  _tort_m_map__initialize(tort_ta o);
  if ( _tort_message && (i = tort_I(_tort_message->argc)) > 1 ) {
    va_list vap;
    va_start(vap, mtable);
    -- i;
    while ( i > 0 ) {
      tort_v k = va_arg(vap, tort_v);
      tort_v v = va_arg(vap, tort_v);
      i -= 2;
      tort_send(tort__s(set), o, k, v);
    }
    va_end(vap);
  }
  return o;
}

tort_v _tort_m_map__initialize(tort_tp tort_map *rcvr)
{
  rcvr->equality = rcvr->hash = rcvr->hash_vector = tort_nil;
  return _tort_m_vector_base___initialize(tort_ta (tort_vector_base*) rcvr, 0, sizeof(tort_pair*));
}

#define USE_HASH(rcvr)  ( rcvr->hash != tort_nil && rcvr->hash_vector != tort_nil )

tort_v _tort_m_map__add(tort_tp tort_map *rcvr, tort_v key, tort_v value)
{
  tort_pair *e = _tort_M_pair__new(tort_ta tort__mt(pair), key, value);
  if ( USE_HASH(rcvr) ) { tort_sendn(tort__s(add_hash_entry), 2, rcvr, e); }
  return _tort_m_vector_base___add(tort_ta (tort_vector_base*) rcvr, &e);
}

tort_pair* _tort_m_map__get_entry(tort_tp tort_map *rcvr, tort_v key)
{
  if ( USE_HASH(rcvr) ) { return tort_sendn(tort__s(get_hash_entry), 2, rcvr, key); }
  tort_map_EACH(rcvr, entry) {
    if ( EQ(entry->first, key) ) 
      return entry;
  } tort_map_EACH_END();
  return 0;
}

tort_pair* _tort_m_map__get_entry_by_value(tort_tp tort_map *rcvr, tort_v value)
{
  tort_map_EACH(rcvr, entry) {
    if ( entry->second == value ) 
      return entry;
  } tort_map_EACH_END();
  return 0;
}

tort_pair* _tort_m_map__get_entry_string(tort_tp tort_map *rcvr, tort_v key)
{
  tort_map_EACH(rcvr, entry) {
    if ( strcmp(tort_string_data(entry->first), tort_string_data(key)) == 0 )
      return entry;
  } tort_map_EACH_END();
  return 0;
}

tort_pair* _tort_m_map__get_entry_cstr(tort_tp tort_map *rcvr, const char *key)
{
  tort_map_EACH(rcvr, entry) {
    if ( strcmp(tort_string_data(entry->first), key) == 0 )
      return entry;
  } tort_map_EACH_END();
  return 0;
}

tort_v _tort_m_map__get(tort_tp tort_map *rcvr, tort_v key)
{
  tort_pair *e = _tort_m_map__get_entry(tort_ta rcvr, key);
  return e ? e->second : tort_nil;
}

tort_v _tort_m_map__get_key(tort_tp tort_map *rcvr, tort_v value)
{
  tort_pair *e = _tort_m_map__get_entry_by_value(tort_ta rcvr, value);
  return e ? e->first : tort_nil;
}

tort_v _tort_m_map__get_string(tort_tp tort_map *rcvr, tort_v key)
{
  tort_pair *e = _tort_m_map__get_entry_string(tort_ta rcvr, key);
  return e ? e->second : tort_nil;
}

tort_v _tort_m_map__set(tort_tp tort_map *rcvr, tort_v key, tort_v value)
{
  tort_pair *e = _tort_m_map__get_entry(tort_ta rcvr, key);
  if ( ! e )
    _tort_m_map__add(tort_ta rcvr, key, value);
  else
    e->second = value;
  return rcvr;
}

tort_v _tort_m_map__delete(tort_tp tort_map *rcvr, tort_v key)
{
  tort_map_EACH(rcvr, entry) {
    if ( EQ(entry->first, key) ) {
      tort_v value = entry->second;
      if ( USE_HASH(rcvr) ) { tort_sendn(tort__s(delete_hash_entry), 2, rcvr, entry); }
      do {
	entryp[-1] = entryp[0];
      } while ( (entry = *(entryp ++)) );
      -- tort_map_size(rcvr);
      return value;
    }
  } tort_map_EACH_END();
  return tort_nil;
}

tort_v _tort_m_map__emit(tort_tp tort_map *rcvr, tort_map *target)
{
  tort_map_EACH(rcvr, entry) {
    // fprintf(stderr, "  entry => { %s, %p }\n", tort_symbol_data(entry->first), (void*) entry->second);
    tort_send(tort__s(set), target, entry->first, entry->second);
  } tort_map_EACH_END();
  return target;
}

tort_v _tort_m_map__clone(tort_tp tort_map *rcvr)
{
  tort_v new_map = _tort_m_object__clone(tort_ta rcvr);
  _tort_m_map__initialize(tort_ta new_map);
  return _tort_m_map__emit(tort_ta rcvr, new_map);
}

tort_v _tort_m_map__add_hash_entry(tort_tp tort_map *rcvr, tort_pair *entry)
{
  tort_vw h; tort_pair *e;
  if ( rcvr->hash == tort_nil ) return rcvr;
  if ( rcvr->hash_vector == tort_nil || rcvr->hash_vector->size <= rcvr->_vector_base.size ) tort_sendn(tort__s(rehash), 1, rcvr);
  h = tort_I(tort_sendn(rcvr->hash, 1, ((tort_pair*) entry)->first));
  h %= rcvr->hash_vector->size;
  e = rcvr->hash_vector->data[h];
  e = _tort_M_pair__new(tort_ta tort__mt(pair), entry, e);
  // fprintf(stderr, "  %p[%lu:%lu] <= %p\n", rcvr, (unsigned long) h, (unsigned long) rcvr->hash_vector->size, entry);
  rcvr->hash_vector->data[h] = e;
  return rcvr;
}

tort_v _tort_m_map__get_hash_entry(tort_tp tort_map *rcvr, tort_v key)
{
  tort_vw h; tort_pair *e;
  if ( ! USE_HASH(rcvr) ) return 0;
  h = tort_I(tort_sendn(rcvr->hash, 1, key));
  h %= rcvr->hash_vector->size;
  e = rcvr->hash_vector->data[h];
  while ( e ) {
    tort_pair *entry = e->first;
    if ( EQ(entry->first, key) ) { e = entry; break; }
    e = e->second;
  }
  return e;
}

tort_v _tort_m_map__delete_hash_entry(tort_tp tort_map *rcvr, tort_pair *entry)
{
  tort_vw h; tort_pair *e, **ep;
  if ( ! USE_HASH(rcvr) ) return 0;
  h = tort_I(tort_sendn(rcvr->hash, 1, entry->first));
  h %= rcvr->hash_vector->size;
  ep = (void*) &rcvr->hash_vector->data[h];
  while ( (e = *ep) ) {
    tort_pair *entry = e->first;
    if ( e->first == entry ) { *ep = e->second; break; }
    ep = (void*) &e->second;
  }
  return e;
}

tort_v _tort_m_map__rehash(tort_tp tort_map *rcvr)
{
  if ( rcvr->hash == tort_nil ) return rcvr;
  rcvr->hash_vector = tort_sendn(tort__s(new), 2, tort__mt(vector), tort_i(rcvr->_vector_base.size * 2 + 1));
  tort_vector_EACH(rcvr, entry) {
    tort_sendn(tort__s(add_hash_entry), 2, rcvr, entry);
  } tort_vector_EACH_END(rcvr);
  return rcvr;
}

tort_v tort_map_new()
{
  return _tort_M_map__new(tort_ta tort__mt(map));
}

tort_v _tort_m_map___gc_mark(tort_tp tort_vector *o)
{
  tort_gc_mark_range(o, o->data, o->data + o->size);
  return 0;
}

