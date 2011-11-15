#include "tort/core.h"

tort_ACCESSOR(map,tort_v,equality);
#define EQ(X,Y) (rcvr->equality != tort_nil ? tort_sendn(rcvr->equality, 2, (X), (Y)) != tort_false : (X) == (Y))

tort_v _tort_M_map__new(tort_tp tort_v mtable)
{
  tort_v val = tort_allocate(mtable, sizeof(tort_map));
  return _tort_m_map__initialize(tort_ta val);
}

tort_v _tort_m_map__initialize(tort_tp tort_map *rcvr)
{
  rcvr->equality = tort_nil;
  return _tort_m_vector_base___initialize(tort_ta (tort_vector_base*) rcvr, 0, sizeof(tort_pair*));
}

tort_v _tort_m_map__add(tort_tp tort_map *rcvr, tort_v key, tort_v value)
{
  tort_pair *e = _tort_M_pair__new(tort_ta tort__mt(pair), key, value);
  return _tort_m_vector_base___add(tort_ta (tort_vector_base*) rcvr, &e);
}

tort_pair* _tort_m_map__get_entry(tort_tp tort_map *rcvr, tort_v key)
{
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

tort_v tort_map_create()
{
  return _tort_M_map__new(tort_ta tort__mt(map));
}

tort_v _tort_m_map___gc_mark(tort_tp tort_vector *o)
{
  tort_gc_mark_range(o, o->data, o->data + o->size);
  return 0;
}

