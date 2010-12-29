#include "tort/core.h"

/********************************************************************/


tort_v _tort_m_map__initialize(tort_thread_param tort_map *rcvr)
{
  return _tort_m_vector_base___initialize(tort_thread_arg (tort_vector_base*) rcvr, 0, sizeof(tort_map_entry*));
}


tort_v _tort_m_map__add(tort_thread_param tort_map *rcvr, tort_v key, tort_v value)
{
  tort_map_entry *e = tort_malloc(sizeof(*e));
  e->key = key;
  e->value = value;
  return _tort_m_vector_base___add(tort_thread_arg (tort_vector_base*) rcvr, &e);
}


tort_map_entry* _tort_m_map__get_entry(tort_thread_param tort_map *rcvr, tort_v key)
{
  tort_map_EACH(rcvr, entry) {
    if ( entry->key == key ) 
      return entry;
  } tort_map_EACH_END();
  return 0;
}


tort_map_entry* _tort_m_map__get_entry_by_value(tort_thread_param tort_map *rcvr, tort_v value)
{
  tort_map_EACH(rcvr, entry) {
    if ( entry->value == value ) 
      return entry;
  } tort_map_EACH_END();
  return 0;
}


tort_map_entry* _tort_m_map__get_entry_string(tort_thread_param tort_map *rcvr, tort_v key)
{
  tort_map_EACH(rcvr, entry) {
    if ( strcmp(tort_string_data(entry->key), tort_string_data(key)) == 0 )
      return entry;
  } tort_map_EACH_END();
  return 0;
}


tort_map_entry* _tort_m_map__get_entry_cstr(tort_thread_param tort_map *rcvr, const char *key)
{
  tort_map_EACH(rcvr, entry) {
    if ( strcmp(tort_string_data(entry->key), key) == 0 ) {
      return entry;
    }
  } tort_map_EACH_END();
  return 0;
}


tort_v _tort_m_map__get(tort_thread_param tort_map *rcvr, tort_v key)
{
  tort_map_entry *e = _tort_m_map__get_entry(tort_thread_arg rcvr, key);
  return e ? e->value : tort_nil;
}


tort_v _tort_m_map__get_key(tort_thread_param tort_map *rcvr, tort_v value)
{
  tort_map_entry *e = _tort_m_map__get_entry_by_value(tort_thread_arg rcvr, value);
  return e ? e->key : tort_nil;
}


tort_v _tort_m_map__get_string(tort_thread_param tort_map *rcvr, tort_v key)
{
  tort_map_entry *e = _tort_m_map__get_entry_string(tort_thread_arg rcvr, key);
  return e ? e->value : tort_nil;
}


tort_v _tort_m_map__set(tort_thread_param tort_map *rcvr, tort_v key, tort_v value)
{
  tort_map_entry *e = _tort_m_map__get_entry(tort_thread_arg rcvr, key);
  if ( ! e ) {
    _tort_m_map__add(tort_thread_arg rcvr, key, value);
  } else {
    e->value = value;
  }
  return rcvr;
}


tort_v _tort_m_map__delete(tort_thread_param tort_map *rcvr, tort_v key)
{
  tort_map_EACH(rcvr, entry) {
    if ( entry->key == key ) {
      do {
	entryp[-1] = entryp[0];
      } while ( (entry = *(entryp ++)) );
      -- tort_map_size(rcvr);
      break;
    }
  } tort_map_EACH_END();
  return rcvr;
}


tort_v _tort_m_map__clone(tort_thread_param tort_map *rcvr)
{
  tort_v new_map = _tort_m_object__clone(tort_thread_arg rcvr);
  _tort_m_map__initialize(tort_thread_arg new_map);
  tort_map_EACH(rcvr, entry) {
    // fprintf(stderr, "  entry => { %s, %p }\n", tort_symbol_data(entry->key), (void*) entry->value);
    _tort_m_map__add(tort_thread_arg new_map, entry->key, entry->value);
  } tort_map_EACH_END();
  return new_map;
}


/************************************g********************************/


tort_v tort_map_create()
{
  tort_v val = tort_allocate(0, 0, sizeof(tort_map), tort__mt(map));
  return _tort_m_map__initialize(tort_thread_arg val);
}

