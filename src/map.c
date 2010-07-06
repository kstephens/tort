#include "tort/core.h"

/********************************************************************/


tort_v _tort_map_initialize(tort_v _tort_message, tort_v rcvr)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  map->entry_n = 0;
  map->entry = tort_malloc(sizeof(map->entry[0]) * (map->entry_n + 1));
  map->entry[0] = 0;
  return rcvr;
}


tort_v _tort_map_add(tort_v _tort_message, tort_v rcvr, tort_v key, tort_v value)
{
  tort_map *map = tort_ref(tort_map, rcvr);

  tort_map_entry *e = tort_malloc(sizeof(*e));
  e->key = key;
  e->value = value;

  map->entry = tort_realloc(map->entry, sizeof(map->entry[0]) * (map->entry_n + 2));
  map->entry[map->entry_n] = e;
  map->entry[++ map->entry_n] = 0;

  return rcvr;
}

tort_map_entry *_tort_map_get_entry(tort_v _tort_message, tort_v rcvr, tort_v key)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry **x = map->entry, *entry;

  while ( (entry = *(x ++)) ) {
    if ( entry->key == key ) {
      return entry;
    }
  }

  return 0;
}


tort_map_entry *_tort_map_get_entry_by_value(tort_v _tort_message, tort_v rcvr, tort_v value)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry **x = map->entry, *entry;

  while ( (entry = *(x ++)) ) {
    if ( entry->value == value ) {
      return entry;
    }
  }

  return 0;
}


tort_map_entry *_tort_map_get_entry_string(tort_v _tort_message, tort_v rcvr, tort_v key)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry **x = map->entry, *entry;

  while ( (entry = *(x ++)) ) {
    if ( strcmp(tort_string_data(entry->key), tort_string_data(key)) == 0 ) {
      return entry;
    }
  }

  return 0;
}


tort_map_entry *_tort_map_get_entry_cstr(tort_v _tort_message, tort_v rcvr, const char *key)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry **x = map->entry, *entry;

  while ( (entry = *(x ++)) ) {
    if ( strcmp(tort_string_data(entry->key), key) == 0 ) {
      return entry;
    }
  }

  return 0;
}


tort_v _tort_map_get(tort_v _tort_message, tort_v rcvr, tort_v key)
{
  tort_map_entry *e = _tort_map_get_entry(_tort_message, rcvr, key);
  return e ? e->value : tort_nil;
}


tort_v _tort_map_get_key(tort_v _tort_message, tort_v rcvr, tort_v value)
{
  tort_map_entry *e = _tort_map_get_entry_by_value(_tort_message, rcvr, value);
  return e ? e->key : tort_nil;
}


tort_v _tort_map_get_string(tort_v _tort_message, tort_v rcvr, tort_v key)
{
  tort_map_entry *e = _tort_map_get_entry_string(_tort_message, rcvr, key);
  return e ? e->value : tort_nil;
}


tort_v _tort_map_set(tort_v _tort_message, tort_v rcvr, tort_v key, tort_v value)
{
  tort_map_entry *e = _tort_map_get_entry(_tort_message, rcvr, key);
  if ( ! e ) {
    _tort_map_add(_tort_message, rcvr, key, value);
  } else {
    e->value = value;
  }
  return rcvr;
}


tort_v _tort_map_delete(tort_v _tort_message, tort_v rcvr, tort_v key)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry **x = map->entry, *entry;

  while ( (entry = *(x ++)) ) {
    if ( entry->key == key ) {
      do {
	x[-1] = x[0];
      } while ( (entry = *(x ++)) );
      map->entry_n --;
      break;
    }
  }

  return rcvr;
}


tort_v _tort_map_size(tort_v _tort_message, tort_v rcvr)
{
  return tort_i(tort_ref(tort_map, rcvr)->entry_n);
}


tort_v _tort_map_clone(tort_v _tort_message, tort_v rcvr)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_v new_map = _tort_object_clone(_tort_message, rcvr);
  _tort_map_initialize(_tort_message, new_map);
  tort_map_entry **x = map->entry, *entry;

  while ( (entry = *(x ++)) ) {
    // fprintf(stderr, "  entry => { %s, %p }\n", tort_symbol_data(entry->key), (void*) entry->value);
    _tort_map_add(_tort_message, new_map, entry->key, entry->value);
  }

#if 0
  map = tort_ref(tort_map, rcvr);
  x = map->entry;
  while ( entry = *(x ++) ) {
    fprintf(stderr, "  new entry => { %s, %p }\n", tort_symbol_data(entry->key), (void*) entry->value);
  }
#endif

  return new_map;
}


/********************************************************************/


tort_v tort_map_create()
{
  tort_v val = tort_allocate(0, 0, sizeof(tort_map), _tort->_mt_map);
  return _tort_map_initialize(0, val);
}


tort_v tort_mtable_create(tort_v delegate)
{
  tort_v val = tort_allocate(0, 0, sizeof(tort_mtable), _tort->_mt_mtable);
  _tort_map_initialize(0, val);
  if ( delegate == 0 ) {
    delegate = tort_nil;
  }
  tort_ref(tort_mtable, val)->delegate = delegate;
  return val;
}


