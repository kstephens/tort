#include "tort/tort.h"

typedef struct tort_dmap {
  tort_map *map;
  struct tort_dmap *delegate;
} tort_dmap;


tort_v _tort_M_dmap__new(tort_tp tort_v mtable, tort_map *map, tort_dmap *delegate)
{
  tort_dmap *dmap = tort_send(tort__s(_allocate), mtable, sizeof(*dmap));
  dmap->map = map;
  dmap->delegate = delegate;
  return dmap;
}


tort_v _tort_m_dmap__get(tort_tp tort_dmap *dmap, tort_v key)
{
  while ( dmap ) {
    tort_pair *e = tort_send(tort__s(get_entry), dmap->map, key);
    if ( e ) return e->value;
    dmap = dmap->delegate;
  }
  return tort_nil;
}


tort_v _tort_m_dmap__set(tort_tp tort_dmap *dmap, tort_v key, tort_v value)
{
  while ( dmap ) {
    tort_pair *e = tort_send(tort__s(get_entry), dmap->map, key);
    if ( e ) {
      e->value = key;
      return dmap;
    }
    dmap = dmap->delegate;
  }
  tort_send(tort__s(set), dmap->map, key, value);
  return dmap;
}


tort_v tort_runtime_initialize_dmap()
{
  return tort_mtable_make("dmap", tort_mt(object));
}

