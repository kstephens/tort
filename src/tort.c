#include "tort/tort.h"
#include "tort/init.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include "gc.h"


tort_runtime *_tort;
tort_v _tort_message;

void *tort_malloc(size_t size)
{
  void *ptr = GC_malloc(size);
  if ( ! ptr ) {
    tort_fatal("tort_malloc(%lu): failed", (unsigned long) size);
  }
  memset(ptr, 0, size);
  return ptr;
}

void *tort_realloc(void *ptr, size_t size)
{
  void *new_ptr = GC_realloc(ptr, size);
  if ( ! new_ptr ) {
    tort_fatal("tort_realloc(%p, %lu): failed", (void *) ptr, (unsigned long) size);
  }
  return new_ptr;
}


#if TORT_ALLOC_DEBUG
static unsigned long _tort_alloc_id = 0;
#endif

tort_v _tort_allocate (
#if TORT_ALLOC_DEBUG
			      const char *alloc_file, int alloc_line, 
#endif
			      tort_v _tort_message, 
			      tort_v rcvr, size_t size, tort_v mtable)
{
  tort_v val;
  void *ptr;
  size_t alloc_size = sizeof(tort_header) + size;

  assert(size);
  assert(alloc_size > size);

  ptr = tort_malloc(alloc_size);
  ptr += sizeof(tort_header);
  val = tort_ref_box(ptr);

  tort_h_ref(val)->alloc_size = size;
  tort_h_ref(val)->lookupf = _tort_object_lookupf;
  tort_h_ref(val)->applyf  = _tort_object_applyf;
  tort_h_ref(val)->mtable  = mtable;

#if TORT_ALLOC_DEBUG
  tort_h_ref(val)->alloc_file = alloc_file;
  tort_h_ref(val)->alloc_line = alloc_line;
  tort_h_ref(val)->alloc_id   = ++ _tort_alloc_id;

  if ( _tort_alloc_id == 0 ) {
    fprintf(stderr, "\nSTOP AT ALLOC ID = %lu\n", _tort_alloc_id);
    abort();
  }
#endif

  return val;
}


tort_v _tort_object_clone (tort_v _tort_message, tort_v rcvr)
{
  tort_v val;
  void *ptr;
  size_t alloc_size = sizeof(tort_header) + tort_h_ref(rcvr)->alloc_size;

  assert(tort_h_ref(rcvr)->alloc_size);

  ptr = tort_malloc(alloc_size);

  memcpy(ptr, tort_h_ref(rcvr), alloc_size);

  ptr += sizeof(tort_header);
  val = tort_ref_box(ptr);

  return val;
}


tort_v _tort_object_identity (tort_v _tort_message, tort_v rcvr)
{
  return rcvr;
}


/******************************************************************************************************/


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


#ifndef TORT_LOOKUP_TRACE
#define TORT_LOOKUP_TRACE 0
#endif

tort_lookup_decl(_tort_object_lookupf)
{
  tort_v meth = tort_nil;
  tort_v mtable;
  
  _tort->message = _tort_message;

#if TORT_LOOKUP_TRACE
  fprintf(stderr, "  tol: rcvr = %s, sel = %s\n", 
	  tort_object_name(tort_ref(tort_message, _tort_message)->receiver), 
	  tort_symbol_data(tort_ref(tort_message, _tort_message)->selector));
#endif

  mtable = tort_h_mtable(tort_ref(tort_message, _tort_message)->receiver);
  
  do {
    meth =
      _tort_map_get(_tort_message,
		    mtable, 
		    tort_ref(tort_message, _tort_message)->selector);

#if TORT_LOOKUP_TRACE
    fprintf(stderr, "    tol: mtable = %s, meth = %s\n", 
	    tort_object_name(mtable), 
	    tort_object_name(meth));
#endif

    assert(meth);
    if ( meth != tort_nil )
      break;
    
    mtable = tort_ref(tort_mtable, mtable)->delegate;
 } while ( mtable != tort_nil );
  
  tort_ref(tort_message, _tort_message)->method = meth;

  return _tort_message;
}


tort_apply_decl(_tort_object_applyf) 
{
  tort_error_message("cannot apply selector %s to", 
		     (char *) tort_object_name(tort_ref(tort_message, _tort_message)->selector)
		     );
  tort_error_message("  receiver %s", tort_object_name(rcvr));
#if TORT_ALLOC_DEBUG
  tort_error_message("  allocated at %s:%d#%lu",
		     tort_h(rcvr).alloc_file,
		     tort_h(rcvr).alloc_line,
		     tort_h(rcvr).alloc_id);
#endif
  abort();
  tort_inspect(tort_stderr, _tort_message);
  tort_error(": not applicable");
  return tort_nil;
}


/******************************************************************************************************/

tort_v tort_vector_new(const tort_v *vec, size_t size)
{
  size_t alloc_size;
  tort_v val = tort_allocate(0, 0, sizeof(tort_vector), _tort->_mt_vector);
  alloc_size = sizeof(tort_ref(tort_vector, val)->data[0]) * (size);
  tort_ref(tort_vector, val)->size = 
  tort_ref(tort_vector, val)->alloc_size = 
    size;
  tort_ref(tort_vector, val)->data = tort_malloc(alloc_size);
  if ( vec ) {
    memcpy(tort_ref(tort_vector, val)->data, vec, alloc_size);
  } else {
    size_t i;

    for ( i = 0; i < size; ++ i ) {
      tort_vector_data(val)[i] = tort_nil;
    }
  }
  return val;
}


tort_v _tort_vector_new(tort_v _tort_message, tort_v rcvr, tort_v _size)
{
  return tort_vector_new(0, tort_I(_size));
}


tort_v _tort_vector_clone (tort_v _tort_message, tort_v rcvr)
{
  tort_v val = _tort_object_clone(_tort_message, rcvr);
  tort_vector_data(val) = tort_malloc(sizeof(tort_vector_data(val)[0]) * (tort_vector_size(rcvr)));
  memcpy(tort_vector_data(val), tort_vector_data(rcvr), sizeof(tort_vector_data(val)[0]) * (tort_vector_size(rcvr)));
  return val;
}


tort_v _tort_vector_get (tort_v _tort_message, tort_v rcvr, tort_v _i)
{
  long i = tort_I(_i);
  return tort_vector_data(rcvr)[i];
}


tort_v _tort_vector_set (tort_v _tort_message, tort_v rcvr, tort_v _i, tort_v _v)
{
  long i = tort_I(_i);
  tort_vector_data(rcvr)[i] = _v;
  return rcvr;
}


tort_v _tort_vector_size (tort_v _tort_message, tort_v rcvr)
{
  return tort_i(tort_vector_size(rcvr));
}


tort_v _tort_vector_alloc_size (tort_v _tort_message, tort_v rcvr)
{
  return tort_i(tort_vector_alloc_size(rcvr));
}


tort_v _tort_vector_each (tort_v _tort_message, tort_v rcvr, tort_v block)
{
  tort_vector_loop(rcvr, x) {
    tort_send(tort__s(value), block, x);
  }
  tort_vector_loop_end(rcvr);
  return rcvr;
}


tort_v _tort_vector_map (tort_v _tort_message, tort_v rcvr, tort_v block)
{
  tort_v new_vec = tort_send(tort__s(clone), rcvr);
  tort_vector_loop(rcvr, x) {
    x = tort_send(tort__s(value), block, x);
    tort_send(tort__s(set), new_vec, tort_i(x_i), x);
#if 0
    tort_printf(tort_stderr, 
		"  tort_v_m(%p(%p) => %p(%p)[%d] => %T\n", 
		rcvr, tort_vector_data(rcvr),
		new_vec, tort_vector_data(new_vec),
		x_i, 
		tort_vector_data(new_vec)[x_i]);
#endif
 }
  tort_vector_loop_end(rcvr);
  return new_vec;
}

/******************************************************************************************************/

tort_v tort_string_new(const char *string, size_t size)
{
  size_t alloc_size;
  tort_v val = tort_allocate(0, 0, sizeof(tort_string), _tort->_mt_string);
  alloc_size = sizeof(tort_ref(tort_string, val)->data[0]) * (size + 1);
  tort_ref(tort_string, val)->size = 
  tort_ref(tort_string, val)->alloc_size = 
    size;
  tort_ref(tort_string, val)->data = tort_malloc(alloc_size);
  if ( string ) {
    memcpy(tort_ref(tort_string, val)->data, string, alloc_size);
    tort_ref(tort_string, val)->data[size] = 0;
  } else {
    memset(tort_ref(tort_string, val)->data, 0, alloc_size);
  }
  // fprintf(stderr, "\n new string = \"%s\" %p\n", tort_ref(tort_string, val)->data, (void *) val);
  return val;
}

tort_v tort_string_new_cstr(const char *string)
{
  return tort_string_new(string, strlen(string));
}


tort_v _tort_string_new(tort_v _tort_message, tort_v rcvr, tort_v size)
{
  return tort_string_new(0, tort_I(size));
}


tort_v _tort_string_clone (tort_v _tort_message, tort_v rcvr)
{
  tort_v val = _tort_object_clone(_tort_message, rcvr);
  tort_string_data(val) = tort_malloc(sizeof(tort_string_data(val)[0]) * (tort_string_size(val) + 1));
  memcpy(tort_string_data(val), tort_string_data(rcvr), sizeof(tort_string_data(val)[0]) * (tort_string_size(val) + 1));
  return val;
}


tort_v _tort_string_get (tort_v _tort_message, tort_v rcvr, tort_v _i)
{
  long i = tort_I(_i);
  return tort_i(tort_string_data(rcvr)[i]);
}


tort_v _tort_string_set (tort_v _tort_message, tort_v rcvr, tort_v _i, tort_v _v)
{
  long i = tort_I(_i);
  long v = tort_I(_v);
  tort_string_data(rcvr)[i] = v;
  return rcvr;
}


/******************************************************************************************************/

tort_v tort_symbol_make(const char *string)
{
  if ( string ) {
  tort_map_entry *e = _tort_map_get_entry_cstr(0, _tort->symbols, string);
  if ( e ) {
    // fprintf(stderr, "\n old symbol = %s %p\n", tort_symbol_data(e->value), (void *) e->value);
    return e->value;
  } else {
    tort_v key, value;
    key = tort_string_new_cstr(string);
    value = tort_allocate(0, 0, sizeof(tort_symbol), _tort->_mt_symbol);
    tort_ref(tort_symbol, value)->name = key;
    _tort_map_add(0, _tort->symbols, key, value);
    // fprintf(stderr, "\n new symbol = %s %p\n", tort_symbol_data(value), (void *) value);
    return value;
  } 
  } else {
    tort_v value;
    value = tort_allocate(0, 0, sizeof(tort_symbol), _tort->_mt_symbol);
    tort_ref(tort_symbol, value)->name = tort_nil;
    return value;
  }
}


tort_v tort_add_method(tort_v map, const char *name, void *applyf)
{
  tort_v meth = tort_method_make(applyf);
  tort_v sym = tort_symbol_make(name);
  tort_ref(tort_method, meth)->name = sym;
  _tort_map_set(0, map, sym, meth);
  return meth;
}


tort_v tort_method_make(tort_apply_decl((*applyf)))
{
  tort_v val = tort_allocate(0, 0, sizeof(tort_method), _tort->_mt_method);
  tort_method *meth = tort_ref(tort_method, val);
  meth->name = 0;
  val = tort_ref_box(meth);
  tort_h_ref(val)->applyf = applyf;
  return val;
}


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


