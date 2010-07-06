#include "tort/tort.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>


tort_runtime *_tort;
tort_val _tort_message;

void *tort_malloc(size_t size)
{
  void *ptr = malloc(size);
  if ( ! ptr ) {
    tort_fatal("tort_malloc(%lu): failed", (unsigned long) size);
  }
  memset(ptr, 0, size);
  return ptr;
}

void *tort_realloc(void *ptr, size_t size)
{
  void *new_ptr = realloc(ptr, size);
  if ( ! new_ptr ) {
    tort_fatal("tort_realloc(%p, %lu): failed", (void *) ptr, (unsigned long) size);
  }
  return new_ptr;
}


static unsigned long _tort_alloc_id = 0;

tort_val _tort_allocate (
#if TORT_ALLOC_DEBUG
			      const char *alloc_file, int alloc_line, 
#endif
			      tort_val _tort_message, 
			      tort_val rcvr, size_t size, tort_val mtable)
{
  tort_val val;
  void *ptr;
  size += sizeof(tort_header);
  ptr = tort_malloc(size);
  ptr += sizeof(tort_header);
  val = tort_ref_box(ptr);

  tort_h_ref(val).applyf  = _tort_object_applyf;
  tort_h_ref(val).lookupf = _tort_object_lookupf;
  tort_h_ref(val).mtable  = mtable;

#if TORT_ALLOC_DEBUG
  tort_h_ref(val).alloc_file = alloc_file;
  tort_h_ref(val).alloc_line = alloc_line;
  tort_h_ref(val).alloc_id   = ++ _tort_alloc_id;

  if ( _tort_alloc_id == 0 ) {
    fprintf(stderr, "\nSTOP AT ALLOC ID = %lu\n", _tort_alloc_id);
    abort();
  }
#endif

  return val;
}


tort_val _tort_object_identity (tort_val _tort_message, tort_val rcvr)
{
  return rcvr;
}

tort_val _tort_object_clone (
			     tort_val _tort_message, 
			     tort_val rcvr, size_t size
			     )
{
  tort_val val;
  void *ptr;
  size += sizeof(tort_header);
  ptr = tort_malloc(size);

  memcpy(ptr, &tort_h_ref(rcvr), size);

  ptr += sizeof(tort_header);
  val = tort_ref_box(ptr);

  return val;
}


/******************************************************************************************************/


tort_val _tort_map_initialize(tort_val _tort_message, tort_val rcvr)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  map->entry_n = 0;
  map->entry = tort_malloc(sizeof(map->entry[0]) * (map->entry_n + 1));
  map->entry[0] = 0;
  return rcvr;
}


tort_val _tort_map_add(tort_val _tort_message, tort_val rcvr, tort_val key, tort_val value)
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

tort_val _tort_map_set(tort_val _tort_message, tort_val rcvr, tort_val key, tort_val value)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry *e = _tort_map_get_entry(_tort_message, rcvr, key);
  if ( ! e ) {
    _tort_map_add(_tort_message, rcvr, key, value);
  } else {
    e->value = value;
  }
  return rcvr;
}


tort_map_entry *_tort_map_get_entry(tort_val _tort_message, tort_val rcvr, tort_val key)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry **x = map->entry, *entry;

  while ( entry = *(x ++) ) {
    if ( entry->key == key ) {
      return entry;
    }
  }

  return 0;
}


tort_map_entry *_tort_map_get_entry_by_value(tort_val _tort_message, tort_val rcvr, tort_val value)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry **x = map->entry, *entry;

  while ( entry = *(x ++) ) {
    if ( entry->value == value ) {
      return entry;
    }
  }

  return 0;
}


tort_map_entry *_tort_map_get_entry_string(tort_val _tort_message, tort_val rcvr, tort_val key)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry **x = map->entry, *entry;

  while ( entry = *(x ++) ) {
    if ( strcmp(tort_string_data(entry->key), tort_string_data(key)) == 0 ) {
      return entry;
    }
  }

  return 0;
}


tort_map_entry *_tort_map_get_entry_cstr(tort_val _tort_message, tort_val rcvr, const char *key)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry **x = map->entry, *entry;

  while ( entry = *(x ++) ) {
    if ( strcmp(tort_string_data(entry->key), key) == 0 ) {
      return entry;
    }
  }

  return 0;
}

tort_val _tort_map_get(tort_val _tort_message, tort_val rcvr, tort_val key)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry *e = _tort_map_get_entry(_tort_message, rcvr, key);
  return e ? e->value : tort_nil;
}


tort_val _tort_map_get_key(tort_val _tort_message, tort_val rcvr, tort_val value)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry *e = _tort_map_get_entry_by_value(_tort_message, rcvr, value);
  return e ? e->key : tort_nil;
}


tort_val _tort_map_get_string(tort_val _tort_message, tort_val rcvr, tort_val key)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry *e = _tort_map_get_entry_string(_tort_message, rcvr, key);
  return e ? e->value : tort_nil;
}


tort_val _tort_map_clone(tort_val _tort_message, tort_val rcvr)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_val new_map = _tort_object_clone(_tort_message, rcvr, sizeof(tort_map));
  _tort_map_initialize(_tort_message, new_map);
  tort_map_entry **x = map->entry, *entry;

  while ( entry = *(x ++) ) {
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



tort_lookup_decl(_tort_object_lookupf)
{
  tort_ref(tort_message, _tort_message)->method = 
    _tort_map_get(_tort_message,
		  tort_mtable(tort_ref(tort_message, _tort_message)->receiver), 
		  tort_ref(tort_message, _tort_message)->selector);
  return _tort_message;
}

tort_apply_decl(_tort_object_applyf) 
{
  tort_error_message("cannot apply selector %s to", 
		     (char *) tort_symbol_data(tort_ref(tort_message, _tort_message)->selector)
		     );
  tort_error_message("  receiver @%p", (void*) rcvr);
#if TORT_ALLOC_DEBUG
  tort_error_message("  allocated at %s:%d#%lu", (void*) rcvr, 
		     tort_h(rcvr).alloc_file,
		     tort_h(rcvr).alloc_line,
		     tort_h(rcvr).alloc_id);
#endif
  // abort();
  tort_write(_tort_message, tort_stderr);
  tort_error(": not applicable");
  return tort_nil;
}


tort_lookup_decl(_tort_message_lookupf)
{
  tort_message *msg = tort_ref(tort_message, rcvr);
  msg->method = 
    _tort_map_get(_tort_message,
		  tort_mtable(msg->receiver), 
		  msg->selector);
  return msg->method;
}

tort_apply_decl(_tort_message_applyf)
{
  tort_message *msg = tort_ref(tort_message, rcvr);
  if ( msg->method != tort_nil ) {
    return tort_applyf(msg->method)(rcvr, msg->receiver);
  } else {
    return tort_nil;
  }
}


/******************************************************************************************************/

tort_val tort_vector_new(const tort_val *vec, size_t size)
{
  size_t alloc_size;
  tort_val val = tort_allocate(0, 0, sizeof(tort_vector), _tort->_mt_vector);
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


tort_val _tort_vector_new(tort_val _tort_message, tort_val rcvr, tort_val _size)
{
  return tort_vector_new(0, tort_I(_size));
}

tort_val _tort_vector_clone (
			     tort_val _tort_message, 
			     tort_val rcvr
			     )
{
  tort_val val = _tort_object_clone(_tort_message, rcvr, sizeof(tort_vector));
  tort_vector_data(val) = tort_malloc(sizeof(tort_vector_data(val)[0]) * (tort_vector_size(val) + 1));
  memcpy(tort_vector_data(val), tort_vector_data(rcvr), sizeof(tort_vector_data(val)[0]) * (tort_vector_size(val) + 1));
  return val;
}


tort_val _tort_vector_get (
			     tort_val _tort_message, 
			     tort_val rcvr, tort_val _i
			     )
{
  long i = tort_I(_i);
  return tort_i(tort_vector_data(rcvr)[i]);
}


tort_val _tort_vector_set (
			     tort_val _tort_message, 
			     tort_val rcvr, tort_val _i, tort_val _v
			     )
{
  long i = tort_I(_i);
  long v = tort_I(_v);
  tort_vector_data(rcvr)[i] = v;
  return rcvr;
}


tort_val _tort_vector_size (tort_val _tort_message, tort_val rcvr)
{
  return tort_i(tort_vector_size(rcvr));
}


tort_val _tort_vector_alloc_size (tort_val _tort_message, tort_val rcvr)
{
  return tort_i(tort_vector_alloc_size(rcvr));
}


/******************************************************************************************************/

tort_val tort_string_new(const char *string, size_t size)
{
  size_t alloc_size;
  tort_val val = tort_allocate(0, 0, sizeof(tort_string), _tort->_mt_string);
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

tort_val tort_string_new_cstr(const char *string)
{
  return tort_string_new(string, strlen(string));
}


tort_val _tort_string_new(tort_val _tort_message, tort_val rcvr, tort_val size)
{
  return tort_string_new(0, tort_I(size));
}


tort_val _tort_string_clone (
			     tort_val _tort_message, 
			     tort_val rcvr
			     )
{
  tort_val val = _tort_object_clone(_tort_message, rcvr, sizeof(tort_string));
  tort_string_data(val) = tort_malloc(sizeof(tort_string_data(val)[0]) * (tort_string_size(val) + 1));
  memcpy(tort_string_data(val), tort_string_data(rcvr), sizeof(tort_string_data(val)[0]) * (tort_string_size(val) + 1));
  return val;
}


tort_val _tort_string_get (
			     tort_val _tort_message, 
			     tort_val rcvr, tort_val _i
			     )
{
  long i = tort_I(_i);
  return tort_i(tort_string_data(rcvr)[i]);
}


tort_val _tort_string_set (
			     tort_val _tort_message, 
			     tort_val rcvr, tort_val _i, tort_val _v
			     )
{
  long i = tort_I(_i);
  long v = tort_I(_v);
  tort_string_data(rcvr)[i] = v;
  return rcvr;
}


/******************************************************************************************************/

tort_val tort_symbol_make(const char *string)
{
  tort_map_entry *e = _tort_map_get_entry_cstr(0, _tort->symbols, string);
  if ( e ) {
    // fprintf(stderr, "\n old symbol = %s %p\n", tort_symbol_data(e->value), (void *) e->value);
    return e->value;
  } else {
    tort_val key, value;
    key = tort_string_new_cstr(string);
    value = tort_allocate(0, 0, sizeof(tort_symbol), _tort->_mt_symbol);
    tort_ref(tort_symbol, value)->name = key;
    _tort_map_add(0, _tort->symbols, key, value);
    // fprintf(stderr, "\n new symbol = %s %p\n", tort_symbol_data(value), (void *) value);
    return value;
  }
}


tort_val tort_add_method(tort_val map, const char *name, void *applyf)
{
  tort_val meth = tort_method_make(applyf);
  tort_val sym = tort_symbol_make(name);
  tort_ref(tort_method, meth)->name = sym;
  _tort_map_set(0, map, sym, meth);
  return meth;
}


tort_val tort_method_make(tort_apply_decl((*applyf)))
{
  tort_val val = tort_allocate(0, 0, sizeof(tort_method), _tort->_mt_method);
  tort_method *meth = tort_ref(tort_method, val);
  meth->name = 0;
  val = tort_ref_box(meth);
  tort_h_ref(val).applyf = applyf;
  return val;
}


tort_val tort_map_create()
{
  tort_val val = tort_allocate(0, 0, sizeof(tort_map), _tort->_mt_map);
  return _tort_map_initialize(0, val);
}


tort_val tort_runtime_create()
{
  /* Create runtime object. */
  _tort = tort_ref(tort_runtime, tort_allocate(0, 0, sizeof(tort_runtime), 0));
  tort_runtime_initialize_error();

  /* Initialize tagged object header. */
  _tort->_tagged_header.lookupf = _tort_object_lookupf;
  _tort->_tagged_header.applyf  = _tort_object_applyf;

  /* Create map method table. */
  _tort->_mt_map         = tort_map_create();
  tort_h_ref(_tort->_mt_map).mtable = _tort->_mt_map;

  /* Create other basic method tables. */
  _tort->_mt_object      = tort_map_create();
  _tort->_mt_string      = tort_map_create();
  _tort->_mt_vector      = tort_map_create();
  _tort->_mt_symbol      = tort_map_create();
  _tort->_mt_method      = tort_map_create();
  _tort->_mt_message     = tort_map_create();
  _tort->_mt_nil         = tort_map_create();
  _tort->_mt_tagged      = tort_map_create();
  _tort->_tagged_header.mtable = _tort->_mt_tagged;

  /* Create the nil object. */
  tort_nil = tort_allocate(0, 0, sizeof(tort_object), _tort->_mt_nil);

  /* Initialize the message reference. */
  _tort_message = tort_nil;
  _tort->message = tort_nil;

  /* Create the empty containers. */
  tort_string_null = _tort_string_new(0, 0, 0);
  tort_vector_null = _tort_vector_new(0, 0, 0);

  /* Create the symbol table. */
  _tort->symbols = tort_map_create();

  /* Create the core symbols. */
  _tort->_s_new    = tort_symbol_make("new");
  _tort->_s_clone  = tort_symbol_make("clone");
  _tort->_s_lookup = tort_symbol_make("lookup");
  _tort->_s_apply  = tort_symbol_make("apply");
  _tort->_s_get    = tort_symbol_make("get");
  _tort->_s_get_key = tort_symbol_make("get_key");
  _tort->_s_set    = tort_symbol_make("set");
  _tort->_s_value  = tort_symbol_make("value");
  _tort->_s_true   = tort_symbol_make("true");
  _tort->_s_false  = tort_symbol_make("false");
  _tort->_s_size  = tort_symbol_make("size");
  _tort->_s_alloc_size  = tort_symbol_make("alloc_size");


  /* Uncloneable objects. */
  tort_add_method(_tort->_mt_symbol, "clone", _tort_object_identity);
  tort_add_method(_tort->_mt_nil,    "clone", _tort_object_identity);
  tort_add_method(_tort->_mt_tagged, "clone", _tort_object_identity);

  /* Basic object methods. */
  tort_add_method(_tort->_mt_object, "clone", _tort_object_clone);
  tort_add_method(_tort->_mt_object, "lookup", _tort_object_lookupf);
  tort_add_method(_tort->_mt_object, "apply", _tort_object_applyf);

  // tort_add_method(_tort->_mt_method, "apply", _tort_method_applyf);

  /* Basic map methods. */
  tort_add_method(_tort->_mt_map, "get", _tort_map_get);
  tort_add_method(_tort->_mt_map, "get_key", _tort_map_get_key);
  tort_add_method(_tort->_mt_map, "set", _tort_map_set);
  tort_add_method(_tort->_mt_map, "clone", _tort_map_clone);

  /* Vector methods. */
  tort_add_method(_tort->_mt_vector, "new", _tort_vector_new);
  tort_add_method(_tort->_mt_vector, "clone", _tort_vector_clone);
  tort_add_method(_tort->_mt_vector, "get", _tort_vector_get);
  tort_add_method(_tort->_mt_vector, "set", _tort_vector_set);
  tort_add_method(_tort->_mt_vector, "size", _tort_vector_size);
  tort_add_method(_tort->_mt_vector, "alloc_size", _tort_vector_alloc_size);

  /* String methods. */
  tort_add_method(_tort->_mt_string, "new", _tort_string_new);
  tort_add_method(_tort->_mt_string, "clone", _tort_string_clone);
  tort_add_method(_tort->_mt_string, "get", _tort_string_get);
  tort_add_method(_tort->_mt_string, "set", _tort_string_set);
  tort_add_method(_tort->_mt_string, "size", _tort_vector_size);
  tort_add_method(_tort->_mt_string, "size", _tort_vector_size);
  tort_add_method(_tort->_mt_string, "alloc_size", _tort_vector_alloc_size);

  /* Subsystem initialization. */
  tort_runtime_initialize_io();
  tort_runtime_initialize_write();
  // tort_runtime_initialize_address();

  /* Prepare symbol table get method. */
  tort_h_ref(_tort->symbols).mtable = tort_send(tort_s(clone), tort_mtable(_tort->symbols));
  tort_add_method(tort_mtable(_tort->symbols), "get", _tort_map_get_string);

  _tort->_initialized = tort_s(true);

  return tort_ref_box(_tort);
}

