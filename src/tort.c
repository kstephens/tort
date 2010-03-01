#include "tort/tort.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>


tort_runtime *_tort;


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


tort_val _tort_map_initialize(tort_val message, tort_val rcvr)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  map->entry_n = 0;
  map->entry = tort_malloc(sizeof(map->entry[0]) * (map->entry_n + 1));
  return rcvr;
}

tort_val _tort_map_add(tort_val message, tort_val rcvr, tort_val key, tort_val value)
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

tort_val _tort_map_set(tort_val message, tort_val rcvr, tort_val key, tort_val value)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry *e = _tort_map_get_entry(message, rcvr, key);
  if ( ! e ) {
    _tort_map_add(message, rcvr, key, value);
  } else {
    e->value = value;
  }
  return rcvr;
}

tort_map_entry *_tort_map_get_entry(tort_val message, tort_val rcvr, tort_val key)
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


tort_map_entry *_tort_map_get_entry_string(tort_val message, tort_val rcvr, const char *key)
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

tort_val _tort_map_get(tort_val message, tort_val rcvr, tort_val key)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry *e = _tort_map_get_entry(0, rcvr, key);
  return e ? e->value : tort_nil;
}

tort_lookup_decl(_tort_object_lookupf) {
  tort_ref(tort_message, message)->method = 
    _tort_map_get(message,
		  tort_mtable(tort_ref(tort_message, message)->receiver), 
		  tort_ref(tort_message, message)->selector);
  return message;
}

tort_apply_decl(_tort_object_applyf) {
  tort_error("cannot apply this object");
  return tort_nil;
}


tort_lookup_decl(_tort_message_lookupf) {
  tort_message *msg = tort_ref(tort_message, rcvr);
  msg->method = 
    _tort_map_get(message,
		  tort_mtable(msg->receiver), 
		  msg->selector);
  return msg->method;
}

tort_apply_decl(_tort_message_applyf) {
  tort_message *msg = tort_ref(tort_message, rcvr);
  if ( msg->method != tort_nil ) {
    return tort_applyf(msg->method)(rcvr, msg->receiver);
  } else {
    return tort_nil;
  }
}



tort_val tort_allocate(tort_val message, tort_val rcvr, size_t size, tort_val mtable)
{
  tort_val val;
  void *ptr;
  size += sizeof(tort_header);
  ptr = tort_malloc(size);
  ptr += sizeof(tort_header);
  val = tort_ref_box(ptr);

  tort_applyf(val)  = _tort_object_applyf;
  tort_lookupf(val) = _tort_object_lookupf;
  tort_mtable(val)  = mtable;

  return tort_ref_box(ptr);
}


tort_val tort_string_new_cstr(const char *string)
{
  size_t size = strlen(string);
  size_t alloc_size;
  tort_val val = tort_allocate(0, 0, sizeof(tort_string), _tort->_mt_string);
  alloc_size = sizeof(tort_ref(tort_string, val)->data[0]) * (size + 1);
  tort_ref(tort_string, val)->size = size;
  tort_ref(tort_string, val)->data = tort_malloc(alloc_size);
  memcpy(tort_ref(tort_string, val)->data, string, alloc_size);
  tort_ref(tort_string, val)->data[size] = 0;
  // fprintf(stderr, "\n new string = \"%s\" %p\n", tort_ref(tort_string, val)->data, (void *) val);
  return val;
}


tort_val tort_symbol_make(const char *string)
{
  tort_map_entry *e = _tort_map_get_entry_string(0, _tort->symbols, string);
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
  tort_applyf(val) = applyf;
  return val;
}


void _tort_fatal(const char *format, va_list vap)
{
  fprintf(stderr, "\ntort fatal: ");
  vfprintf(stderr, format, vap);
  fprintf(stderr, "\n");
  fflush(stderr);
  abort();
}


void _tort_error(const char *format, va_list vap)
{
  fprintf(stderr, "\ntort error: ");
  vfprintf(stderr, format, vap);
  fprintf(stderr, "\n");
  fflush(stderr);
}


tort_val tort_map_create()
{
  tort_val val = tort_allocate(0, 0, sizeof(tort_map), _tort->_mt_map);
  return _tort_map_initialize(0, val);
}


tort_val tort_runtime_create()
{
  _tort = tort_ref(tort_runtime, tort_allocate(0, 0, sizeof(tort_runtime), 0));
  _tort->error = _tort_error;
  _tort->fatal = _tort_fatal;

  _tort->_mt_map         = tort_map_create();
  tort_mtable(_tort->_mt_map) = _tort->_mt_map;

  _tort->_mt_object      = tort_map_create();
  _tort->_mt_string      = tort_map_create();
  _tort->_mt_symbol      = tort_map_create();
  _tort->_mt_method      = tort_map_create();
  _tort->_mt_message     = tort_map_create();
  _tort->_mt_nil         = tort_map_create();

  tort_nil = tort_allocate(0, 0, sizeof(tort_object), _tort->_mt_nil);
  
  _tort->symbols = tort_map_create();

  _tort->_s_new    = tort_symbol_make("new");
  _tort->_s_lookup = tort_symbol_make("lookup");
  _tort->_s_apply  = tort_symbol_make("apply");
  _tort->_s_get    = tort_symbol_make("get");
  _tort->_s_set    = tort_symbol_make("set");
  _tort->_s_write  = tort_symbol_make("write");
  _tort->_s_value  = tort_symbol_make("value");

  tort_add_method(_tort->_mt_object, "lookup", _tort_object_lookupf);
  tort_add_method(_tort->_mt_object, "apply", _tort_object_applyf);

  // tort_add_method(_tort->_mt_method, "apply", _tort_method_applyf);

  tort_add_method(_tort->_mt_map, "get", _tort_map_get);
  tort_add_method(_tort->_mt_map, "set", _tort_map_set);

  tort_runtime_initialize_write();

  return tort_ref_box(_tort);
}

