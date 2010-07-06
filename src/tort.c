#include "tort/tort.h"
#include "tort/init.h"
#include "tort/internal.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include "gc.h"


/********************************************************************/


tort_runtime *_tort;
tort_v _tort_message;


/********************************************************************/


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


/********************************************************************/


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


/********************************************************************/


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


/********************************************************************/


tort_v tort_object_make()
{
  tort_v obj = tort_allocate(0, 0, sizeof(tort_object), tort__mt(object));
  return obj;
}


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



