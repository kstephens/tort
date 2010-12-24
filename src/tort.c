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
tort_v _tort_fiber;

/********************************************************************/


unsigned long _tort_alloc_id = 0;

tort_v _tort_allocate (
#if TORT_ALLOC_DEBUG
			      const char *alloc_file, int alloc_line, 
#endif
			      tort_thread_param 
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

  ++ _tort_alloc_id;

#if TORT_ALLOC_DEBUG
  tort_h_ref(val)->alloc_file = alloc_file;
  tort_h_ref(val)->alloc_line = alloc_line;
  tort_h_ref(val)->alloc_id   = _tort_alloc_id;

  if ( _tort_alloc_id == 0 ) {
    fprintf(stderr, "\nSTOP AT ALLOC ID = %lu\n", _tort_alloc_id);
    abort();
  }
#endif

  return val;
}


tort_v _tort_m_object__clone (tort_thread_param tort_v rcvr)
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


tort_v _tort_m_object__identity (tort_thread_param tort_v rcvr)
{
  return rcvr;
}


/********************************************************************/


#ifndef MCACHE
#define MCACHE 1
#endif

#if MCACHE
typedef struct tort_mcache_entry {
  tort_v mt;
  tort_v sel;
  tort_v sel_version;
  tort_v meth;
} tort_mcache_entry;

static
tort_mcache_entry mcache[1021];

#define MC_HASH(mt, sel) (((size_t) mt >> 7) ^ ((size_t) sel >> 3)) 
#endif

#ifndef TORT_LOOKUP_TRACE
#define TORT_LOOKUP_TRACE 0
#endif

tort_lookup_decl(_tort_object_lookupf)
{
  tort_v meth = tort_nil;
  tort_v mtable, sel;
  
  _tort->message = _tort_message;

  mtable = tort_h_mtable(tort_ref(tort_message, _tort_message)->receiver);
  sel = tort_ref(tort_message, _tort_message)->selector;

#if TORT_LOOKUP_TRACE
  fprintf(stderr, "  tol: rcvr = %s, sel = %s\n", 
	  tort_object_name(tort_ref(tort_message, _tort_message)->receiver), 
	  tort_symbol_data(sel));
#endif

#if MCACHE
  tort_mcache_entry *mce;
  int i = MC_HASH(mtable, sel) % (sizeof(mcache) / sizeof(mcache[0]));
  mce = &mcache[i];
  if ( mce->mt == mtable && 
       mce->sel == sel && 
       mce->sel_version == tort_ref(tort_symbol, sel)->version ) {
    // fprintf(stderr, "+"); fflush(stderr);
    meth = mce->meth;
  } else {
#endif

    do {
      meth =
	_tort_m_map__get(_tort_message,
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

#if MCACHE
    // fprintf(stderr, "-");
    mce->mt = mtable;
    mce->sel = sel;
    mce->sel_version = tort_ref(tort_symbol, mce->sel)->version;
    mce->meth = meth;
  }
#endif

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


tort_v _tort_m_mtable__add_method (tort_thread_param tort_v map, tort_v sym, tort_v func)
{
  tort_v meth = tort_method_make((void*) tort_I(func));
  tort_ref(tort_method, meth)->name = sym;
  tort_ref(tort_symbol, sym)->version += 2;
  _tort_m_map__set(0, map, sym, meth);
  return meth;
}


tort_v tort_add_method(tort_v map, const char *name, void *applyf)
{
  tort_v sym = tort_symbol_make(name);
  tort_v meth = tort_i(applyf);
  return _tort_m_mtable__add_method(tort_thread_arg map, sym, meth);
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

