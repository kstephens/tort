#include "tort/tort.h"
#include "tort/init.h"
#include "tort/internal.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include "gc.h"


/********************************************************************/

tort_message *_tort_message;
tort_v _tort_fiber;

/********************************************************************/


unsigned long _tort_alloc_id = 0;

tort_v _tort_allocate(tort_thread_param tort_v mtable, size_t size
#if TORT_ALLOC_DEBUG
		      ,const char *alloc_file, int alloc_line, 
#endif
)
{
  tort_v val = _tort_m_mtable___allocate(tort_thread_arg mtable, size);
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


tort_v _tort_m_mtable__allocate (tort_tp tort_mtable *mtable)
{
  return _tort_m_mtable___allocate(tort_ta mtable, mtable->instance_size);
}

tort_v _tort_m_mtable___allocate (tort_thread_param tort_mtable *mtable, size_t size)
{
  tort_v val;
  void *ptr;
  size_t alloc_size = sizeof(tort_header) + size;

  assert(size);
  assert(alloc_size > size);

  /* HACK: save the instance size in the mtable. */
  if ( mtable && ! mtable->instance_size ) {
    mtable->instance_size = size;
  }

  ptr = tort_malloc(alloc_size);
  ptr += sizeof(tort_header);
  val = tort_ref_box(ptr);

  tort_h_ref(val)->alloc_size = size;
  tort_h_ref(val)->lookupf = _tort_object_lookupf;
  tort_h_ref(val)->applyf  = _tort_object_applyf;
  tort_h_ref(val)->mtable  = mtable;

  ++ _tort_alloc_id;

  return val;
}


tort_v _tort_m_object___mtable (tort_thread_param tort_v rcvr)
{
  return tort_i(tort_h_ref(rcvr)->mtable);
}

tort_v _tort_m_object___alloc_size (tort_thread_param tort_v rcvr)
{
  return tort_i(tort_h_ref(rcvr)->alloc_size);
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


#if TORT_GLOBAL_MCACHE

typedef struct tort_mcache_entry {
  tort_v mt;
  tort_v sel;
#if TORT_MCACHE_USE_SYMBOL_VERSION
  tort_v sel_version;
#endif
  tort_v meth;
} tort_mcache_entry;

#define mcache_size 1021
static
tort_mcache_entry mcache[mcache_size];

struct {
  unsigned long 
  hit_n, hit_mtable_n, hit_sel_n, hit_sel_version_n, 
    lookup_n;
} mcache_stats;

#if TORT_GLOBAL_MCACHE_STATS
#define TORT_MCACHE_STAT(X) ((X), 1)
#endif

void _tort_mcache_stats()
{
  fprintf(stderr, "tort: mache: %lu hit / %lu lookup = %.8g\n",
	  (unsigned long) mcache_stats.hit_n,
	  (unsigned long) mcache_stats.lookup_n,
	  (double) mcache_stats.hit_n / (double) mcache_stats.lookup_n);

  fprintf(stderr, "tort: mache: %lu hit mt, %lu hit sel, %lu hit sel_version.\n",
	  (unsigned long) mcache_stats.hit_mtable_n,
	  (unsigned long) mcache_stats.hit_sel_n,
	  (unsigned long) mcache_stats.hit_sel_version_n);
}

#endif


tort_v _tort_m_mtable___delegate_changed(tort_thread_param tort_v rcvr)
{
#if TORT_GLOBAL_MCACHE
  memset(&mcache, 0, sizeof(mcache));
#endif
  return rcvr;
}

tort_v _tort_m_mtable__delegate(tort_thread_param tort_v rcvr)
{
  return tort_ref(tort_mtable, rcvr)->delegate;
}

tort_v _tort_m_mtable__set_delegate(tort_thread_param tort_v rcvr, tort_v delegate)
{
  if ( tort_ref(tort_mtable, rcvr)->delegate != delegate ) {
    tort_ref(tort_mtable, rcvr)->delegate = delegate;
    tort_send(tort__s(_delegate_changed), rcvr);
  }
  return rcvr;
}


tort_v _tort_m_mtable___method_changed(tort_thread_param tort_v rcvr, tort_v sym, tort_v meth)
{
#if TORT_GLOBAL_MCACHE
#if TORT_MCACHE_USE_SYMBOL_VERSION
  if ( sym == tort__s(lookup) ) {
    memset(&mcache, 0, sizeof(mcache));
  } else {
    int i;
    for ( i = 0; i < mcache_size; ++ i ) {
      if ( mcache[i].sel == sym ) {
	memset(&mcache[i], 0, sizeof(mcache[i]));
      }
    }
  }
#else
  memset(&mcache, 0, sizeof(mcache));
#endif
#endif
  return rcvr;
}


tort_lookup_decl(_tort_object_lookupf)
{
  tort_v meth = tort_nil;
  tort_v mtable, sel;

#if 0
  tort_(message) = _tort_message;
#endif

  mtable = tort_h_mtable(_tort_message->receiver);
  sel = _tort_message->selector;

#ifndef TORT_MCACHE_STAT
#define TORT_MCACHE_STAT(X) 1
#endif

#if TORT_LOOKUP_TRACE
  fprintf(stderr, "  tol: rcvr = %s, sel = %s\n", 
	  tort_object_name(tort_ref(tort_message, _tort_message)->receiver), 
	  tort_symbol_data(sel));
#endif

#if TORT_GLOBAL_MCACHE
  (void) TORT_MCACHE_STAT(mcache_stats.lookup_n ++);
  size_t i = 
    (
     ((((size_t) mtable) + (size_t) sel) << 5) ^
     (((size_t) sel) << 3) ^
     ((size_t) mtable) ^
     ((size_t) sel)
     );
  tort_mcache_entry *mce = &mcache[i % mcache_size];
  if (    mce->mt == mtable && TORT_MCACHE_STAT(++ mcache_stats.hit_mtable_n)
       && mce->sel == sel && TORT_MCACHE_STAT(++ mcache_stats.hit_sel_n)
#if TORT_MCACHE_USE_SYMBOL_VERSION
       && mce->sel_version == tort_ref(tort_symbol, sel)->version && TORT_MCACHE_STAT(++ mcache_stats.hit_sel_version_n)
#endif
       ) {
    (void) TORT_MCACHE_STAT(mcache_stats.hit_n ++);
    // fprintf(stderr, "+"); fflush(stderr);
    meth = mce->meth;
  } else {
#endif

    do {
      meth =
	_tort_m_map__get(tort_thread_arg
			 mtable, 
			 sel);
      
#if TORT_LOOKUP_TRACE
      fprintf(stderr, "    tol: mtable = %s, meth = %s\n", 
	      tort_object_name(mtable), 
	      tort_object_name(meth));
#endif
      
      if ( meth != tort_nil )
	break;

      assert(! meth);
      
      mtable = tort_ref(tort_mtable, mtable)->delegate;
    } while ( mtable != tort_nil );

#if TORT_GLOBAL_MCACHE
    // fprintf(stderr, "-");
    mce->mt = tort_h_mtable(tort_ref(tort_message, _tort_message)->receiver);
    mce->sel = sel;
#if TORT_MCACHE_USE_SYMBOL_VERSION
    mce->sel_version = tort_ref(tort_symbol, mce->sel)->version;
#endif
    mce->meth = meth;
  }
#endif

  _tort_message->method = meth;

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
  tort_v obj = tort_allocate(tort__mt(object), sizeof(tort_object));
  return obj;
}


tort_v _tort_m_mtable__add_method (tort_thread_param tort_v map, tort_v sym, tort_v func)
{
  tort_v meth = tort_method_make((void*) tort_I(func));
  tort_ref(tort_method, meth)->name = sym;
  tort_ref(tort_symbol, sym)->version += 2;
  _tort_m_map__set(tort_thread_arg map, sym, meth);
  _tort_m_mtable___method_changed(tort_thread_arg map, sym, meth);
  return meth;
}


tort_v tort_add_method(tort_v mtable, const char *name, void *applyf)
{
  tort_v sym = tort_symbol_make(name);
  tort_v meth = tort_i(applyf);
  return _tort_m_mtable__add_method(tort_thread_arg mtable, sym, meth);
}

tort_v tort_add_class_method(tort_v mtable, const char *name, void *applyf)
{
  return tort_add_method(tort_h_ref(mtable)->mtable, name, applyf);
}

tort_v tort_method_make(tort_apply_decl((*applyf)))
{
  tort_v val = tort_allocate(tort__mt(method), sizeof(tort_method));
  tort_method *meth = tort_ref(tort_method, val);
  meth->name = 0;
  val = tort_ref_box(meth);
  tort_h_ref(val)->applyf = applyf;
  return val;
}



tort_v tort_runtime_initialize_tort()
{
#if TORT_GLOBAL_MCACHE && TORT_GLOBAL_MCACHE_STATS
  atexit(_tort_mcache_stats);
#endif
  return 0;
}
