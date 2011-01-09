#include "tort/tort.h"
#include "tort/init.h"
#include "tort/internal.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include "gc.h"

#if 0
#undef TORT_LOOKUP_TRACE
#define TORT_LOOKUP_TRACE 1
#endif

int _tort_lookup_trace_level = 0;

tort_message *_tort_message;
tort_v _tort_fiber;

tort_v _tort_m_object___mtable (tort_thread_param tort_v rcvr)
{
  return tort_h_ref(rcvr)->mtable;
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
  tort_v mt; /* The starting mtable. */
  tort_v sel;
#if TORT_MCACHE_USE_SYMBOL_VERSION
  tort_v sel_version;
#endif
  tort_v method; /* The method if found. */
  tort_mtable *mtable; /* The mtable the method was found in. */
} tort_mcache_entry;

#define mcache_size 1021
static
tort_mcache_entry mcache[mcache_size];

struct {
  unsigned long 
     hit_n
    ,hit_mtable_n
    ,hit_sel_n
    ,hit_sel_version_n 
    ,lookup_n
    ,method_change_n
    ,lookup_change_n
    ,delegate_change_n
    ,flush_all_n
    ,flush_symbol_n
    ,symbol_version_change_n
    ;
} mcache_stats;

#if TORT_GLOBAL_MCACHE_STATS
#define TORT_MCACHE_STAT(X) ((X), 1)
#else
#define TORT_MCACHE_STAT(X) (1)
#endif

void _tort_mcache_stats()
{
  fprintf(stderr, "tort: mcache: %lu hit / %lu lookup = %.8g\n",
	  (unsigned long) mcache_stats.hit_n,
	  (unsigned long) mcache_stats.lookup_n,
	  (double) mcache_stats.hit_n / (double) mcache_stats.lookup_n);

#define S(N) \
  fprintf(stderr, "tort: mcache: %26s = %16lu\n", #N, (unsigned long) mcache_stats.N)
  S(method_change_n);
  S(lookup_change_n);
  S(delegate_change_n);
  S(symbol_version_change_n);
  S(flush_all_n);
  S(flush_symbol_n);
#undef S
}


static void mcache_flush_all()
{
  (void) TORT_MCACHE_STAT(++ mcache_stats.flush_all_n);
  memset(&mcache, 0, sizeof(mcache));
}

#endif

tort_v _tort_m_mtable___delegate_changed(tort_tp tort_mtable *rcvr)
{
#if TORT_GLOBAL_MCACHE
  (void) TORT_MCACHE_STAT(++ mcache_stats.delegate_change_n);
  mcache_flush_all();
#endif
  return rcvr;
}

tort_v _tort_m_mtable__delegate(tort_tp tort_mtable *rcvr)
{
  return rcvr->delegate;
}

tort_v _tort_m_mtable__set_delegate(tort_tp tort_mtable *rcvr, tort_v delegate)
{
  if ( rcvr->delegate != delegate ) {
    rcvr->delegate = delegate;
    _tort_m_mtable___delegate_changed(tort_ta rcvr);
  }
  return rcvr;
}

tort_v _tort_m_mtable___method_changed(tort_tp tort_mtable *rcvr, tort_v sym, tort_v meth)
{
#if TORT_GLOBAL_MCACHE
  (void) TORT_MCACHE_STAT(++ mcache_stats.method_change_n);
#if TORT_MCACHE_USE_SYMBOL_VERSION
  if ( sym == tort__s(lookup) ) {
    (void) TORT_MCACHE_STAT(++ mcache_stats.lookup_change_n);
    mcache_flush_all();
  } else {
    int i;
    for ( i = 0; i < mcache_size; ++ i ) {
      if ( mcache[i].sel == sym ) {
	(void) TORT_MCACHE_STAT(++ mcache_stats.flush_symbol_n);
	memset(&mcache[i], 0, sizeof(mcache[i]));
      }
    }
  }
#else
  mcache_flush_all();
#endif
#endif
  return rcvr;
}


tort_v _tort_m_symbol___version_change(tort_tp tort_symbol *sym)
{
  (void) TORT_MCACHE_STAT(++ mcache_stats.symbol_version_change_n);
  sym->version += 2;
  return sym;
}


tort_lookup_decl(_tort_m_mtable__lookup)
{
  tort_v method = tort_nil, sel = message->selector;

  if ( TORT_LOOKUP_TRACE ) 
    _tort_lookup_trace_level ++;

  method =
    _tort_m_map__get(tort_thread_arg
		     (tort_v) mtable, 
		     sel);

  if ( TORT_LOOKUP_TRACE ) {
    fprintf(stderr, "  %p %*s_tort_m_mtable__lookup: mtable = %s, sel = %s, meth = %s\n", 
	    message,
	    _tort_lookup_trace_level, "",
	    tort_object_name(mtable), 
	    tort_symbol_data(sel),
	    tort_object_name(method));
  }
  
  if ( method != tort_nil ) {
    message->method = method;
    message->mtable = mtable;
  }
  else if ( (mtable = mtable->delegate) != tort_nil ) {
    message = tort_send(tort_s(lookup), mtable, message);
  }
  
  if ( TORT_LOOKUP_TRACE ) 
    _tort_lookup_trace_level --;

  return message;
}


tort_lookup_decl(_tort_lookup)
{
  tort_v sel = message->selector;

  message->method = tort_nil;

#ifndef TORT_MCACHE_STAT
#define TORT_MCACHE_STAT(X) 1
#endif

  if ( TORT_LOOKUP_TRACE )  {
    _tort_lookup_trace_level ++;
    fprintf(stderr, "  %p %*s_tort_lookup: rcvr = %s, sel = %s\n",
	    message,
	    _tort_lookup_trace_level, "",
	    tort_object_name(message->receiver), 
	    tort_symbol_data(sel));
  }

#if TORT_GLOBAL_MCACHE
  (void) TORT_MCACHE_STAT(mcache_stats.lookup_n ++);
  size_t i = 
    (
     (((size_t) mtable) << 2) ^
     (((size_t) sel) >> 3)
     );
  tort_mcache_entry *mce = &mcache[i % mcache_size];
  if (    mce->mt  == mtable && TORT_MCACHE_STAT(++ mcache_stats.hit_mtable_n)
       && mce->sel == sel    && TORT_MCACHE_STAT(++ mcache_stats.hit_sel_n)
#if TORT_MCACHE_USE_SYMBOL_VERSION
       && mce->sel_version == tort_ref(tort_symbol, sel)->version && TORT_MCACHE_STAT(++ mcache_stats.hit_sel_version_n)
#endif
       ) {
    (void) TORT_MCACHE_STAT(mcache_stats.hit_n ++);
    // fprintf(stderr, "+"); fflush(stderr);
    /* Use mcache entry. */
    message->method = mce->method;
    message->mtable = mce->mtable;
  } else {
#endif

    if ( sel == tort__s(lookup) && message->receiver == (tort_v) tort__mt(mtable) ) {
      message = _tort_m_mtable__lookup(tort_ta mtable, message);
    } else {
      message = tort_send(tort__s(lookup), mtable, message);
    }
    
    if ( message->method == tort_nil ) {
      if ( TORT_LOOKUP_TRACE )  {
	fprintf(stderr, "  %p %*s_tort_lookup: method_not_found: %s, %s\n", 
		message,
		_tort_lookup_trace_level, "",
		tort_object_name(message->receiver), 
		tort_symbol_data(sel));
      }

      message->mtable = mtable;
      message->method = tort_(_m_method_not_found);
    }

#if TORT_GLOBAL_MCACHE
    /* fill mcache entry. */
    // fprintf(stderr, "-");
    mce->mt  = tort_h_mtable(message->receiver);
    mce->sel = sel;
#if TORT_MCACHE_USE_SYMBOL_VERSION
    mce->sel_version = tort_ref(tort_symbol, mce->sel)->version;
#endif
    mce->method = message->method;
    mce->mtable = message->mtable;
  }
#endif

  if ( TORT_LOOKUP_TRACE )
    _tort_lookup_trace_level --;


  return message;
}


tort_apply_decl(_tort_m_object___method_not_found) 
{
  tort_error_message("cannot apply selector %s to", 
		     (char *) tort_object_name(_tort_message->selector)
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


tort_v tort_runtime_initialize_tort()
{
  tort_(_m_method_not_found) = tort_method_make(_tort_m_object___method_not_found);

#if TORT_GLOBAL_MCACHE && TORT_GLOBAL_MCACHE_STATS
  if ( getenv("TORT_MCACHE_STATS") ) {
    atexit(_tort_mcache_stats);
  }
#endif
  return 0;
}
