#include "tort/tort.h"
#include "tort/init.h"
#include "tort/internal.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

int _tort_lookup_trace_level = 0;

tort_message *_tort_message;
tort_v _tort_fiber;

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

#define MCACHE_SIZE 1021
static
tort_mcache_entry mcache[MCACHE_SIZE];

static
struct {
  unsigned long 
     hit_n
    ,hit_mtable_n
    ,hit_sel_n
    ,hit_sel_version_n 
    ,lookup_n
    ,delegate_traverse_n
    ,method_change_n
    ,lookup_change_n
    ,delegate_change_n
    ,flush_all_n
    ,flush_symbol_n
    ,symbol_version_change_n
    ,collision_n
    ,mcache_size
    ;
} mcache_stats;

#if TORT_GLOBAL_MCACHE_STATS
#define TORT_MCACHE_STAT(X) ((X), 1)
#else
#define TORT_MCACHE_STAT(X) (1)
#endif

void _tort_mcache_stats()
{
  mcache_stats.mcache_size = MCACHE_SIZE;
  fprintf(stderr, "tort: mcache: %lu hit / %lu lookup = %.8g\n",
	  (unsigned long) mcache_stats.hit_n,
	  (unsigned long) mcache_stats.lookup_n,
	  (double) mcache_stats.hit_n / (double) mcache_stats.lookup_n);

#define S(N) \
  fprintf(stderr, "tort: mcache: %26s = %16lu\n", #N, (unsigned long) mcache_stats.N)
  S(hit_mtable_n);
  S(hit_sel_n);
  S(hit_sel_version_n);
  S(lookup_n);
  S(delegate_traverse_n);
  S(mcache_size);
  S(method_change_n);
  S(lookup_change_n);
  S(delegate_change_n);
  S(symbol_version_change_n);
  S(flush_all_n);
  S(flush_symbol_n);
  S(collision_n);
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

#if 1
#define s_lookup tort__s(lookup)
#else
#define s_lookup tort_s(lookup)
#endif

tort_v _tort_m_mtable___method_changed(tort_tp tort_mtable *rcvr, tort_v sym, tort_v meth)
{
#if TORT_GLOBAL_MCACHE
  (void) TORT_MCACHE_STAT(++ mcache_stats.method_change_n);
#if TORT_MCACHE_USE_SYMBOL_VERSION
  if ( sym == s_lookup ) {
    (void) TORT_MCACHE_STAT(++ mcache_stats.lookup_change_n);
    mcache_flush_all();
  } else {
    int i;
    for ( i = 0; i < MCACHE_SIZE; ++ i ) {
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
  tort_v method = tort_nil;
  tort_symbol *sel = message->selector;

  if ( TORT_LOOKUP_TRACE ) 
    _tort_lookup_trace_level ++;

#if TORT_ANON_SYMBOL_MTABLE
  if ( sel->name == tort_nil )
    method = _tort_m_map__get(tort_ta (tort_v) sel->mtable_method_map, mtable);
  else
#endif
    method = _tort_m_map__get(tort_ta (tort_v) mtable, sel);

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

    if ( TORT_LOOKUP_TRACE ) 
      _tort_lookup_trace_level --;
  }
  else if ( (mtable = mtable->delegate) != tort_nil ) {
    (void) TORT_MCACHE_STAT(mcache_stats.delegate_traverse_n ++);
    if ( TORT_LOOKUP_TRACE ) {
      message = tort_send(s_lookup, mtable, message);
      _tort_lookup_trace_level --;
    } else {
      return_tort_send(s_lookup, mtable, message);
    }
  }
  
  return message;
}

tort_message* _tort_lookup (tort_tp tort_v rcvr, tort_message *message)
{
  tort_v sel = message->selector;
  
  rcvr = tort_h_mtable(rcvr); /* See tort_send* macros. */
#define MTABLE ((tort_mtable*) rcvr)

  message->_h[-1].alloc_size = sizeof(tort_message);
  message->_h[-1].mtable = tort__mt(message);
  message->mtable = tort_nil; 
  message->method = tort_nil;
  message->fiber = message->previous_message ? message->previous_message->fiber : _tort_fiber;

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
     (((size_t) MTABLE) << 2) ^
     (((size_t) sel) >> 3)
     );
  tort_mcache_entry *mce = &mcache[i % MCACHE_SIZE];
  if (    mce->mt  == MTABLE && TORT_MCACHE_STAT(++ mcache_stats.hit_mtable_n)
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

    if ( sel == s_lookup && message->receiver == (tort_v) tort__mt(mtable) ) {
      message = _tort_m_mtable__lookup(tort_ta MTABLE, message);
    } else {
      message = tort_send(s_lookup, MTABLE, message);
    }
    
    if ( message->method == tort_nil ) {
      if ( TORT_LOOKUP_TRACE )  {
	fprintf(stderr, "  %p %*s_tort_lookup: method_not_found: %s, %s\n", 
		message,
		_tort_lookup_trace_level, "",
		tort_object_name(message->receiver), 
		tort_symbol_data(sel));
      }

      message->mtable = MTABLE;
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

#if TORT_GLOBAL_MCACHE_STATS
    if ( mce->method )
      (void) TORT_MCACHE_STAT(++ mcache_stats.collision_n);
#endif
    mce->method = message->method;
    mce->mtable = message->mtable;
  }
#endif

  if ( TORT_LOOKUP_TRACE )
    _tort_lookup_trace_level --;

#undef MTABLE
  return message;
}

tort_apply_decl(_tort_m_object___method_not_found) 
{
  tort_error_message("cannot apply selector %s to", 
		     (char *) tort_object_name(_tort_message->selector)
		     );
  tort_error_message("  receiver %s", tort_object_name(rcvr));
#if TORT_ALLOC_DEBUG
  tort_error_message("  allocated at %s:%d #%lu",
		     tort_h(rcvr).alloc_file,
		     tort_h(rcvr).alloc_line,
		     tort_h(rcvr).alloc_id);
#endif
  tort_error_message("  message %T", _tort_message);
  tort_error(": not applicable");
  return tort_nil;
}

tort_v tort_runtime_initialize_lookup()
{
  tort_(_m_method_not_found) = tort_method_make(_tort_m_object___method_not_found);

#if TORT_GLOBAL_MCACHE && TORT_GLOBAL_MCACHE_STATS
  if ( getenv("TORT_MCACHE_STATS") )
    atexit(_tort_mcache_stats);
#endif
  return 0;
}
