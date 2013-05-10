#include "tort/tort.h"
#include "tort/internal.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

int _tort_lookup_trace = 0;
#if TORT_LOOKUP_TRACE
#else
#define _tort_lookup_trace 0
#endif

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
    ,non_symbol_lookup_n
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
  S(non_symbol_lookup_n);
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
  bzero(&mcache, sizeof(mcache));
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

tort_v _tort_m_mtable__delegateSET(tort_tp tort_mtable *rcvr, tort_v delegate)
{
  if ( rcvr->delegate != delegate ) {
    rcvr->delegate = delegate;
    _tort_m_mtable___delegate_changed(tort_ta rcvr);
  }
  return rcvr;
}

#define s_lookup tort__s(lookup)

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
	bzero(&mcache[i], sizeof(mcache[i]));
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

void tort_lookup_stop_at() // Add breakpoint here.
{
  // NOTHING
}

void tort_lookup_not_found_stop_at() // Add breakpoint here.
{
  // NOTHING
}

tort_lookup_decl(_tort_m_mtable__lookup)
{
  tort_v method = tort_nil;
  tort_symbol *sel = message->selector;

  if ( _tort_lookup_trace ) 
    _tort_lookup_trace_level ++;

 again:
  if ( tort_h_mtable(sel) != tort__mt(symbol) ) {
    if ( _tort_lookup_trace ) 
      _tort_lookup_trace_level ++;
    return_tort_sendn(tort__s(lookup), 2, sel, message);
  }
#if TORT_ANON_SYMBOL_MTABLE
  if ( sel->name == tort_nil )
    method = _tort_m_map__get(tort_ta (tort_v) sel->mtable_method_map, mtable);
  else
#endif
    method = _tort_m_map__get(tort_ta (tort_v) mtable, sel);

  if ( _tort_lookup_trace ) {
    fprintf(stderr, "  %p %*s_tort_m_mtable__lookup: mtable = %s, sel = %s, meth = %s\n", 
	    message,
	    _tort_lookup_trace_level, "",
	    tort_object_name(mtable), 
	    tort_object_name(sel),
	    tort_object_name(method));
    tort_lookup_stop_at();
  }

  if ( method != tort_nil ) {
    message->method = method;
    message->mtable = mtable;

    if ( _tort_lookup_trace ) 
      _tort_lookup_trace_level --;
  }
  else if ( (mtable = mtable->delegate) != tort_nil ) {
    (void) TORT_MCACHE_STAT(mcache_stats.delegate_traverse_n ++);
    if ( _tort_lookup_trace ) {
      if ( tort_h_mtable(mtable) == tort__mt(mtable) ) {
	goto again;
      } else {
	message = tort_sendn(s_lookup, 2, mtable, message);
	_tort_lookup_trace_level --;
      }
    } else {
      if ( tort_h_mtable(mtable) == tort__mt(mtable) ) {
	goto again;
      } else {
	return_tort_sendn(s_lookup, 2, mtable, message);
      }
    }
  }
  
  return message;
}

tort_message* _tort_lookup (tort_tp tort_v rcvr, tort_message *message)
{
  tort_v sel = message->selector;
#if TORT_GLOBAL_MCACHE
  tort_mcache_entry *mce = 0;
#endif

  /* Reuse rcvr as the actual rcvr's mtable search start. */
  /* See tort_send* macros. */
  // Start from rcvr's mtable, if not specified.
  if ( ! message->mtable )
    rcvr = message->mtable = tort_h_mtable(rcvr);
  else
    rcvr = message->mtable;

#define MTABLE ((tort_mtable*) rcvr)

  /* Initialize rest of message object. */
  message->_h[-1].applyf = _tort_m_object___cannot_apply;
  message->_h[-1].mtable = tort__mt(message);
  // message->previous_message = _tort_message;
  message->method = tort_nil;
  message->fiber = message->previous_message ? message->previous_message->fiber : _tort_fiber;
#if TORT_MESSAGE_FILE_LINE
  if ( ! message->caller_info ) 
    message->caller_info = tort_(unknown_caller_info);
#endif

  if ( _tort_lookup_trace )  {
    _tort_lookup_trace_level ++;
    fprintf(stderr, "  %p %*s_tort_lookup(rcvr = %s, sel = %s):\n",
	    message,
	    _tort_lookup_trace_level, "",
	    tort_object_name(message->receiver), 
	    tort_object_name(sel));
    tort_lookup_stop_at();
  }

  if ( tort_h_mtable(sel) != tort__mt(symbol) ) {
    (void) TORT_MCACHE_STAT(mcache_stats.non_symbol_lookup_n ++);
    goto do_lookup;
  }

#if TORT_GLOBAL_MCACHE
  (void) TORT_MCACHE_STAT(mcache_stats.lookup_n ++);
  size_t i = 
    (
     (((size_t) MTABLE) << 2) ^
     (((size_t) sel) >> 3)
     );
  mce = &mcache[i % MCACHE_SIZE];
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

    /* Avoid infinite regres. */
    if ( sel == s_lookup && MTABLE == tort__mt(mtable) ) {
    do_lookup:
      message = _tort_m_mtable__lookup(tort_ta MTABLE, message);
    } else {
      message = tort_sendn(s_lookup, 2, MTABLE, message);
    }
    
    if ( message->method == tort_nil ) {
      if ( _tort_lookup_trace )  {
	fprintf(stderr, "  %p %*s_tort_lookup(rcvr = %s, sel = %s) => method_not_found\n", 
		message,
		_tort_lookup_trace_level, "",
		tort_object_name(message->receiver), 
		tort_object_name(sel));
      }

      // message->mtable = MTABLE; // ???
      message->method = tort_(_m_method_not_found);
      tort_lookup_not_found_stop_at();
    }

#if TORT_GLOBAL_MCACHE
    if ( mce ) {
    /* fill mcache entry. */
    // fprintf(stderr, "-");
    mce->mt  = tort_h_mtable(message->receiver);
    mce->sel = sel;
#if TORT_MCACHE_USE_SYMBOL_VERSION
    mce->sel_version = tort_ref(tort_symbol, mce->sel)->version;
#endif

#if TORT_GLOBAL_MCACHE_STATS
    if ( mce->method != message->method )
      (void) TORT_MCACHE_STAT(++ mcache_stats.collision_n);
#endif
    mce->method = message->method;
    mce->mtable = message->mtable;
    }
  }
#endif

  if ( _tort_lookup_trace )  {
    fprintf(stderr, "  %p %*s_tort_lookup(rcvr = %s, sel = %s) => %s\n",
	    message,
	    _tort_lookup_trace_level, "",
	    tort_object_name(message->receiver), 
	    tort_object_name(sel),
	    tort_object_name(message->method));
    _tort_lookup_trace_level --;
  }

#undef MTABLE
  return message;
}

#ifdef _tort_lookup_trace
#undef _tort_lookup_trace
#endif

tort_message* _tort_lookup_debug (tort_tp tort_v rcvr, tort_message *message)
{
  ++ _tort_lookup_trace;
  message = _tort_lookup(tort_ta rcvr, message);
  -- _tort_lookup_trace;
  tort_lookup_stop_at();
  return message;
}

static void method_error(tort_message *message, const char *msg, tort_v object)
{
  extern void tort_debug_stop_at();
  tort_v rcvr = message->receiver;
  tort_error_message(msg, (char *) tort_object_name(object));
  tort_error_message("  selector %s", tort_object_name(message->selector));
  tort_error_message("  receiver %s", tort_object_name(rcvr));
  tort_error_message("  mtable   %s", tort_object_name(tort_h_mtable(rcvr)));
#if TORT_ALLOC_DEBUG
  tort_error_message("  receiver allocated at %s:%d #%lu",
		     tort_h(rcvr).alloc_file,
		     tort_h(rcvr).alloc_line,
		     tort_h(rcvr).alloc_id);
#endif
  tort_error_message("  message  %T", message);
  tort_debug_stop_at();
}

tort_apply_decl(_tort_m_object___method_not_found)
{
  method_error(tort_ta "method not found", _tort_message->selector);
  return tort_error(tort_ta ": not applicable");
}

tort_apply_decl(_tort_m_object___cannot_apply)
{
  method_error(tort_ta "cannot apply method %s", _tort_message->method);
  return tort_error(tort_ta ": not applicable");
}

static void mark_cache(void *data)
{
  tort_gc_mark_range(tort_nil, &mcache, ((void*) &mcache) + sizeof(mcache));
}

tort_v _tort_m_initializer__lookup(tort_tp tort_v init)
{
  tort_(_m_method_not_found) = tort_method_new(_tort_m_object___method_not_found, 0);
  tort_(_m_cannot_apply) = tort_method_new(_tort_m_object___cannot_apply, 0);
  tort_gc_add_root_callback(mark_cache, 0);
#if TORT_GLOBAL_MCACHE && TORT_GLOBAL_MCACHE_STATS
  if ( getenv("TORT_MCACHE_STATS") )
    atexit(_tort_mcache_stats);
#endif
  return init;
}

