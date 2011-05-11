#include "tort/core.h"


/********************************************************************/


size_t _tort_gc_finalize_count = 0;


/********************************************************************/


int _tort_gc_mode = 0;

static
void *(*_tort_malloc)(size_t size) = malloc;

static
void *(*_tort_malloc_atomic)(size_t size) = malloc;

static
void (*_tort_free)(void *ptr) = free;

static
void *(*_tort_realloc)(void *ptr, size_t size) = realloc;

static
void *(*_tort_realloc_atomic)(void *ptr, size_t size) = realloc;

void *tort_malloc(size_t size)
{
  void *ptr = _tort_malloc(size);
  if ( ! ptr )
    tort_fatal("tort_malloc(%lu): failed", (unsigned long) size);
  if ( ! _tort_gc_mode )
    memset(ptr, 0, size);
  return ptr;
}

void *tort_malloc_atomic(size_t size)
{
  void *ptr = _tort_malloc_atomic(size);
  if ( ! ptr )
    tort_fatal("tort_malloc_atomic(%lu): failed", (unsigned long) size);
  if ( ! _tort_gc_mode )
    memset(ptr, 0, size);
  return ptr;
}

void tort_free(void *ptr)
{
  if ( ! ptr )
    tort_fatal("tort_free(%p): free null", ptr);
  _tort_free(ptr);
}

void *tort_realloc(void *ptr, size_t size)
{
  void *new_ptr = _tort_realloc(ptr, size);
  if ( ! new_ptr )
    tort_fatal("tort_realloc(%p, %lu): failed", (void *) ptr, (unsigned long) size);
  return new_ptr;
}

void *tort_realloc_atomic(void *ptr, size_t size)
{
  void *new_ptr = _tort_realloc_atomic(ptr, size);
  if ( ! new_ptr )
    tort_fatal("tort_realloc_atomic(%p, %lu): failed", (void *) ptr, (unsigned long) size);
  return new_ptr;
}

static void _tort_finalization_proc (void * obj, void * client_data)
{
  if ( ! _tort_gc_mode ) return;
  _tort_gc_finalize_count ++;
  tort_send(tort__s(__finalize), tort_ref_box(obj));
}

tort_v _tort_m_object____register_finalizer(tort_thread_param tort_v rcvr)
{
  if ( _tort_gc_mode ) {
    // fprintf(stderr, "\n  _tort_object___register_finalizer @%p\n", (void*) rcvr);
    GC_register_finalizer(rcvr, _tort_finalization_proc, 0, 0, 0);
  }
  return tort_nil;
}

static
void tort_gc_atexit()
{
#if 0
  fprintf(stderr, "\n  tort_gc_atexit()\n");
  fflush(stderr);
#endif
  tort_gc_collect();
  // tort_gc_dump_stats();
}

tort_v tort_runtime_initialize_malloc()
{
  const char *var;

  _tort_gc_mode = atoi((var = getenv("TORT_GC")) ? var : "1");
  if ( _tort_gc_mode > 0 ) {
    GC_finalize_on_demand = 1;
    _tort_malloc  = GC_malloc;
    _tort_malloc_atomic = GC_malloc_atomic;
    _tort_free    = GC_free;
    _tort_realloc = GC_realloc;
    _tort_realloc_atomic = GC_realloc; /* ??? */
  }

  return 0;
}

tort_v tort_runtime_initialize_gc()
{
  tort_add_method(tort__mt(object), "__finalize",  _tort_m_object__identity);
  atexit(tort_gc_atexit);
  return 0;
}

void tort_gc_dump_stats()
{
  tort_v io = tort_stderr;

  if ( ! _tort_gc_mode ) return;

  tort_flush(tort_stdout);
  tort_flush(tort_stderr);

  tort_flush(io);
  tort_printf(io, "\n");
#define Pf(X) tort_printf(io, "tort gc stats: %24s = %16lu\n", #X, GC_##X())
#define Pl(X) tort_printf(io, "tort gc stats: %24s = %16lu\n", #X, GC_##X)
#include "gc_stats.h"

  tort_flush(io);
}

void tort_gc_collect()
{
  if ( ! _tort_gc_mode ) return;
  GC_gcollect();
  tort_gc_invoke_finalizers();
}

void tort_gc_invoke_finalizers()
{
  if ( ! _tort_gc_mode ) return;
  GC_invoke_finalizers();
}


