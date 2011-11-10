#include "tort/core.h"

size_t _tort_gc_finalize_count = 0;

static
struct {
  unsigned long 
     malloc_n
    ,malloc_bytes
    ,realloc_n
    ,realloc_bytes
    ,free_n
    ,malloc_atomic_n
    ,malloc_atomic_bytes
    ,realloc_atomic_n
    ,realloc_atomic_bytes
    ,free_atomic_n
    ,finalizer_n
    ,finalize_n
    ;
} gc_stats;

#if TORT_GC_STATS
#define TORT_GC_STAT(X) ((gc_stats.X), 1)
#else
#define TORT_GC_STAT(X) (1)
#endif

int _tort_gc_mode = 0;

static
void *(*_tort_malloc)(size_t size) = malloc;
void *tort_malloc(size_t size)
{
  void *ptr = _tort_malloc(size);
  if ( ! ptr )
    tort_fatal(tort_ta "tort_malloc(%lu): failed", (unsigned long) size);
  if ( ! _tort_gc_mode )
    memset(ptr, 0, size);
  TORT_GC_STAT(malloc_n ++);
  TORT_GC_STAT(malloc_bytes += size);
  return ptr;
}

static
void *(*_tort_malloc_atomic)(size_t size) = malloc;
void *tort_malloc_atomic(size_t size)
{
  void *ptr = _tort_malloc_atomic(size);
  if ( ! ptr )
    tort_fatal(tort_ta "tort_malloc_atomic(%lu): failed", (unsigned long) size);
  if ( ! _tort_gc_mode )
    memset(ptr, 0, size);
  TORT_GC_STAT(malloc_atomic_n ++);
  TORT_GC_STAT(malloc_atomic_bytes += size);
  return ptr;
}

static
void (*_tort_free)(void *ptr) = free;
void tort_free(void *ptr)
{
  if ( ! ptr )
    tort_fatal(tort_ta "tort_free(%p): free null", ptr);
  _tort_free(ptr);
  TORT_GC_STAT(free_n ++);
}

static
void *(*_tort_realloc)(void *ptr, size_t size) = realloc;
void *tort_realloc(void *ptr, size_t size)
{
  void *new_ptr = _tort_realloc(ptr, size);
  if ( ! new_ptr )
    tort_fatal(tort_ta "tort_realloc(%p, %lu): failed", (void *) ptr, (unsigned long) size);
  TORT_GC_STAT(realloc_n ++);
  TORT_GC_STAT(realloc_bytes += size);
  return new_ptr;
}

static
void *(*_tort_realloc_atomic)(void *ptr, size_t size) = realloc;
void *tort_realloc_atomic(void *ptr, size_t size)
{
  void *new_ptr = _tort_realloc_atomic(ptr, size);
  if ( ! new_ptr )
    tort_fatal(tort_ta "tort_realloc_atomic(%p, %lu): failed", (void *) ptr, (unsigned long) size);
  TORT_GC_STAT(realloc_atomic_n ++);
  TORT_GC_STAT(realloc_atomic_bytes += size);
  return new_ptr;
}

static
void (*_tort_free_atomic)(void *ptr) = free;
void tort_free_atomic(void *ptr)
{
  if ( ! ptr )
    tort_fatal(tort_ta "tort_free(%p): free null", ptr);
  _tort_free_atomic(ptr);
  TORT_GC_STAT(free_n ++);
}

static void _tort_finalization_proc (void * obj, void * client_data)
{
  if ( ! _tort_gc_mode ) return;
  _tort_gc_finalize_count ++;
  TORT_GC_STAT(finalize_n ++);
  tort_send(tort__s(__finalize), tort_ref_box(obj));
}

tort_v _tort_m_object____register_finalizer(tort_tp tort_v rcvr)
{
  if ( _tort_gc_mode ) {
    // fprintf(stderr, "\n  _tort_object___register_finalizer @%p\n", (void*) rcvr);
    GC_register_finalizer(rcvr, _tort_finalization_proc, 0, 0, 0);
    TORT_GC_STAT(finalizer_n ++);
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
  if ( getenv("TORT_GC_STATS") )
    tort_gc_dump_stats();
}

void tort_gc_dump_stats()
{
  tort_v io = tort_stderr;
  tort_flush(tort_stdout);
  tort_flush(tort_stderr);
  tort_printf(io, "\n");
  if ( _tort_gc_mode ) {
#define Pf(X) tort_printf(io, "tort: gc stats: %26s = %16lu\n", #X, GC_##X())
#define Pl(X) tort_printf(io, "tort: gc stats: %26s = %16lu\n", #X, GC_##X)
#include "gc_stats.h"
  }
#if TORT_GC_STATS
#define S(N) \
  tort_printf(io, "tort: gc stats: %26s = %16lu\n", #N, (unsigned long) gc_stats.N)
  S(malloc_n);
  S(malloc_bytes);
  S(realloc_n);
  S(realloc_bytes);
  S(malloc_atomic_n);
  S(malloc_atomic_bytes);
  S(realloc_atomic_n);
  S(realloc_atomic_bytes);
  S(free_n);
  S(free_atomic_n);
  S(finalizer_n);
  S(finalize_n);
#undef S
#endif
  tort_flush(io);
}

static void (*_tort_gc_collect)() = 0;
void tort_gc_collect()
{
  if ( _tort_gc_collect )
    _tort_gc_collect();
  tort_gc_invoke_finalizers();
}

static void (*_tort_gc_invoke_finalizers)() = 0;
void tort_gc_invoke_finalizers()
{
  if ( ! _tort_gc_invoke_finalizers )
    _tort_gc_invoke_finalizers();
}

tort_v tort_runtime_initialize_malloc()
{
  const char *var;
  _tort_gc_mode = atoi((var = getenv("TORT_GC")) ? var : "0"); // BOEHM GC is broken - kurt 2011/10/30
  if ( _tort_gc_mode > 0 ) {
    GC_finalize_on_demand = 1;
    _tort_malloc  = GC_malloc;
    _tort_malloc_atomic = GC_malloc_atomic;
    _tort_free = GC_free;
    _tort_free_atomic = GC_free; /* ??? */
    _tort_realloc = GC_realloc;
    _tort_realloc_atomic = GC_realloc; /* ??? */
    _tort_gc_collect = GC_gcollect;
    _tort_gc_invoke_finalizers = (void*) GC_invoke_finalizers;
  }
  return 0;
}

tort_v tort_runtime_initialize_gc()
{
  tort_add_method(tort__mt(object), "__finalize",  _tort_m_object__identity);
  atexit(tort_gc_atexit);
  return 0;
}
