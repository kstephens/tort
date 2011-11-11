#include "tort/core.h"
#include "smal/smal.h"
#include "smal/roots.h"

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
    ,object_alloc_n
    ,object_alloc_bytes
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

int _tort_gc_disabled;

static void (*_tort_gc_collect)() = 0;
void tort_gc_collect()
{
  if ( _tort_gc_disabled )
    return;
  if ( _tort_gc_collect )
    _tort_gc_collect();
  tort_gc_invoke_finalizers();
}

static void (*_tort_gc_invoke_finalizers)() = 0;
void tort_gc_invoke_finalizers()
{
  if ( _tort_gc_invoke_finalizers )
    _tort_gc_invoke_finalizers();
}

/********************************************************************/

void smal_collect_before_inner(void *top_of_stack)
{
  smal_thread *thr = smal_thread_self();
  thr->top_of_stack = top_of_stack;
  thr->bottom_of_stack = tort_(stack_bottom);
}
void smal_collect_before_mark()
{
}
void smal_collect_mark_roots()
{
  smal_thread *thr = smal_thread_self();
  fprintf(stderr, "  mark_roots: stack [@%p,@%p)\n", thr->top_of_stack, thr->bottom_of_stack);
  smal_mark_ptr_range(0, thr->top_of_stack, thr->bottom_of_stack);
  smal_mark_ptr_range(0, _tort, _tort + 1);
  smal_roots_mark_chain();
}
void smal_collect_after_mark()
{
}
void smal_collect_before_sweep()
{
}
void smal_collect_after_sweep()
{
}

void tort_gc_mark(tort_v referrer, tort_v referred)
{
  if ( referred != tort_nil && ! tort_taggedQ(referred) ) {
    smal_mark_ptr(referrer - sizeof(tort_header), 
		  referred - sizeof(tort_header));
  }
}

void tort_gc_mark_range(void *b, void *e)
{
  smal_mark_ptr_range(0, b, e);
}

void tort_gc_add_callback(void (*func)(void *data), void *data)
{
  smal_roots_add_callback(func, data);
}

static
void print_smal_stats(const char *msg)
{
  smal_stats stats = { 0 };
  int i;
  
  smal_global_stats(&stats);
  fprintf(stderr, "\n  smal stats %s:\n", msg ? msg : "");
  for ( i = 0; smal_stats_names[i]; ++ i ) {
    fprintf(stderr, "    %24llu %s\n", (unsigned long long) (((size_t*) &stats)[i]), smal_stats_names[i]);
  }
  fprintf(stderr, "\n");
}

static
void _smal_gc_collect()
{
  smal_collect();
  smal_collect_wait_for_sweep();
  print_smal_stats("after gc");
}

extern int _tort_lookup_trace;

static
void *mark_obj(void *ptr)
{
  tort_v obj = ptr + sizeof(tort_header);
  fprintf(stderr, "  %p mark %p %s [%p-%p]\n", &obj, ptr, tort_object_name(obj), obj, obj + tort_h(obj)->alloc_size);
  smal_mark_ptr_range(ptr, obj, obj + tort_h(obj)->alloc_size);
  if ( tort_h_mtable(obj)->gc_mark_method != tort_true ) {
    tort_v result;
    // _tort_lookup_trace ++;
    result = tort_send(tort__s(_gc_mark), obj);
    // _tort_lookup_trace --;
    smal_mark_ptr(ptr, result);
  }
  return ((tort_v*) obj)[-1]; // obj->mtable.
}
static
void free_obj(void *ptr)
{
  tort_v obj = ptr + sizeof(tort_header);
  fprintf(stderr, "  %p free %p %s\n", &obj, ptr, tort_object_name(obj));
  if ( tort_h_mtable(obj)->gc_free_method != tort_true ) {
    // _tort_lookup_trace ++;
    tort_send(tort__s(_gc_free), obj);
    // _tort_lookup_trace --;
  }
}
static
void *_type_for_size(size_t size)
{
  smal_type_descriptor desc = { 0 };
  desc.object_size = size;
  desc.object_alignment = sizeof(tort_v);
  desc.mark_func = mark_obj;
  desc.free_func = free_obj;
  return smal_type_for_desc(&desc);
}
static
void *_tort_object_alloc_default(tort_mtable *mtable, size_t size)
{
  if ( mtable ) {
    if ( ! mtable->gc_data ) {
      mtable->gc_data = _type_for_size(size);
    }
    if ( gc_stats.object_alloc_n % 10000 == 0 ) {
      tort_gc_collect();
    }
    return smal_alloc(mtable->gc_data);
  }
  return smal_alloc(_type_for_size(size));
  return tort_malloc(size);
}
static void *(*_tort_object_alloc)() = _tort_object_alloc_default;
void *tort_object_alloc(tort_mtable *mtable, size_t size)
{
  void *ptr;
  size_t alloc_size = sizeof(tort_header) + size;

  /* Save the instance size in the mtable. */
  if ( mtable ) {
    if ( ! mtable->instance_size ) {
      mtable->instance_size = size;
    }
    assert(mtable->instance_size == size);
  } else {
    fprintf(stderr, "  tort_object_alloc: no mtable for %lu\n", (unsigned long) alloc_size);
  }
  ptr = _tort_object_alloc(mtable, alloc_size);
  if ( ptr ) {
    extern unsigned long _tort_alloc_id;
    ptr += sizeof(tort_header);
    tort_h_ref(ptr)->alloc_size = size;
    tort_h_ref(ptr)->mtable = mtable;
    ++ _tort_alloc_id;
    fprintf(stderr, "  alloc %p[%llu] %s %lu\n", ptr, (unsigned long long) size, tort_object_name(mtable), _tort_alloc_id);
    TORT_GC_STAT(object_alloc_n ++);
    TORT_GC_STAT(object_alloc_bytes += alloc_size);
#if 0
    if ( gc_stats.object_alloc_n % 10000 == 0 ) {
      print_smal_stats(0);
    }
#endif
  }
  return ptr;
}

tort_v tort_runtime_initialize_malloc()
{
  const char *var;
  _tort_gc_disabled ++;
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
  smal_debug_set_level(smal_debug_all, 1);
  smal_init();
  _tort_gc_collect = _smal_gc_collect;
  return 0;
}

static
void tort_gc_atexit()
{
#if 0
  fprintf(stderr, "\n  tort_gc_atexit()\n");
  fflush(stderr);
#endif
  print_smal_stats("before atexit");
  tort_gc_collect();
  if ( getenv("TORT_GC_STATS") ) {
    tort_gc_dump_stats();
  }
  print_smal_stats("after atexit gc");
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
  S(object_alloc_n);
  S(object_alloc_bytes);
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

tort_v tort_runtime_initialize_gc()
{
  tort_add_method(tort__mt(object), "__finalize",  _tort_m_object__identity);
  atexit(tort_gc_atexit);
  return 0;
}

tort_v tort_runtime_initialize_gc_ready()
{
  assert(_tort_gc_disabled);
  _tort_gc_disabled --;
  return 0;
}
