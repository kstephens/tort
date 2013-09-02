#include "tort/core.h"

#ifndef TORT_GC_BDW
#define TORT_GC_BDW 0
#endif
#ifndef TORT_GC_SMAL
#define TORT_GC_SMAL 0
#endif

#if TORT_GC_SMAL
#include "smal/smal.h"
#include "smal/roots.h"
#endif

extern int _tort_lookup_trace;
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
#define TORT_GC_STAT(X) (void) ((gc_stats.X), 1)
#else
#define TORT_GC_STAT(X) (void) (1)
#endif

const char *_tort_gc_mode;
static int _tort_alloc_bzero = 1;

static void *(*_tort_malloc)(size_t size) = malloc;
void *tort_malloc(size_t size)
{
  void *ptr = _tort_malloc(size);
  if ( ! ptr )
    tort_fatal(tort_ta "tort_malloc(%lu): failed", (unsigned long) size);
  if ( ! _tort_alloc_bzero )
    bzero(ptr, size);
  TORT_GC_STAT(malloc_n ++);
  TORT_GC_STAT(malloc_bytes += size);
  return ptr;
}

static void *(*_tort_malloc_atomic)(size_t size) = malloc;
void *tort_malloc_atomic(size_t size)
{
  void *ptr = _tort_malloc_atomic(size);
  if ( ! ptr )
    tort_fatal(tort_ta "tort_malloc_atomic(%lu): failed", (unsigned long) size);
  if ( ! _tort_alloc_bzero )
    bzero(ptr, size);
  TORT_GC_STAT(malloc_atomic_n ++);
  TORT_GC_STAT(malloc_atomic_bytes += size);
  return ptr;
}

static void (*_tort_free)(void *ptr) = free;
void tort_free(void *ptr)
{
  if ( ! ptr )
    tort_fatal(tort_ta "tort_free(%p): free null", ptr);
  _tort_free(ptr);
  TORT_GC_STAT(free_n ++);
}

static void *(*_tort_realloc)(void *ptr, size_t size) = realloc;
void *tort_realloc(void *ptr, size_t size)
{
  void *new_ptr = _tort_realloc(ptr, size);
  if ( ! new_ptr )
    tort_fatal(tort_ta "tort_realloc(%p, %lu): failed", (void *) ptr, (unsigned long) size);
  TORT_GC_STAT(realloc_n ++);
  TORT_GC_STAT(realloc_bytes += size);
  return new_ptr;
}

static void *(*_tort_realloc_atomic)(void *ptr, size_t size) = realloc;
void *tort_realloc_atomic(void *ptr, size_t size)
{
  void *new_ptr = _tort_realloc_atomic(ptr, size);
  if ( ! new_ptr )
    tort_fatal(tort_ta "tort_realloc_atomic(%p, %lu): failed", (void *) ptr, (unsigned long) size);
  TORT_GC_STAT(realloc_atomic_n ++);
  TORT_GC_STAT(realloc_atomic_bytes += size);
  return new_ptr;
}

static void (*_tort_free_atomic)(void *ptr) = free;
void tort_free_atomic(void *ptr)
{
  if ( ! ptr )
    tort_fatal(tort_ta "tort_free(%p): free null", ptr);
  _tort_free_atomic(ptr);
  TORT_GC_STAT(free_n ++);
}

static void _tort_finalization_proc (void * obj, void * client_data)
{
  _tort_gc_finalize_count ++;
  TORT_GC_STAT(finalize_n ++);
  // fprintf(stderr, "\n  _tort_finalizer_proc(@%p, @%p)\n", (void*) obj, client_data);
  tort_send(tort__s(__finalize), tort_ref_box(obj + sizeof(tort_header)));
}

#if TORT_GC_BDW
void _tort_gc_register_finalizer_bdw(tort_v obj)
{
  GC_register_finalizer(obj - sizeof(tort_header), _tort_finalization_proc, 0, 0, 0);
}
#endif

static void (*_tort_gc_register_finalizer)(tort_v obj) = 0;
tort_v _tort_m_object____register_finalizer(tort_tp tort_v rcvr)
{
  if ( _tort_gc_register_finalizer ) {
    // fprintf(stderr, "\n  _tort_object___register_finalizer @%p\n", (void*) rcvr);
    _tort_gc_register_finalizer(rcvr);
  }
  TORT_GC_STAT(finalizer_n ++);
  return tort_nil;
}

int _tort_gc_disabled;

static void (*_tort_gc_invoke_finalizers)() = 0;
void tort_gc_invoke_finalizers()
{
  if ( _tort_gc_invoke_finalizers )
    _tort_gc_invoke_finalizers();
}

static void (*_tort_gc_collect)() = 0;
void tort_gc_collect()
{
  if ( _tort_gc_disabled )
    return;
  if ( _tort_gc_collect )
    _tort_gc_collect();
  tort_gc_invoke_finalizers();
}

/********************************************************************/

void tort_gc_mark(tort_v referrer, tort_v referred)
{
  // FIXME
#if TORT_GC_SMAL
  if ( referred != tort_nil && ! tort_taggedQ(referred) ) {
    smal_mark_ptr(referrer != tort_nil ? referrer - sizeof(tort_header) : 0, 
		  referred - sizeof(tort_header));
  }
#endif
}

void tort_gc_mark_range(tort_v referrer, void *b, void *e)
{
  // FIXME
#if TORT_GC_SMAL
  smal_mark_ptr_range(tort_h(referrer), b, e);
#endif
}

void tort_gc_add_root_callback(void (*func)(void *data), void *data)
{
  // FIXME
#if TORT_GC_SMAL
  smal_roots_add_callback(func, data);
#endif
}

/********************************************************************/

#if TORT_GC_SMAL
static int allocs_since_gc;
static int allocs_per_gc;
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
  // fprintf(stderr, "  mark_roots: stack [@%p,@%p)\n", thr->top_of_stack, thr->bottom_of_stack);
  smal_mark_ptr_range(0, thr->top_of_stack, thr->bottom_of_stack);
  smal_mark_ptr(0, _tort);
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

static void _tort_gc_stats_smal(tort_v map)
{
  smal_stats stats = { 0 };
  int i;
  smal_global_stats(&stats);
  for ( i = 0; smal_stats_names[i]; ++ i ) {
    tort_send(tort__s(set), map, tort_symbol_new(smal_stats_names[i]), tort_i(((size_t*) &stats)[i]));
  }
}

static void _tort_gc_collect_smal()
{
  smal_collect();
  smal_collect_wait_for_sweep();
  tort_gc_dump_stats();
}

static void *mark_obj(void *ptr)
{
  tort_v obj = ptr + sizeof(tort_header);
  // fprintf(stderr, "  %p mark %p %s [%p-%p]\n", &obj, ptr, tort_object_name(obj), obj, obj + tort_h(obj)->alloc_size);
  smal_mark_ptr_range(ptr, obj, obj + tort_h_mtable(obj)->instance_size);
  if ( tort_h_mtable(obj)->gc_mark_method != tort_true ) {
    tort_v result;
    // _tort_lookup_trace ++;
    result = tort_send(tort__s(_gc_mark), obj);
    // _tort_lookup_trace --;
    smal_mark_ptr(ptr, result);
  }
  return ((tort_v*) obj)[-1] - sizeof(tort_header); // obj->mtable.
}
static void free_obj(void *ptr)
{
  tort_v obj = ptr + sizeof(tort_header);
  fprintf(stderr, "  %p free %p %s\n", &obj, ptr, tort_object_name(obj));
  if ( tort_h_mtable(obj)->gc_free_method != tort_true ) {
    // _tort_lookup_trace ++;
    tort_send(tort__s(_gc_free), obj);
    // _tort_lookup_trace --;
  }
}
static void *_type_for_size(size_t size)
{
  smal_type_descriptor desc = { 0 };
  desc.object_size = size;
  desc.object_alignment = sizeof(tort_v);
  desc.mark_func = mark_obj;
  desc.free_func = free_obj;
  return smal_type_for_desc(&desc);
}

static void *_tort_object_alloc_smal(tort_mtable *mtable, size_t size)
{
  if ( mtable ) {
    if ( ! mtable->gc_data )
      mtable->gc_data = _type_for_size(size);
    if ( ++ allocs_since_gc > allocs_per_gc ) {
      tort_gc_collect();
      allocs_since_gc = 0;
    }
    return smal_alloc(mtable->gc_data);
  }
  return smal_alloc(_type_for_size(size));
}
#endif

static void *_tort_object_alloc_default(tort_mtable *mtable, size_t size)
{
  return tort_malloc(size);
}

static void *(*_tort_object_alloc)() = _tort_object_alloc_default;
void *tort_object_alloc(tort_mtable *mtable, size_t size)
{
  void *ptr;
  size_t alloc_size = sizeof(tort_header) + size;

  /* Save the instance size in the mtable. */
  if ( mtable ) {
    if ( ! mtable->instance_size )
      mtable->instance_size = size;
    assert(size >= mtable->instance_size);
  } else {
    // fprintf(stderr, "  tort_object_alloc: no mtable for %lu\n", (unsigned long) alloc_size);
  }
  ptr = _tort_object_alloc(mtable, alloc_size);
  if ( ptr ) {
    extern unsigned long _tort_alloc_id;
    if ( ! _tort_alloc_bzero )
      bzero(ptr, alloc_size);
    ptr += sizeof(tort_header);
    tort_h_ref(ptr)->mtable = mtable;
    tort_h_ref(ptr)->applyf = _tort_m_object___cannot_apply;
    ++ _tort_alloc_id;
    // fprintf(stderr, "  alloc %p[%llu] %s %lu\n", ptr, (unsigned long long) size, tort_object_name(mtable), _tort_alloc_id);
    TORT_GC_STAT(object_alloc_n ++);
    TORT_GC_STAT(object_alloc_bytes += alloc_size);
  }
  return ptr;
}

#if TORT_GC_BDW
static void _tort_gc_stats_bdw(tort_v map)
{
#define Pf(X) tort_send(tort__s(set), map, tort_s(bdw_##X), tort_i(GC_##X()));
#define Pl(X) tort_send(tort__s(set), map, tort_s(bdw_##X), tort_i(GC_##X));
#include "gc_stats.h"
}
#endif

static void (*_tort_gc_stats)(tort_v map) = 0;
void tort_gc_stats(tort_v map)
{
#if TORT_GC_STATS
#define S(N) \
  tort_send(tort__s(set), map, tort_s(tort_##N), tort_i(gc_stats.N));
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
  if ( _tort_gc_stats )
    _tort_gc_stats(map);
}

void tort_gc_dump_stats()
{
  tort_v io = tort_stderr;
  tort_v map;
  tort_flush(tort_stdout);
  tort_flush(tort_stderr);
  tort_printf(io, "  tort GC stats:\n");
  map = tort_map_new();
  tort_gc_stats(map);
  tort_map_EACH(map, e); {
    tort_printf(io, "    %T = %T\n", e->first, e->second);
  } tort_map_EACH_END();
  tort_printf(io, "\n");
  tort_flush(io);
}

tort_v _tort_m_initializer__malloc(tort_tp tort_v init)
{
  const char *var;
  _tort_gc_disabled ++; // temporarily disabled till boot is finished.

  (void) _tort_finalization_proc; // avoid warning.

  var = getenv("TORT_GC");
  if ( ! var || ! *var || ! strcmp(var, "0") ) var = "bdw";

#if TORT_GC_BDW
  if ( ! strcmp(var, "bdw") ) {
    GC_set_all_interior_pointers(1);
    GC_set_finalize_on_demand(0);
    GC_INIT();
    _tort_gc_mode = "bdw";
    _tort_malloc  = GC_malloc;
    _tort_malloc_atomic = GC_malloc_atomic;
    _tort_free = GC_free;
    _tort_free_atomic = GC_free; /* ??? */
    _tort_realloc = GC_realloc;
    _tort_realloc_atomic = GC_realloc; /* ??? */
    _tort_gc_collect = GC_gcollect;
    _tort_gc_register_finalizer = _tort_gc_register_finalizer_bdw;
    _tort_gc_invoke_finalizers = (void*) GC_invoke_finalizers;
    _tort_gc_stats = _tort_gc_stats_bdw;
  }
#endif
#if TORT_GC_SMAL
  if ( ! strcmp(var, "smal") ) {
    _tort_gc_mode = "smal";
    smal_debug_set_level(smal_debug_all, 1);
    smal_init();
    _tort_gc_collect = _tort_gc_collect_smal;
    _tort_object_alloc = _tort_object_alloc_smal;
    _tort_gc_stats = _tort_gc_stats_smal;
    {
      const char *s = getenv("TORT_GC_ALLOCS_PER_GC");
      allocs_per_gc = s && *s ? atoi(s) : 10000;
    }
  }
#endif
  if ( ! strcmp(var, "malloc") )
    _tort_gc_mode = "malloc";

  if ( ! _tort_gc_mode ) {
    _tort_gc_mode = "malloc";
    fprintf(stderr, "  tort: WARNING: defaulting to TORT_GC=%s\n", _tort_gc_mode);
  }

  if ( ! strcmp(_tort_gc_mode, "malloc") )
    fprintf(stderr, "  tort: WARNING: using malloc(), NO GC!\n");

  { /* assert alignments. */
    void *p;
    assert((size_t) (p = tort_malloc(sizeof(tort_header))) % sizeof(tort_v) == 0);
    tort_free(p);
    assert((size_t) (p = tort_malloc_atomic(sizeof(tort_header))) % sizeof(tort_v) == 0);
    tort_free_atomic(p);
  }
  return init;
}

static void tort_gc_atexit()
{
  if ( getenv("TORT_GC_STATS") ) {
    fprintf(stderr, "  tort_gc_atexit(): before atexit\n");
    tort_gc_dump_stats();
  }
  tort_gc_collect();
  if ( getenv("TORT_GC_STATS") ) {
    tort_gc_dump_stats();
  }
}

tort_v _tort_M_gc__gc_collect(tort_tp tort_mtable *o)
{
  tort_gc_collect();
  return o;
}

tort_v _tort_M_gc__gc_stats(tort_tp tort_mtable *o)
{
  tort_v map = tort_map_new();
  tort_gc_stats(map);
  return map;
}

tort_v _tort_M_gc__mark(tort_tp tort_mtable *o, tort_v object)
{
  tort_gc_mark(0, object);
  return o;
}

tort_v _tort_m_initializer__gc(tort_tp tort_v init)
{
  tort_add_method(tort__mt(object), "__finalize",  _tort_m_object__identity);
  return init;
}

tort_v _tort_m_initializer__gc_ready(tort_tp tort_v init)
{
  assert(_tort_gc_disabled);
  _tort_gc_disabled --;
  atexit(tort_gc_atexit);
  return init;
}
