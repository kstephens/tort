#include "tort/core.h"


/********************************************************************/


size_t _tort_gc_finalize_count = 0;


/********************************************************************/


static
void _tort_finalization_proc (void * obj, void * client_data)
{
  _tort_gc_finalize_count ++;
  tort_send(tort__s(__finalize), tort_ref_box(obj));
}


tort_v _tort_object___register_finalizer(tort_v _tort_message, tort_v rcvr)
{
  // fprintf(stderr, "\n  _tort_object___register_finalizer @%p\n", (void*) rcvr);
  GC_register_finalizer(rcvr, _tort_finalization_proc, 0, 0, 0);
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


void tort_runtime_initialize_gc()
{
  GC_finalize_on_demand = 1;

  tort__s(__finalize) = tort_symbol_make("__finalize");
  tort__s(__register_finalizer) = tort_symbol_make("__register_finalizer");

  tort_add_method(tort__mt(object), "__finalize",  _tort_object_identity);
  tort_add_method(tort__mt(object), "__register_finalizer",  _tort_object___register_finalizer);

  atexit(tort_gc_atexit);
}


void tort_gc_dump_stats()
{
  tort_v io = tort_stderr;

  tort_flush(tort_stdout);
  tort_flush(tort_stderr);

  tort_flush(io);
  tort_printf(io, "\n");
#define P(X) tort_printf(io, "tort gc stats: %24s = %16lu\n", #X, GC_##X())
  P(get_heap_size);
  P(get_free_bytes);
  P(get_bytes_since_gc);
  P(get_total_bytes);
#undef P
#define P(X) tort_printf(io, "tort gc stats: %24s = %16lu\n", #X, GC_##X)
  P(gc_no);
  P(parallel);
  P(all_interior_pointers);
  P(finalize_on_demand);
  P(java_finalization);
  P(dont_gc);
  P(dont_expand);
  P(use_entire_heap);
  P(full_freq);
  P(non_gc_bytes);
  P(no_dls);
  P(free_space_divisor);
  P(max_retries);
  P(dont_precollect);
#undef P

  tort_flush(io);
}


void tort_gc_collect()
{
  GC_gcollect();
  tort_gc_invoke_finalizers();
}


void tort_gc_invoke_finalizers()
{
  GC_invoke_finalizers();
}


