
#include "tort/tort.h"
#include "gc.h"

void tort_gc_dump_stats()
{
  tort_val io = tort_stderr;

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
  tort_flush(io);

}
