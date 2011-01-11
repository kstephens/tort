#include <stdlib.h>

#if ! TORT_GC

void *GC_malloc(size_t size)
{
  return malloc(size);
}
void *GC_realloc(void *ptr, size_t size)
{
  return realloc(ptr, size);
}
void GC_free(void *ptr)
{
  free(ptr);
}
void GC_gcollect()
{
}
void GC_register_finalizer(void *ptr, ...)
{
}
void GC_invoke_finalizers()
{
}

#define Pf(x) int GC_##x() { return 0; }
#define Pl(x) int GC_##x;
#include "gc_stats.h"

#endif
