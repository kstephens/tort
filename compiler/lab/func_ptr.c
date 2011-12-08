#include "tort/tort.h"
#include <stdarg.h>

tort_v _tort_return_func_ptr()
{
  return tort_p(printf);
}

static
void local_function()
{
}

tort_v _tort_return_local_func_ptr()
{
  return tort_p(local_function);
}
