#include "tort/tort.h"

tort_v _tort_return_func_call()
{
  return (tort_v) printf("Hello, World!");
}

void local_function()
{
}

tort_v _tort_return_local_func_ptr()
{
  local_function();
  return 0;
}
