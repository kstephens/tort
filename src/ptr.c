#include "tort/core.h"

tort_v tort_ptr_new(void *ptr)
{
  tort_v val = tort_allocate(tort__mt(ptr), sizeof(tort_ptr));
  tort_ptr_data(val) = ptr;
  return val;
}
