#include "tort/core.h"


/********************************************************************/


tort_v _tort_string_new(tort_v _tort_message, tort_v rcvr, tort_v size)
{
  return tort_string_new(0, tort_I(size));
}


tort_v _tort_string_clone (tort_v _tort_message, tort_v rcvr)
{
  tort_v val = _tort_object_clone(_tort_message, rcvr);
  tort_string_data(val) = tort_malloc(sizeof(tort_string_data(val)[0]) * (tort_string_size(val) + 1));
  memcpy(tort_string_data(val), tort_string_data(rcvr), sizeof(tort_string_data(val)[0]) * (tort_string_size(val) + 1));
  return val;
}


tort_v _tort_string_get (tort_v _tort_message, tort_v rcvr, tort_v _i)
{
  long i = tort_I(_i);
  return tort_i(tort_string_data(rcvr)[i]);
}


tort_v _tort_string_set (tort_v _tort_message, tort_v rcvr, tort_v _i, tort_v _v)
{
  long i = tort_I(_i);
  long v = tort_I(_v);
  tort_string_data(rcvr)[i] = v;
  return rcvr;
}


/********************************************************************/


tort_v tort_string_new(const char *string, size_t size)
{
  size_t alloc_size;
  tort_v val = tort_allocate(0, 0, sizeof(tort_string), _tort->_mt_string);
  alloc_size = sizeof(tort_ref(tort_string, val)->data[0]) * (size + 1);
  tort_ref(tort_string, val)->size = 
  tort_ref(tort_string, val)->alloc_size = 
    size;
  tort_ref(tort_string, val)->data = tort_malloc(alloc_size);
  if ( string ) {
    memcpy(tort_ref(tort_string, val)->data, string, alloc_size);
    tort_ref(tort_string, val)->data[size] = 0;
  } else {
    memset(tort_ref(tort_string, val)->data, 0, alloc_size);
  }
  // fprintf(stderr, "\n new string = \"%s\" %p\n", tort_ref(tort_string, val)->data, (void *) val);
  return val;
}


tort_v tort_string_new_cstr(const char *string)
{
  return tort_string_new(string, strlen(string));
}


