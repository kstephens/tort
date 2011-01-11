#include "tort/core.h"


/********************************************************************/

tort_v _tort_m_string__get (tort_thread_param tort_string *rcvr, tort_v _i)
{
  long i = tort_I(_i);
  return tort_i(rcvr->data[i]);
}


tort_v _tort_m_string__set (tort_thread_param tort_string *rcvr, tort_v _i, tort_v _v)
{
  long i = tort_I(_i);
  long v = tort_I(_v);
  rcvr->data[i] = v;
  return rcvr;
}


tort_v _tort_M_string___new(tort_tp tort_mtable *mtable, const char *string, size_t size)
{
  tort_string *v = tort_vector_base_new(mtable, string, size, sizeof(string[0]));
  if ( ! string ) {
    memset(v->data, 0, v->alloc_size);
  }
  return v;
}

tort_v _tort_M_string__new(tort_tp tort_mtable *mtable, tort_v size)
{
  return tort_send(tort__s(_new), mtable, 0, tort_I(size));
}

/********************************************************************/


tort_v tort_string_new(const char *ptr, size_t size)
{
  return _tort_M_string___new(tort_ta tort__mt(string), ptr, size);
}


tort_v tort_string_new_cstr(const char *string)
{
  return tort_string_new(string, strlen(string));
}


