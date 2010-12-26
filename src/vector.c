#include "tort/core.h"


/********************************************************************/


tort_v tort_vector_new(const tort_v *vec, size_t size)
{
  size_t alloc_size;
  tort_v val = tort_allocate(0, 0, sizeof(tort_vector), tort__mt(vector));
  alloc_size = sizeof(tort_ref(tort_vector, val)->data[0]) * (size);
  tort_ref(tort_vector, val)->size = 
  tort_ref(tort_vector, val)->alloc_size = 
    size;
  tort_ref(tort_vector, val)->data = tort_malloc(alloc_size);
  if ( vec ) {
    memcpy(tort_ref(tort_vector, val)->data, vec, alloc_size);
  } else {
    size_t i;

    for ( i = 0; i < size; ++ i ) {
      tort_vector_data(val)[i] = tort_nil;
    }
  }
  return val;
}


tort_v _tort_m_vector__new(tort_thread_param tort_v rcvr, tort_v _size)
{
  return tort_vector_new(0, tort_I(_size));
}


tort_v _tort_m_vector__clone (tort_thread_param tort_v rcvr)
{
  tort_v val = _tort_m_object__clone(tort_thread_arg rcvr);
  tort_vector_data(val) = tort_malloc(sizeof(tort_vector_data(val)[0]) * (tort_vector_size(rcvr)));
  memcpy(tort_vector_data(val), tort_vector_data(rcvr), sizeof(tort_vector_data(val)[0]) * (tort_vector_size(rcvr)));
  return val;
}


tort_v _tort_m_vector__get (tort_thread_param tort_v rcvr, tort_v _i)
{
  long i = tort_I(_i);
  return tort_vector_data(rcvr)[i];
}


tort_v _tort_m_vector__set (tort_thread_param tort_v rcvr, tort_v _i, tort_v _v)
{
  long i = tort_I(_i);
  tort_vector_data(rcvr)[i] = _v;
  return rcvr;
}


tort_v _tort_m_vector__size (tort_thread_param tort_v rcvr)
{
  return tort_i(tort_vector_size(rcvr));
}


tort_v _tort_m_vector__alloc_size (tort_thread_param tort_v rcvr)
{
  return tort_i(tort_vector_alloc_size(rcvr));
}


tort_v _tort_m_vector__each (tort_thread_param tort_v rcvr, tort_v block)
{
  tort_vector_loop(rcvr, x) {
    tort_send(tort__s(value), block, x);
  }
  tort_vector_loop_end(rcvr);
  return rcvr;
}


tort_v _tort_m_vector__map (tort_thread_param tort_v rcvr, tort_v block)
{
  tort_v new_vec = tort_send(tort__s(clone), rcvr);
  tort_vector_loop(rcvr, x) {
    x = tort_send(tort__s(value), block, x);
    tort_send(tort__s(set), new_vec, tort_i(x_i), x);
#if 0
    tort_printf(tort_stderr, 
		"  tort_v_m(%p(%p) => %p(%p)[%d] => %T\n", 
		rcvr, tort_vector_data(rcvr),
		new_vec, tort_vector_data(new_vec),
		x_i, 
		tort_vector_data(new_vec)[x_i]);
#endif
 }
  tort_vector_loop_end(rcvr);
  return new_vec;
}


/********************************************************************/


