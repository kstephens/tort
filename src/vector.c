#include "tort/core.h"
#include <assert.h>


tort_v tort_vector_base_new(tort_v mtable, const void *data, size_t size, size_t element_size)
{
  tort_vector_base *v = tort_allocate(0, 0, sizeof(tort_vector_base), mtable);
  _tort_m_vector_base___initialize(tort_thread_arg v, size, element_size);
  if ( data ) {
    memcpy(v->data, data, v->element_size * v->size);
  }
  return v;
}


tort_v _tort_m_vector_base___initialize(tort_thread_param tort_vector_base *v, size_t size, size_t element_size)
{
  v->data = tort_malloc(
			v->alloc_size = 
			(v->element_size = element_size) *
			((v->size = size) + 1) /* + 1 null terminator */
			); 
  memset(v->data, 0, v->alloc_size); /* not if GC_malloc() */
  return (tort_v) v;
}


tort_v _tort_m_vector_base__clone (tort_thread_param tort_vector_base *v)
{
  tort_vector_base *v2 = _tort_m_object__clone(tort_thread_arg v);
  memcpy(v2->data = tort_malloc(v2->alloc_size), v->data, v2->element_size * (v2->size + 1));
  return (tort_v) v2;
}


void* _tort_m_vector_base___data (tort_thread_param tort_vector_base *v)
{
  return v->data;
}


void* _tort_m_vector_base___ref (tort_thread_param tort_vector_base *v, tort_v i)
{
  return v->data + v->element_size * tort_I(i);
}


tort_v _tort_m_vector_base__size (tort_thread_param tort_vector_base *v)
{
  return tort_i(v->size);
}


tort_v _tort_m_vector_base__alloc_size (tort_thread_param tort_vector_base *v)
{
  return tort_i(v->alloc_size);
}


tort_v _tort_m_vector_base__element_size (tort_thread_param tort_vector_base *v)
{
  return tort_i(v->element_size);
}


tort_v _tort_m_vector_base___resize (tort_thread_param tort_vector_base *v, size_t size)
{
  size_t old_size = v->size;
  size_t old_alloc_size = v->alloc_size;
  if ( size > old_size || size < old_size / 2 ) {
    v->alloc_size = v->element_size * (size + 1); /* + 1 null terminator */
    v->data = 
      old_alloc_size ? tort_realloc(v->data, v->alloc_size) :
      tort_malloc(v->alloc_size);
  }
  return (tort_v) v;
}


tort_v _tort_m_vector_base___append (tort_thread_param tort_vector_base *v, const void *datap, size_t data_count)
{
  size_t size = v->size;
  _tort_m_vector_base___resize(tort_thread_arg v, size + data_count);
  memcpy(v->data + v->element_size * size,
	 datap,
	 v->element_size * data_count);
  v->size = size += data_count;
  memset(v->data + v->element_size * size, 0, v->element_size);
  return (tort_v) v;
}


tort_v _tort_m_vector_base__append (tort_thread_param tort_vector_base *v, tort_v other)
{
  void *data = tort_send(tort__s(_data), other);
  size_t size = tort_I(tort_send(tort__s(size), other));
  assert(v != other);
  return _tort_m_vector_base___append(tort_thread_arg v, data, size); 
}


tort_v _tort_m_vector_base___add (tort_thread_param tort_vector_base *v, const void *datap)
{
  return _tort_m_vector_base___append(tort_thread_arg v, datap, 1);
}


/********************************************************************/

tort_v tort_vector_new(const tort_v *vec, size_t size)
{
  tort_v val = tort_vector_base_new(tort__mt(vector), vec, size, sizeof(tort_v));
  if ( ! vec ) {
    size_t i;
    for ( i = 0; i < (size + 1); ++ i ) {
      tort_vector_data(val)[i] = tort_nil;
    }
  }
  return val;
}


tort_v _tort_m_vector__new(tort_thread_param tort_v rcvr, tort_v _size)
{
  return tort_vector_new(0, tort_I(_size));
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


