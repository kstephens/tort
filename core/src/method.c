#include "tort/core.h"

tort_ACCESSOR(method,tort_v,name);
tort_ACCESSOR(method,tort_v,data);

tort_method* tort_method_new(void *applyf, tort_v data)
{
  tort_method *meth = tort_allocate(tort__mt(method), sizeof(tort_method));
  meth->_h[-1].applyf = applyf;
  meth->data = data;
  meth->name = 0;
  return meth;
}

tort_v _tort_m_object___applyf(tort_tp tort_v o)
{
  return tort_h(o)->applyf;
}
tort_v _tort_m_object___applyfSET(tort_tp tort_v o, tort_v ptr)
{
  void *p;
  tort_send(tort__s(_ptr_data), ptr, &p);
  return tort_h(o)->applyf = p;
}

tort_method* tort_offset_getter_new(tort_v offset)
{
  return tort_method_new(_tort_offset_getter__applyf, offset);
}
tort_v _tort_offset_getter__applyf(tort_tp void *o)
{
  assert(_tort_message->argc >= tort_i(1));
  return *(tort_v*)(o + tort_I(_tort_message->method->data));
}

tort_method* tort_offset_setter_new(tort_v offset)
{
  return tort_method_new(_tort_offset_setter__applyf, offset);
}
tort_v _tort_offset_setter__applyf(tort_tp void *o, tort_v v)
{
  assert(_tort_message->argc >= tort_i(2));
  *(tort_v*)(o + tort_I(_tort_message->method->data)) = v;
  return o;
}

tort_v _tort_constant_getter__applyf(tort_tp tort_v *o, tort_v v)
{
  return _tort_message->method->data;
}
tort_method* tort_constant_getter_new(tort_v value)
{
  return tort_method_new(_tort_constant_getter__applyf, value);
}

tort_v tort_runtime_initialize_method()
{
  extern int _tort_lookup_trace;
  ++ _tort_lookup_trace;
#define tort_d_m(MT, S, F) \
  fprintf(stderr, "   tort_d_m(%s, %s, %s)\n", #MT, #S, #F); \
  tort_send(tort__s(add_method), MT, S, tort_method_new(F, 0)); 
#include "tort/d_m.h"
  -- _tort_lookup_trace;
  return 0;
}

