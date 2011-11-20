#include "tort/core.h"

tort_v _tort_m_value___applyf(tort_tp tort_v *o, tort_v v)
{
  return _tort_message->method->data;
}
tort_v _tort_m_value__value(tort_tp tort_method *o)
{
  return o->data;
}

tort_v _tort_m_value__value_locative(tort_tp tort_method *o)
{
  return tort_l(&o->data);
}

tort_v _tort_m_value__valueSET(tort_tp tort_method *o, tort_v v)
{
  o->data = v;
  return o;
}

tort_v _tort_m_value__initialize(tort_tp tort_method *o, tort_v v)
{
  o->_h[-1].applyf = (void*) _tort_m_value___applyf;
  o->name = o->data = tort_nil;
  return_tort_send(tort__s(valueSET), o, v);
}

tort_v _tort_M_value__new(tort_tp tort_mtable *mtable, tort_v v)
{
  tort_v o = tort_send(tort__s(_allocate), mtable, tort_i(sizeof(tort_method)));
  return_tort_send(tort__s(initialize), o, v);
}

tort_v tort_runtime_initialize_value()
{
  return 0;
}

