#include "tort/core.h"

tort_SLOT(method,tort_v,name);
tort_SLOT(method,tort_v,data);

tort_method* tort_method_new(void *applyf, tort_v data)
{
  tort_method *meth = tort_allocate(tort__mt(method), sizeof(tort_method));
  meth->_h[-1].applyf = applyf;
  meth->data = data;
  meth->name = 0;
  return meth;
}

/* non-symbol lookup interface. */
tort_v _tort_m_method__lookup(tort_tp tort_method *obj, tort_message *msg)
{
  msg->mtable = tort_h_mtable(obj);
  msg->method = (tort_v) obj;
  return msg;
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

tort_v _tort_offset_getter__applyf(tort_tp void *o)
{
  assert(_tort_message->argc >= tort_i(1));
  return *(tort_v*)(o + tort_I(_tort_message->method->data));
}
tort_method* tort_offset_getter_new(tort_v offset)
{
#define CACHE(N)                                                        \
  int i = tort_I(offset); tort_v m;                                     \
  if ( i < 256 && ! (m = tort_(_m_##N[i])) )                            \
    m = tort_(_m_##N[i]) = tort_method_new(_tort_offset_##N##__applyf, offset); \
  return m
  CACHE(getter);
}

tort_v _tort_offset_setter__applyf(tort_tp void *o, tort_v v)
{
  assert(_tort_message->argc >= tort_i(2));
  *(tort_v*)(o + tort_I(_tort_message->method->data)) = v;
  return o;
}
tort_method* tort_offset_setter_new(tort_v offset)
{
  CACHE(setter);
}

tort_v _tort_offset_locater__applyf(tort_tp void *o)
{
  assert(_tort_message->argc >= tort_i(1));
  return tort_l((tort_v*)(o + tort_I(_tort_message->method->data)));
}
tort_method* tort_offset_locater_new(tort_v offset)
{
  CACHE(locater);
#undef CACHE
}

static struct d_m {
  const char *type;
  const char *mtable;
  const char *name;
  void *func;
} meths[] = {
#define tort_d_m(T,MT,N,F) { #T, #MT, #N, F },
#include "tort/d_m.h"
  { 0 }
};

tort_v _tort_m_initializer__method(tort_tp tort_v init)
{
  int i;
  for ( i = 0; meths[i].func; ++ i ) {
    tort_mtable *mt = tort_mtable_get(meths[i].mtable);
    tort_v sel = tort_symbol_new_encode(meths[i].name);
    if ( meths[i].type[0] == 'M' )
      mt = tort_h_mtable(mt);
    tort_send(tort__s(add_method), mt, sel, tort_method_new(meths[i].func, 0)); 
  }
  return init;
}

