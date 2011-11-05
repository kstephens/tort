#include "tort/core.h"

typedef struct tort_slotted_object { tort_H;
  tort_v _names;
  tort_v _values;
} tort_slotted_object;
tort_h_struct(tort_slotted_object);

tort_SETTER(slotted_object,tort_v,_names);
tort_SETTER(slotted_object,tort_v,_values);

tort_v _tort_m_slotted_object___gc_mark(tort_tp tort_slotted_object *o)
{
  tort_gc_mark(o, o->_names);
  return o->_values;
}

tort_v _tort_m_slotted_object___gc_free(tort_tp tort_slotted_object *o)
{
  return 0;
}

static tort_v _tort_getter__applyf(tort_tp tort_slotted_object *o)
{
  assert(_tort_message->argc >= tort_i(1));
  return tort_vector_data(o->_values)[tort_I(_tort_message->method->data)];
}

static tort_v _tort_setter__applyf(tort_tp tort_slotted_object *o, tort_v v)
{
  assert(_tort_message->argc >= tort_i(2));
  tort_vector_data(o->_values)[tort_I(_tort_message->method->data)] = v;
  return o;
}

tort_v _tort_m_slotted_object___add_slot(tort_tp tort_slotted_object *o, tort_v name, tort_v value)
{
  tort_vi i = o->_names == tort_nil ? 0 : tort_vector_size(o->_names);
  tort_mtable *mtable = tort_h_mtable(o);
  tort_v mmtable = tort_h_mtable(mtable);
  tort_method *m;

  mmtable = mtable->delegate; // ???
  if ( o->_names == tort_nil ) {
    o->_names  = tort_vector_new(&name,  i + 1);
    o->_values = tort_vector_new(&value, i + 1);
  } else {
    tort_send(tort_s(add), o->_names,  name);
    tort_send(tort_s(add), o->_values, value);
  }

  m = tort_method_make(_tort_getter__applyf, tort_i(i));
  // m->name = name;
  tort_send(tort__s(add_method), mtable, name, m);
  // append "=" to name for setter.
  {
    char *setter_str = tort_malloc_atomic(strlen(tort_symbol_charP(name) + 2));
    tort_v setter_name = tort_symbol_make(strcat(strcpy(setter_str, tort_symbol_charP(name)), "="));
    m = tort_method_make(_tort_setter__applyf, tort_i(i));
    // m->name = setter_name;
    tort_send(tort__s(add_method), mtable, setter_name, m);
    tort_free_atomic(setter_str);
  }
  return o;
}

tort_v _tort_m_slotted_object___initialize(tort_tp tort_slotted_object *o)
{
  return o;
}

tort_v _tort_M_slotted_object__new(tort_tp tort_mtable *mtable, ...)
{
  tort_mtable *o_mtable = tort_mtable_create(mtable);
  tort_slotted_object *o = tort_allocate(o_mtable, sizeof(*o));
  va_list vap;

  va_start(vap, mtable);
  o->_names = o->_values = tort_nil;
  switch ( tort_I(_tort_message->argc) ) {
  case 2:
    o->_names = va_arg(vap, tort_v);
    break;
  case 3:
    o->_names = va_arg(vap, tort_v);
    o->_values = va_arg(vap, tort_v);
    break;
  }
  va_end(vap);

  if ( o->_names != tort_nil && tort_h_mtable(o->_values) != tort__mt(vector) ) {
    o->_values = tort_vector_new(0, tort_vector_size(o->_names));
  }

  return o;
  return_tort_send(tort_ta tort__s(_initialize), o);
}

tort_v tort_runtime_initialize_slotted_object()
{
  tort_mtable_make("slotted_object", 0);
  return tort_true;
}

