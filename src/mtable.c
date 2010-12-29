#include "tort/core.h"


/********************************************************************/

tort_mtable* tort_mtable_set_delegate(tort_mtable *obj_mt, tort_v delegate)
{
  tort_mtable *cls_mt;

  if ( delegate == 0 && tort_nil != 0 ) {
    delegate = tort_nil;
  }
  obj_mt->delegate = delegate;

  cls_mt = tort_h_ref(obj_mt)->mtable;
  cls_mt->delegate = delegate ? tort_h_ref(delegate)->mtable : tort__mt(mtable);

  return obj_mt;
}

tort_mtable* tort_mtable_create(tort_v delegate)
{
  tort_mtable *obj_mt = tort_allocate(tort__mt(mtable), sizeof(tort_mtable));
  _tort_m_map__initialize(tort_thread_arg (tort_v) obj_mt);

  tort_mtable *cls_mt = tort_allocate(tort__mt(mtable), sizeof(tort_mtable));
  _tort_m_map__initialize(tort_thread_arg (tort_v) cls_mt);

  tort_h_ref(obj_mt)->mtable = cls_mt;

  tort_mtable_set_delegate(obj_mt, delegate);
 
  return obj_mt;
}


tort_mtable* tort_mtable_get(const char *name)
{
  tort_v sym = tort_symbol_make(name);
  tort_v mt = tort_send(tort__s(get), tort_(m_mtable), sym);
  return mt;
}


tort_mtable* tort_mtable_make(const char *name, tort_v parent)
{
  tort_v sym = tort_symbol_make(name);
  tort_v mt = tort_send(tort__s(get), tort_(m_mtable), sym);
  if ( mt == tort_nil ) {
    mt = tort_mtable_create(parent ? parent : tort__mt(object));
    tort_send(tort__s(set), tort_(m_mtable), sym, mt);
  }
  return mt;
}


tort_v tort_runtime_initialize_mtable()
{
  /* Create mtable method table. */
  tort__mt(mtable)      = tort_mtable_create(0);

  /* Create core method tables. */
  tort__mt(object)      = tort_mtable_create(0);

  /* Create vector base. */
  tort__mt(vector_base) = tort_mtable_create(tort__mt(object));

  /* Create map mtable. */
  tort__mt(map)         = tort_mtable_create(tort__mt(vector_base));

  /* Back patch mtable -> map. */
  tort_mtable_set_delegate(tort__mt(mtable), tort__mt(map));

  /* Initialize nil object header. */
  tort__mt(nil)         = tort_mtable_create(tort__mt(object));
  tort_(nil_header).alloc_size = 0;
  tort_(nil_header).lookupf = _tort_object_lookupf;
  tort_(nil_header).applyf  = _tort_object_applyf;
  tort_(nil_header).mtable  = tort__mt(nil);

  /* Initialize tagged object header. */
  tort__mt(tagged)      = tort_mtable_create(tort__mt(object));
  tort_(tagged_header).alloc_size = 0;
  tort_(tagged_header).lookupf = _tort_object_lookupf;
  tort_(tagged_header).applyf  = _tort_object_applyf;
  tort_(tagged_header).mtable  = tort__mt(tagged);

  /* Other core. */
  tort__mt(string)      = tort_mtable_create(tort__mt(vector_base));
  tort__mt(vector)      = tort_mtable_create(tort__mt(vector_base));
  tort__mt(symbol)      = tort_mtable_create(tort__mt(object));
  tort__mt(method)      = tort_mtable_create(tort__mt(object));
  tort__mt(message)     = tort_mtable_create(tort__mt(object));
  tort__mt(boolean)     = tort_mtable_create(tort__mt(object));

  /* io */
  tort__mt(io)     = tort_mtable_create(tort__mt(object));
  tort__mt(eos)    = tort_mtable_create(tort__mt(object));

  return tort__mt(mtable);
}

