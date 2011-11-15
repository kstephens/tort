#include "tort/core.h"

// tort_GETTER(mtable,tort_v,delegate)
tort_ACCESSOR(mtable,size_t,instance_size);
tort_ACCESSOR(mtable,voidP,gc_data);
tort_ACCESSOR(mtable,tort_v,gc_mark_method);
tort_ACCESSOR(mtable,tort_v,gc_free_method);
tort_ACCESSOR(mtable,tort_v,data);

unsigned long _tort_alloc_id = 0;

tort_v _tort_allocate(tort_tp tort_v mtable, size_t size
#if TORT_ALLOC_DEBUG
		      ,const char *alloc_file, int alloc_line
#endif
)
{
  tort_v val = _tort_m_mtable___allocate(tort_ta mtable, size);
#if TORT_ALLOC_DEBUG
  tort_h_ref(val)->alloc_file = alloc_file;
  tort_h_ref(val)->alloc_line = alloc_line;
  tort_h_ref(val)->alloc_id   = _tort_alloc_id;
#endif
  return val;
}

tort_v _tort_m_mtable__allocate (tort_tp tort_mtable *mtable)
{
  return _tort_m_mtable___allocate(tort_ta mtable, mtable->instance_size);
}

tort_v _tort_m_mtable___allocate (tort_tp tort_mtable *mtable, size_t size)
{
  void *ptr;
  ptr = tort_object_alloc(mtable, size);
  return ptr;
}

tort_v _tort_m_mtable__add_method (tort_tp tort_mtable *mtable, tort_symbol *symbol, tort_method *method)
{
  // fprintf(stderr, "  add_method %s %s %p\n", tort_object_name(mtable), tort_object_name(symbol), method->applyf);
  if ( method->name == tort_nil || method->name == 0 ) {
    char *buf = tort_malloc(strlen(tort_mtable_name_(mtable)) + 2 + strlen(tort_symbol_charP(symbol)) + 1);
    sprintf(buf, "%s::%s", tort_mtable_name_(mtable), tort_symbol_charP(symbol));
    method->name = tort_symbol_new(buf);
  }
  _tort_m_symbol___version_change(tort_ta symbol);
#if TORT_ANON_SYMBOL_MTABLE
  if ( symbol->name == tort_nil || symbol->name == 0 ) {
    _tort_m_map__set(tort_ta (tort_v) symbol->mtable_method_map, mtable, method);
  } else
#endif
  _tort_m_map__set(tort_ta (tort_v) mtable, symbol, method);
  _tort_m_mtable___method_changed(tort_ta mtable, symbol, method);
  return method;
}

tort_v _tort_m_mtable__remove_method (tort_tp tort_mtable *mtable, tort_symbol *symbol)
{
  tort_v method;
  _tort_m_symbol___version_change(tort_ta symbol);
#if TORT_ANON_SYMBOL_MTABLE
  if ( symbol->name == tort_nil ) {
    method = _tort_m_map__delete(tort_ta (tort_v) symbol->mtable_method_map, mtable);
  } else
#endif
  method = _tort_m_map__delete(tort_ta (tort_v) mtable, symbol);
  _tort_m_mtable___method_changed(tort_ta mtable, symbol, method);
  return mtable;
}

tort_v _tort_m_mtable__alias_method (tort_tp tort_mtable *mtable, tort_symbol *symbol, tort_symbol *other_symbol)
{
  tort_message_ msg_;
  tort_message *msg = &msg_._;
  msg->_h[-1].mtable = tort__mt(message);
  msg->selector = other_symbol;
  msg->method = tort_nil;
  msg = tort_send(tort_s(lookup), mtable, msg);
  return_tort_send(tort_s(add_method), mtable, symbol, msg->method);
}

/********************************************************************/

tort_v tort_add_method(tort_v mtable, const char *name, void *applyf)
{
  tort_v sym = tort_symbol_new(name);
  tort_v meth = tort_method_new(applyf, 0);
  assert(mtable); assert(applyf);
  return _tort_m_mtable__add_method(tort_ta mtable, sym, meth);
}

tort_v tort_add_class_method(tort_v mtable, const char *name, void *applyf)
{
  return tort_add_method(tort_h_ref(mtable)->mtable, name, applyf);
}

tort_mtable* tort_mtable_set_delegate(tort_mtable *obj_mt, tort_v delegate)
{
  tort_mtable *cls_mt;
  tort_v cls_delegate;
  if ( delegate == 0 && tort_nil != 0 )
    delegate = tort_nil;
  cls_mt = tort_h_ref(obj_mt)->mtable;
  cls_delegate = delegate != tort_nil ? tort_h_ref(delegate)->mtable : tort_nil;
  _tort_m_mtable__set_delegate(tort_ta obj_mt, delegate);
  _tort_m_mtable__set_delegate(tort_ta cls_mt, cls_delegate);
  return obj_mt;
}

static tort_mtable * tort_mtable_create_0(tort_v delegate)
{
  tort_mtable *mt = tort_allocate(tort__mt(mtable), sizeof(tort_mtable));
  _tort_m_map__initialize(tort_ta (tort_v) mt);
  mt->delegate = delegate;
  mt->instance_size = 0;
  mt->gc_data = 0;
  mt->gc_mark_method = 0;
  mt->gc_free_method = 0;
  mt->data = 0;
  return mt;
}

tort_mtable* tort_mtable_create(tort_v delegate)
{
  tort_mtable *obj_mt = tort_mtable_create_0(delegate);
  tort_mtable *cls_mt = tort_mtable_create_0(0);
  tort_h_ref(obj_mt)->mtable = cls_mt;
  tort_mtable_set_delegate(obj_mt, delegate);
  return obj_mt;
}

tort_v _tort_M_mtable__new(tort_tp tort_mtable *mtable, tort_v delegate)
{
  return tort_mtable_create(delegate);
}

tort_mtable* tort_mtable_get(const char *name)
{
  tort_v sym = tort_symbol_new(name);
  tort_v mt = tort_send(tort__s(get), tort_(m_mtable), sym);
  return mt;
}

tort_mtable* tort_mtable_new(const char *name, tort_v parent)
{
  tort_v sym = tort_symbol_new(name);
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
  tort__mt(mtable)      = tort_mtable_create_0(0);
  tort_h(tort__mt(mtable))->mtable = tort__mt(mtable);

  /* Create object method table. */
  tort__mt(object)      = tort_mtable_create_0(0);
  tort_h(tort__mt(object))->mtable = tort__mt(mtable);
  
  /*************************************************/
  /* Create core method tables. */

  tort__mt(vector_base) = tort_mtable_create(tort__mt(object));
  tort__mt(pair)        = tort_mtable_create(tort__mt(object));
  tort__mt(vector)      = tort_mtable_create(tort__mt(vector_base));
  tort__mt(map)         = tort_mtable_create(tort__mt(vector));
  _tort_m_mtable__set_delegate(tort_ta tort__mt(mtable), tort__mt(map));

  /* Initialize nil object header. */
  tort__mt(nil)         = tort_mtable_create(tort__mt(object));
  tort_(nil_header).mtable  = tort__mt(nil);
  tort_(nil_header).applyf = _tort_m_object___cannot_apply;

  /* Initialize tagged object headers. */
  tort__mt(tagged)      = tort_mtable_create(tort__mt(object));
  {
    int i;
    for ( i = 0; i < 1 << TORT_TAG_BITS; ++ i ) {
      tort_(tagged_header[i]).mtable = tort_mtable_create(tort__mt(tagged));
      tort_(tagged_header[i]).applyf = _tort_m_object___cannot_apply;
    }
  }
#ifdef tort_tag_fixnum 
  tort__mt(fixnum) = tort_(tagged_header[tort_tag_fixnum]).mtable;
#endif
#ifdef tort_tag_locative
  tort__mt(locative) = tort_(tagged_header[tort_tag_locative]).mtable;
  tort_(tagged_header[tort_tag_locative]).applyf = (void*) _tort_m_locative___applyf;
#endif

  /* Other core. */
  tort__mt(ptr)         = tort_mtable_create(tort__mt(object));
  tort__mt(string)      = tort_mtable_create(tort__mt(vector_base));
  tort__mt(symbol)      = tort_mtable_create(tort__mt(object));
  tort__mt(method)      = tort_mtable_create(tort__mt(object));
  tort__mt(message)     = tort_mtable_create(tort__mt(object));
  tort__mt(caller_info) = tort_mtable_create(tort__mt(object));
  tort__mt(boolean)     = tort_mtable_create(tort__mt(object));

  /* io */
  tort__mt(io)     = tort_mtable_create(tort__mt(object));
  tort__mt(eos)    = tort_mtable_create(tort__mt(object));

  /* dynlib */
  tort__mt(dynlib) = tort_mtable_create(tort__mt(map));

  /* gc */
  tort__mt(gc)     = tort_mtable_create(tort__mt(object));

  /* force references for extensions. */
  (void) tort__mt(block);

  return tort__mt(mtable);
}

