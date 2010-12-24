#include "tort/core.h"


/********************************************************************/

tort_v tort_class_get(const char *name)
{
  tort_v sym = tort_symbol_make(name);
  tort_v mt = tort_send(tort__s(get), _tort->_m_class, sym);
  return mt;
}


tort_v tort_class_make(const char *name, tort_v parent)
{
  tort_v sym = tort_symbol_make(name);
  tort_v mt = tort_send(tort__s(get), _tort->_m_class, sym);
  if ( mt == tort_nil ) {
    mt = tort_mtable_create(parent ? parent : _tort->_mt_object);
    tort_send(tort__s(set), _tort->_m_class, sym, mt);
  }
  return mt;
}


tort_v tort_runtime_initialize_mtable()
{
  /* Create mtable method table. */
  _tort->_mt_mtable      = tort_mtable_create(0);

  /* Create core method tables. */
  _tort->_mt_object      = tort_mtable_create(0);

  /* Backpatch mtable method table as object. */
  tort_h_ref(_tort->_mt_mtable)->mtable = _tort->_mt_mtable;

  _tort->_mt_map         = tort_mtable_create(_tort->_mt_object);
  /* Backpatch mtable to map delegation. */
  tort_ref(tort_mtable, _tort->_mt_mtable)->delegate = _tort->_mt_map;

  _tort->_mt_string      = tort_mtable_create(_tort->_mt_object);
  _tort->_mt_vector      = tort_mtable_create(_tort->_mt_object);
  _tort->_mt_symbol      = tort_mtable_create(_tort->_mt_object);
  _tort->_mt_method      = tort_mtable_create(_tort->_mt_object);
  _tort->_mt_message     = tort_mtable_create(_tort->_mt_object);
  _tort->_mt_nil         = tort_mtable_create(_tort->_mt_object);
  _tort->_mt_boolean     = tort_mtable_create(_tort->_mt_object);

  /* Initialize tagged object header. */
  _tort->_mt_tagged      = tort_mtable_create(_tort->_mt_object);
  _tort->_tagged_header.alloc_size = 0;
  _tort->_tagged_header.lookupf = _tort_object_lookupf;
  _tort->_tagged_header.applyf  = _tort_object_applyf;
  _tort->_tagged_header.mtable  = _tort->_mt_tagged;

  _tort->_mt_io     = tort_mtable_create(_tort->_mt_object);
  _tort->_mt_eos    = tort_mtable_create(_tort->_mt_object);

  return _tort->_mt_mtable;
}

tort_v tort_runtime_initialize_mtable_class()
{
#define D(X) \
  tort_send(tort__s(set), _tort->_m_class, tort_symbol_make(#X), _tort->_mt_##X);

  D(mtable);
  D(object);
  D(map);
  D(string);
  D(vector);
  D(symbol);
  D(method);
  D(message);
  D(nil);
  D(boolean);
  D(tagged);
  D(io);
  D(eos);

  return _tort->_m_class;
}

