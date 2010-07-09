#include "tort/core.h"


/********************************************************************/


void tort_runtime_initialize_mtable()
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
  _tort->_tagged_header.mtable = _tort->_mt_tagged;

  _tort->_mt_block = tort_mtable_create(_tort->_mt_object);

  _tort->_mt_io     = tort_mtable_create(_tort->_mt_object);
  _tort->_mt_eos    = tort_mtable_create(_tort->_mt_object);

  _tort->_mt_pair = tort_mtable_create(_tort->_mt_object);

}

