#include "tort/core.h"
#include "tort/block.h"

tort_apply_decl(_tort_block_lookupf)
{
  if ( tort_ref(tort_message, _tort_message)->selector == tort__s(value) ) {
    
    return tort_ref(tort_message, _tort_message)->method = 
      tort_ref(tort_message, _tort_message)->receiver;
  } else {
    return _tort_object_lookupf(tort_thread_arg rcvr);
  }
}


tort_v tort_runtime_initialize_block()
{
  _tort->_mt_block = tort_class_make("block", 0);
  // tort_add_method(_tort->_mt_block, "value", );
  return _tort->_mt_block;
}

