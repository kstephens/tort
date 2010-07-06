#include "tort/core.h"
#include "tort/block.h"

tort_apply_decl(_tort_block_lookupf)
{
  if ( tort_ref(tort_message, _tort_message)->selector == tort__s(value) ) {
    
    return tort_ref(tort_message, _tort_message)->method = 
      tort_ref(tort_message, _tort_message)->receiver;
  } else {
    return _tort_object_lookupf(_tort_message, rcvr);
  }
}


void tort_runtime_initialize_block()
{
  _tort->_mt_block = tort_mtable_create(_tort->_mt_object);
}

