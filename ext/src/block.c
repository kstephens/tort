#include "tort/core.h"
#include "tort/block.h"

tort_apply_decl(_tort_block_lookupf)
{
  if ( _tort_message->selector == tort__s(value) ) {
    return _tort_message->method = _tort_message->receiver;
  } else {
    return _tort_object_lookupf(tort_thread_arg rcvr);
  }
}


tort_v tort_runtime_initialize_block()
{
  tort__mt(block) = tort_mtable_make("block", 0);
  // tort_add_method(tort_mt(block), "value", );
  return tort__mt(block);
}

