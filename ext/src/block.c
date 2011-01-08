#include "tort/core.h"
#include "tort/block.h"

tort_lookup_decl(_tort_M_block__lookup)
{
  if ( message->selector == tort__s(value) ) {
    message->mtable = mtable;
    message->method = (tort_v) message->receiver;
    return message;
  } else {
    return tort_send(tort__s(lookup), message);
  }
}


tort_v tort_runtime_initialize_block()
{
  tort__mt(block) = tort_mtable_make("block", 0);
  return tort__mt(block);
}

