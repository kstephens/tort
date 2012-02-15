#include "tort/core.h"
#include "tort/block.h"

tort_lookup_decl(_tort_M_block__lookup)
{
  if ( message->selector == tort__s(value) ) {
    message->mtable = mtable;
    message->method = (tort_v) message->receiver;
    return message;
  } else {
    return _tort_m_mtable__lookup(tort_ta mtable, message);
  }
}

tort_v _tort_m_initializer__block(tort_tp tort_v init)
{
  tort__mt(block) = tort_mtable_create_class("block", tort_mt(method));
  return init;
}

