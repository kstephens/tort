#include "tort/core.h"

typedef struct tort_send_block {
  tort_v selector;
} tort_send_block;

tort_ACCESSOR(send_block,tort_v,selector);

tort_v _tort_m_send_block__value(tort_tp tort_send_block *o, tort_v rcvr) /* TORT_NO_INTERNAL */
{
  return_tort_send(o->selector, rcvr);
}

tort_v _tort_M_send_block__new(tort_tp tort_mtable* mtable, tort_v selector) /* TORT_NO_INTERNAL */
{
  tort_send_block *o = tort_send(tort__s(_allocate), mtable, tort_i(sizeof(*o)));
  o->selector = selector;
  return o;
}

tort_v tort_runtime_initialize_send_block()
{
  tort_mtable_create_class("send_block", 0);
  return tort_true;
}

tort_v _tort_m_map__keys(tort_tp tort_map *map)
{
  tort_v block = tort_send(tort_s(new), tort_mt(send_block), tort__s(first));
  return_tort_send(tort_s(map), map, block);
}

tort_v _tort_m_map__values(tort_tp tort_map *map)
{
  tort_v block = tort_send(tort_s(new), tort_mt(send_block), tort__s(second));
  return_tort_send(tort_s(map), map, block);
}
