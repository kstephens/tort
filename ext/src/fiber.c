#include "tort/core.h"
#include "tort/fiber.h"
#include <assert.h>

tort_SLOT(message,tort_v,fiber);

struct tort_fiber_data
{
  tort_message *message;
  tort_v block;
};

static
tort_fiber_func_DECL(fiber_func)
{
  tort_v _tort_fiber = (tort_v) _tort_fiber_ptr;
  struct tort_fiber_data *d = data;
  tort_message *_tort_message = d->message;
  assert(d);
  assert(_tort_message);
  _tort_message->fiber = _tort_fiber;
  return (tort_v) tort_send(tort__s(value), d->block);
}

tort_v _tort_M_fiber__start(tort_tp tort_v fiber, tort_v block)
{
  struct tort_fiber_data d = {
    _tort_message,
    block,
  };
  assert(_tort_message);
  assert(block);
  __tort_fiber_start(fiber, fiber_func, &d);
  return fiber;
}

tort_v _tort_M_fiber__new(tort_tp tort_v rcvr)
{
  tort_fiber_t *fiber;
  fiber = tort_send(tort__s(_allocate), rcvr, tort_i(sizeof(*fiber)));
  __tort_fiber_init(fiber, 0);
  return fiber;
}

tort_v _tort_m_fiber__yield (tort_tp tort_v rcvr)
{
  __tort_fiber_yield(tort_ref(tort_fiber_t, rcvr));
  return rcvr;
}

tort_v _tort_m_initializer__fiber(tort_tp tort_v init)
{
  tort_mtable_create_class("fiber", tort_mt(object));
  return init;
}

