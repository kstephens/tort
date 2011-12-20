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

tort_v _tort_M_fiber__new(tort_thread_param tort_v rcvr, tort_v block)
{
  struct tort_fiber_data d = {
    _tort_message,
    block,
  };
  tort_v _tort_fiber = _tort_message->fiber;
  assert(_tort_message);
  assert(block);
  return __tort_fiber_new((tort_fiber_t*) _tort_fiber, fiber_func, &d, 0);  
}

tort_v _tort_m_fiber__yield (tort_thread_param tort_v rcvr)
{
  tort_v _tort_fiber = _tort_message->fiber;
  assert(_tort_fiber);
  return (tort_v) __tort_fiber_yield(_tort_fiber, tort_ref(tort_fiber_t, rcvr));
}

static void *allocate(size_t size)
{
  return tort_send(tort__s(_allocate), tort_mt(fiber), tort_i(size));
}

tort_v tort_runtime_initialize_fiber()
{
  tort_v _mt_fiber = tort_mtable_create_class("fiber", tort_mt(object));
  __tort_fiber_allocate = allocate;
  return _mt_fiber;
}

