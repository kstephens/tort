#include "tort/core.h"
#include "tort/fiber.h"

tort_v _tort_m_message__fiber(tort_thread_param tort_message *msg)
{
  return msg->fiber;
}

tort_v _tort_m_message__set_fiber(tort_thread_param tort_message *msg, tort_v fiber)
{
  return msg->fiber = fiber;
}

static
tort_fiber_func_DECL(fiber_func)
{
  tort_v _tort_fiber = (tort_v) _tort_fiber_ptr;
  return (tort_v) tort_send(tort__s(value), (tort_v) data);
}

tort_v _tort_m_object___fiber_new(tort_thread_param tort_v rcvr, tort_v proc)
{
  _tort_fiber = _tort_message->fiber;
  return __tort_fiber_new((tort_fiber_t*) _tort_fiber, fiber_func, (void*) proc, 0);  
}

tort_v _tort_m_fiber__yield (tort_thread_param tort_v rcvr)
{
  _tort_fiber = _tort_message->fiber;
  return (tort_v) __tort_fiber_yield(_tort_fiber, tort_ref(tort_fiber_t, rcvr));
}

static void *allocate(size_t size)
{
  return tort_allocate(tort_mt(fiber), size);
}

tort_v tort_runtime_initialize_fiber()
{
  tort_v _mt_fiber = tort_mtable_make("fiber", 0);

  __tort_fiber_allocate = allocate;

  tort_add_method(tort_mt(message), "fiber", _tort_m_message__fiber);
  tort_add_method(tort_mt(object), "_fiber_new", _tort_m_object___fiber_new);
  tort_add_method(_mt_fiber, "yield", _tort_m_fiber__yield);

  return _mt_fiber;
}

