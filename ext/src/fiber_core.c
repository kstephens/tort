#include "tort/fiber.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <strings.h> /* bzero() */
#include <sys/mman.h>
#include <assert.h>


#define C(X) _rse_##X##0, _rse_##X##1, _rse_##X##2, _rse_##X##3, _rse_##X##4 
#define S(X) _rse_##X##0= _rse_##X##1= _rse_##X##2= _rse_##X##3= _rse_##X##4 
static volatile int 
  C(00), C(01), C(02), C(03), C(04),
  C(10), C(11), C(12), C(13), C(14),
  C(20), C(21), C(22), C(23), C(24),
  C(30), C(31), C(32), C(33), C(34);
int _tort_dummy_false = 0;
#define _FLUSH_REGISTERS			\
  if ( _tort_dummy_false ) {			\
  S(00)= S(01)= S(02)= S(03)= S(04)=		\
    S(10)= S(11)= S(12)= S(13)= S(14)=		\
    S(20)= S(21)= S(22)= S(23)= S(24)=		\
    S(30)= S(31)= S(32)= S(33)= S(34)= 0;	\
  S(00)= S(01)= S(02)= S(03)= S(04)=		\
    S(10)= S(11)= S(12)= S(13)= S(14)=		\
    S(20)= S(21)= S(22)= S(23)= S(24)=		\
    S(30)= S(31)= S(32)= S(33)= S(34)= 0;	\
  }

void *__tort_fiber_yield(TORT_FIBER_VOLATILE tort_fiber_t *fiber, TORT_FIBER_VOLATILE tort_fiber_t *other_fiber)
{
  _FLUSH_REGISTERS;
  if ( other_fiber == fiber ) {
    return fiber;
  }
  /* Pause current fiber. */
  switch ( fiber ? _tort_setjmp(fiber) : 0 ) {
  case 0:
    if ( fiber ) fiber->status = PAUSED;
    /* Resume other fiber. */
    _tort_longjmp(other_fiber, 1);
  case 1:
    if ( fiber ) fiber->status = RUNNING;
    return fiber;
  default:
    abort();
  }
  return 0;
}


static
void *__tort_fiber_begin(TORT_FIBER_VOLATILE tort_fiber_t *fiber)
{
  _FLUSH_REGISTERS;
  /* Pause parent fiber. */
  switch ( fiber->parent ? _tort_setjmp(fiber->parent) : 0 ) {
  case 0:
    /* Invoke new fiber func. */
    if ( fiber->parent ) fiber->parent->status = PAUSED;
    fiber->status = RUNNING;
    return fiber->func_result = fiber->func(fiber, fiber->func_data);
  case 1:
    if ( fiber->parent ) fiber->parent->status = RUNNING;
    fiber->status = PAUSED;
    /* Other fiber resumed parent fiber. */
    return fiber->func_result;
  default:
    abort();
  }
  return 0;
}


size_t _tort_fiber_default_stack_size = 1 * 1024 * 1024;

void *(*__tort_fiber_allocate)(size_t size) = malloc;

void *__tort_fiber_new(tort_fiber_t *fiber_parent, tort_fiber_func func, void *func_data, size_t size)
{
  tort_fiber_t *fiber = __tort_fiber_allocate(sizeof(*fiber));
  bzero(fiber, sizeof(*fiber));
  fiber->status = CREATED;
  fiber->parent = fiber_parent;
  fiber->func = func;
  fiber->func_data = func_data;
  if ( size == 0 ) size = _tort_fiber_default_stack_size;
  fiber->stk_size = size;

  fiber->stk_ptr = mmap(NULL, fiber->stk_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
  if (fiber->stk_ptr == (void*) -1LL) {
    perror("can't alloc machine stacks");
  }
  if (mprotect(fiber->stk_ptr,
	       1024, PROT_READ | PROT_WRITE) < 0) {
    perror("mprotect failed");
  }
  
  /*
    +----------------+                    -------------------+
    |                |                                       |
    +----------------+                    -------------------+
    |                 |                    |    |
    ptr               ptr + size           sp   fp
  */
  fiber->parent_sp = __builtin_alloca((size_t) 0); /* (void*) &ptr; */
  long long dist = fiber->parent_sp - (fiber->stk_ptr + fiber->stk_size);
  // fprintf(stderr, "  fiber(@%18p)\n        sp = @%18p\n       ptr = @%18p\n      dist = @0x%16llx\n", fiber, fiber->parent_sp, fiber->stk_ptr, (long long) dist);
  size_t pad = sizeof(void*);
  fiber->start_sp = __builtin_alloca((dist + pad) & ~pad);
  // fprintf(stderr, "    start_sp = @%18p\n", fiber->start_sp);
  __tort_fiber_begin(fiber);
  fiber->status = PAUSED;
  return fiber->func_result;
}

