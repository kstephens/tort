#include "tort/fiber.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <strings.h> /* bzero() */
#include <errno.h>
#include <sys/mman.h>
#include <assert.h>

size_t _tort_fiber_default_stack_size = 1 * 1024 * 1024;

void  (*__tort_fiber_mmap_region)(void *ptr, size_t size) = 0;
void  (*__tort_fiber_munmap_region)(void *ptr, size_t size) = 0;

static
void __tort_fiber_error(tort_fiber_t *fiber, const char *msg)
{
  fiber->error_str = msg;
  fiber->error_code = errno;
  perror(msg);
}

TORT_FIBER_VOLATILE tort_fiber_t *__tort_fiber_current_ = 0; // NOT THREAD SAFE
tort_fiber_t *__tort_fiber_current()
{
  // NOT THREAD SAFE
  return __tort_fiber_current_;
}

static void __tort_fiber_set_current(tort_fiber_t *fiber)
{
  // NOT THREAD SAFE
  if ( fiber == __tort_fiber_current() ) return;
  fiber->status = RUNNING;
  __tort_fiber_current_ = fiber;
}

static tort_fiber_t *__tort_fiber_main_, __tort_fiber_main__; // NOT THREAD SAFE
tort_fiber_t *__tort_fiber_main()
{
  tort_fiber_t *fiber;
  if ( ! (fiber = __tort_fiber_main_) ) { // NOT THREAD SAFE
    fiber = &__tort_fiber_main__;
    fiber->status = CREATED;
    if ( getcontext(&fiber->_ucontext) ) {
      __tort_fiber_error(fiber, "__tort_fiber_main: fiber main: getcontext");
      abort(); return 0;
    }
    if ( fiber->status >= EXITED ) return fiber;
    __tort_fiber_set_current(fiber);
    __tort_fiber_main_ = fiber;
  }
  return fiber;
}

static
void __tort_fiber_func(tort_fiber_t *fiber)
{
  fprintf(stderr, "  fiber @%p STARTING\n", fiber);
  if ( ! setjmp(fiber->_func_exit) ) {
    fiber->_func_exit_valid = 1;
    fiber->func_result = 0;
    __tort_fiber_set_current(fiber);
    fiber->func_result = fiber->func(fiber, fiber->func_data);
    fiber->status = EXITED;
    fiber->_func_exit_valid = 0;
  }
  assert(fiber->_func_exit_valid == 0);
  fprintf(stderr, "  fiber @%p EXITED: switch to fiber @%p\n", fiber, fiber->prev);
  if ( setcontext(&fiber->prev->_ucontext) ) {
    __tort_fiber_error(fiber, "__tort_fiber_func: EXIT: setcontext");
    abort();
  }
}

void __tort_fiber_terminate(tort_fiber_t *fiber, void *func_result)
{
  if ( fiber->status >= EXITED ) {
    __tort_fiber_error(fiber, "__tort_fiber_exit: already exited");
    abort(); return;
  }
  fiber->func_result = func_result;
  fiber->status = TERMINATED;
  if ( __tort_fiber_current() == fiber ) {
    if ( ! fiber->_func_exit_valid ) {
      __tort_fiber_error(fiber, "__tort_fiber_exit: already longjmped");
      abort(); return;
    }
    fiber->_func_exit_valid = 0;
    longjmp(fiber->_func_exit, 1);
  }
}

void __tort_fiber_yield(tort_fiber_t *fiber, tort_fiber_t *other_fiber)
{
  if ( other_fiber == fiber ) return;
  fiber->stk_sp = &fiber; // approx.
  fiber->status = PAUSED;
  other_fiber->prev = fiber;
  __tort_fiber_set_current(other_fiber);
  if ( swapcontext(&fiber->_ucontext, &other_fiber->_ucontext) ) {
    __tort_fiber_error(fiber, "__tort_fiber_yield: swapcontext");
    abort(); return;
  }
}

void __tort_fiber_begin(tort_fiber_t *fiber, tort_fiber_t *fiber_parent, tort_fiber_func func, void *func_data)
{
  fiber->func = func;
  fiber->func_data = func_data;
  fiber->parent = fiber_parent ? fiber_parent : __tort_fiber_current();
  fiber->status = STARTING;
  if ( getcontext(&fiber->_ucontext) ) {
    __tort_fiber_error(fiber, "__tort_fiber_begin: getcontext");
    abort(); return;
  }
  fiber->_ucontext.uc_stack.ss_sp   = fiber->stk_base;
  fiber->_ucontext.uc_stack.ss_size = fiber->stk_size;
  fiber->_ucontext.uc_link          = &__tort_fiber_main()->_ucontext;
  makecontext(&fiber->_ucontext, __tort_fiber_func, 1, fiber);
  if ( 0 ) {
    __tort_fiber_error(fiber, "__tort_fiber_begin: makecontext");
    abort(); return;
  }
  __tort_fiber_yield(fiber->parent, fiber);
}

void __tort_fiber_init(tort_fiber_t *fiber, size_t size)
{
  __tort_fiber_main();
  fiber->status = CREATED;
  if ( ! size ) size = _tort_fiber_default_stack_size;
  if ( ! fiber->stk_size ) fiber->stk_size = size;

  if ( ! fiber->stk_base ) {
    fiber->stk_base = mmap(NULL, fiber->stk_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if ( fiber->stk_base == (void*) -1LL ) {
      __tort_fiber_error(fiber, "__tort_fiber_new: alloc stack: mmap");
      return;
    }
  }
#if 0
  if (mprotect(fiber->stk_base, fiber->stk_size, PROT_READ | PROT_WRITE) < 0) {
    __tort_fiber_error(fiber, "__tort_fiber_new: mprotect: failed");
    return;
  }
#endif
  if ( __tort_fiber_mmap_region )
    __tort_fiber_mmap_region(fiber->stk_base, fiber->stk_size);
  
  /*
    +----------------+                    -------------------+
    |                |                                       |
    +----------------+                    -------------------+
    |                 |                    |    |
    base              base + size          sp   fp
  */
  fiber->status = INITIALIZED;
}

void __tort_fiber_destroy(tort_fiber_t *fiber)
{
  int result = 0;
  if ( fiber->stk_base ) {
    if ( __tort_fiber_munmap_region )
      __tort_fiber_munmap_region(fiber->stk_base, fiber->stk_size);
    if ( (result = munmap(fiber->stk_base, fiber->stk_size)) ) {
      __tort_fiber_error(fiber, "__tort_fiber_destroy: free stack: munmap");
    }
  }
}
