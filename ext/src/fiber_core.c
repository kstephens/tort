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

static
void __tort_fiber_set_current(tort_fiber_t *fiber)
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
    fiber->name = "*MAIN*";
    // fprintf(stderr, "  fiber @%p(\"%s\")\n", fiber, fiber->name);
    fiber->_next = fiber->_prev = fiber;
    __tort_fiber_set_current(fiber);
    __tort_fiber_main_ = fiber;
  }
  return fiber;
}

static
void __tort_fiber_add(tort_fiber_t *fiber)
{
  tort_fiber_t *head = __tort_fiber_main();
  fiber->_prev = head->_prev;
  fiber->_next = head;
  head->_prev->_next = fiber;
  head->_prev = fiber;
}

static
void __tort_fiber_remove(tort_fiber_t *fiber)
{
  if ( ! fiber->_next ) return;
  fiber->_prev->_next = fiber->_next;
  fiber->_next->_prev = fiber->_prev;
  fiber->_next = fiber->_prev = 0;
}

static
void __tort_fiber_print_fibers(void *highlight)
{
  tort_fiber_t *head = __tort_fiber_main();
  tort_fiber_t *f = head;
  do { 
    fprintf(stderr, "  %s@%p(\"%s\", %d) -->", f == highlight ? "^" : " ", f, f->name, f->status);
    f = f->_next;
  } while ( f != head );
  fprintf(stderr, "  END\n");
}

static
tort_fiber_t *__tort_fiber_find_paused(tort_fiber_t *fiber)
{
  tort_fiber_t *head = __tort_fiber_main();
  tort_fiber_t *f = fiber->_next;
  __tort_fiber_print_fibers(fiber);
  do {
    if ( f != head && f->status == PAUSED ) return f;
    f = f->_next;
  } while ( f != fiber );
  return 0;
}

static
void __tort_fiber_exited(tort_fiber_t *fiber)
{
  tort_fiber_t *next_fiber;

  next_fiber = __tort_fiber_find_paused(fiber);
  __tort_fiber_remove(fiber);
  if ( ! next_fiber ) {
    fprintf(stderr, "  fiber @%p(\"%s\") EXITED: no more fibers\n", fiber, fiber->name);
    __tort_fiber_print_fibers(fiber);
    next_fiber = __tort_fiber_main();
    __tort_fiber_set_current(next_fiber);
    if ( setcontext(&next_fiber->_ucontext_start) ) {
      __tort_fiber_error(fiber, "__tort_fiber_func: NO FIBERS: setcontext");
      abort();
    }
  }

  fprintf(stderr, "  fiber @%p(\"%s\") EXITED: switch to fiber @%p(\"%s\")\n", fiber, fiber->name, next_fiber, next_fiber->name);
  __tort_fiber_set_current(next_fiber);
  if ( setcontext(&next_fiber->_ucontext) ) {
    __tort_fiber_error(fiber, "__tort_fiber_func: EXIT: setcontext");
    abort();
  }
}

static
void __tort_fiber_func(tort_fiber_t *fiber)
{
  fprintf(stderr, "  fiber @%p(\"%s\") STARTING\n", fiber, fiber->name);
  if ( ! setjmp(fiber->_func_exit) ) {
    fiber->_func_exit_valid = 1;
    fiber->func_result = 0;
    __tort_fiber_set_current(fiber);
    fiber->func_result = fiber->func(fiber, fiber->func_data);
    fiber->status = EXITED;
    fiber->_func_exit_valid = 0;
  }
  assert(fiber->_func_exit_valid == 0);
  __tort_fiber_exited(fiber);
}

void __tort_fiber_terminate(tort_fiber_t *fiber)
{
  if ( fiber->status >= EXITED ) {
    __tort_fiber_error(fiber, "__tort_fiber_exit: already exited");
    abort(); return;
  }
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

void __tort_fiber_yield(tort_fiber_t *fiber)
{
  tort_fiber_t *curr = __tort_fiber_current();
  if ( ! fiber && ! (fiber = __tort_fiber_find_paused(curr)) ) return;
  if ( fiber == curr ) return;
  if ( fiber->status >= EXITED ) {
    __tort_fiber_error(curr, "__tort_fiber_yield: other fiber exited.");
    abort(); return;
  }

  curr->stk_sp = &fiber; // approx.
  curr->status = PAUSED;
  fiber->prev = curr;
  __tort_fiber_set_current(fiber);
  fprintf(stderr, "  fiber @%p(\"%s\") -> @%p(\"%s\")\n", curr, curr->name, fiber, fiber->name);
  if ( swapcontext(&curr->_ucontext, &fiber->_ucontext) ) {
    __tort_fiber_error(fiber, "__tort_fiber_yield: swapcontext");
    abort(); return;
  }
}

void __tort_fiber_start(tort_fiber_t *fiber, tort_fiber_func func, void *func_data)
{
  tort_fiber_t *head = __tort_fiber_main();
  tort_fiber_t *curr = __tort_fiber_current();

  __tort_fiber_remove(fiber);
  __tort_fiber_add(fiber);

  fprintf(stderr, "  __tort_fiber_start: @%p(\"%s\"): in @%p(\"%s\")\n", fiber, fiber->name, curr, curr->name);
  getcontext(&curr->_ucontext_start);
  fprintf(stderr, "  __tort_fiber_start: @%p(\"%s\"): in @%p(\"%s\") again\n", fiber, fiber->name, curr, curr->name);
  /* No more fibers? -- return to caller. */
  if ( head->_next == head ) {
    __tort_fiber_set_current(curr);
    fprintf(stderr, "  no more fibers: returning to caller\n");
    return;
  }

  fiber->func = func;
  fiber->func_data = func_data;
  fiber->parent = curr;
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
}

void __tort_fiber_init(tort_fiber_t *fiber, size_t size)
{
  fiber->status = CREATED;
  if ( ! size ) size = _tort_fiber_default_stack_size;
  if ( ! fiber->stk_size ) fiber->stk_size = size;

  if ( ! fiber->stk_base ) {
    fiber->stk_base = fiber->stk_mmap = mmap(NULL, fiber->stk_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if ( fiber->stk_base == (void*) -1LL ) {
      __tort_fiber_error(fiber, "__tort_fiber_new: alloc stack: mmap");
      return;
    }
    if ( __tort_fiber_mmap_region )
      __tort_fiber_mmap_region(fiber->stk_base, fiber->stk_size);
  }
#if 0
  if (mprotect(fiber->stk_base, fiber->stk_size, PROT_READ | PROT_WRITE) < 0) {
    __tort_fiber_error(fiber, "__tort_fiber_new: mprotect: failed");
    return;
  }
#endif
  fiber->status = INITIALIZED;
}

void __tort_fiber_destroy(tort_fiber_t *fiber)
{
  int result = 0;
  if ( fiber->stk_mmap ) {
    void *stk_base = fiber->stk_mmap;
    fiber->stk_mmap = 0;
    if ( __tort_fiber_munmap_region )
      __tort_fiber_munmap_region(stk_base, fiber->stk_size);
    if ( (result = munmap(stk_base, fiber->stk_size)) ) {
      __tort_fiber_error(fiber, "__tort_fiber_destroy: free stack: munmap");
    }
  }
}
