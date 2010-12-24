#ifndef USE_CONTEXT
#define USE_CONTEXT 0
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#if USE_CONTEXT
#include <ucontext.h>
#else
#include <setjmp.h>
#endif
#include <sys/mman.h>
#include <assert.h>

#define TORT_FIBER_VOLATILE
#ifndef TORT_FIBER_VOLATILE
#define TORT_FIBER_VOLATILE volatile
#endif

struct tort_fiber_t;
typedef struct tort_fiber_t tort_fiber_t;

#define tort_fiber_func_DECL(X) \
  void * X(TORT_FIBER_VOLATILE tort_fiber_t *fiber, TORT_FIBER_VOLATILE void *data)

typedef tort_fiber_func_DECL((*tort_fiber_func));

struct tort_fiber_t {
#if USE_CONTEXT
  ucontext_t _jb;
#else
  jmp_buf _jb;
#endif
  struct tort_fiber_t *parent;
  tort_fiber_func func;
  void *func_result;
  void *func_data;
  void *stk_ptr;
  size_t stk_size;
  void *parent_sp;
  void *start_sp;
};


#define _FORCE_REGISTER_SPILL \
  int \
    _r00, _r01, _r02, _r03, \
    _r10, _r11, _r12, _r13, \
    _r20, _r21, _r22, _r23, \
    _r30, _r31, _r32, _r33; \
  _r00 = _r01 = _r02 = _r03 = \
  _r10 = _r11 = _r12 = _r13 = \
  _r20 = _r21 = _r22 = _r23 = \
  _r30 = _r31 = _r32 = _r33 = \
    0; \
  _r00 = _r01 = _r02 = _r03 = \
  _r10 = _r11 = _r12 = _r13 = \
  _r20 = _r21 = _r22 = _r23 = \
  _r30 = _r31 = _r32 = _r33 = \
    1;

static 
void *_tort_fiber_yield(TORT_FIBER_VOLATILE tort_fiber_t *fiber, TORT_FIBER_VOLATILE tort_fiber_t *other_fiber)
{
  _FORCE_REGISTER_SPILL;

  assert(fiber != other_fiber);
  switch ( fiber ? setjmp(fiber->_jb) : 0 ) {
  case 0:
    fprintf(stderr, "  _tort_fiber_yield(%p, %p): switching from fiber %p\n", fiber, other_fiber, fiber);
    longjmp(other_fiber->_jb, 1);
  case 1:
    fprintf(stderr, "  _tort_fiber_yield(%p, %p): switched to fiber %p\n", fiber, other_fiber, fiber);
    return fiber;
  default:
    abort();
  }
  return 0;
}


static
void *_tort_fiber_begin(TORT_FIBER_VOLATILE tort_fiber_t *fiber)
{
  _FORCE_REGISTER_SPILL;

  /* Save return location of parent fiber. */
  switch ( fiber->parent ? setjmp(fiber->parent->_jb) : 0 ) {
  case 0:
    fprintf(stderr, "  _tort_fiber_begin(%p): starting fiber\n", fiber);
    return fiber->func_result = fiber->func(fiber, fiber->func_data);
  case 1:
    fprintf(stderr, "  _tort_fiber_begin(%p): returned to parent fiber\n", fiber);
    return fiber->func_result;
  default:
    abort();
  }
  return 0;
}


size_t _tort_fiber_default_stack_size = 1 * 1024 * 1024;

static
void *_tort_fiber_new(tort_fiber_t *fiber_parent, tort_fiber_func func, void *func_data, size_t size)
{
  tort_fiber_t *fiber = malloc(sizeof(*fiber));
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
  fprintf(stderr, "  fiber(%018p)\n        sp = %018p\n       ptr = %018p\n      dist = 0x%016llx\n",
	  fiber, fiber->parent_sp, fiber->stk_ptr, (long long) dist);
  size_t pad = sizeof(void*);
  fiber->start_sp = __builtin_alloca((dist + pad) & ~pad);
  fprintf(stderr, "    start_sp = %018p\n", fiber->start_sp);
  return _tort_fiber_begin(fiber);
}

/********************************************************************/

static tort_fiber_t *fiber_a, *fiber_b;

static tort_fiber_func_DECL(a);
static tort_fiber_func_DECL(b);

static
tort_fiber_func_DECL(a)
{
  static int n = 10;
  fiber_a = fiber;
  fprintf(stderr, "fiber_a = %p\n", fiber_a);
  while ( n -- ) {
    fprintf(stderr, "a: fiber = %p\n", fiber);
    assert(fiber == fiber_a);
    fprintf(stderr, "a(%s) @ %p: n = %d\n", (char*) data, fiber, n);
    if ( ! fiber_b ) {
      _tort_fiber_new(fiber_a, b, "from a()", (size_t) 0);
    } else {
      _tort_fiber_yield(fiber_a, fiber_b);
    }
  }
  return "a return";
}

static
tort_fiber_func_DECL(b)
{
  static int n = 10;
  fiber_b = fiber;
  fprintf(stderr, "fiber_b = %p\n", fiber_b);
  while ( n -- ) {
    fprintf(stderr, "b: fiber = %p\n", fiber);
    assert(fiber == fiber_b);
    fprintf(stderr, "b(%s) @ %p: n = %d\n", (char*) data, fiber, n);
    _tort_fiber_yield(fiber_b, fiber_a);
  }
  return "b return";
}


int main(int argc, char **argv)
{
  void *result;

  fprintf(stderr, "main() @ %p\n", &argv);

  result = _tort_fiber_new(0, a, "from main()", (size_t) 0);

  return 0;
}
