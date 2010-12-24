#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h> /* memset(). */
#include <setjmp.h>
#include <sys/mman.h>
#include <assert.h>

#define TORT_FIBER_VOLATILE
#ifndef TORT_FIBER_VOLATILE
#define TORT_FIBER_VOLATILE volatile
#endif

struct tort_fiber_t;
typedef struct tort_fiber_t tort_fiber_t;

#define tort_fiber_func_DECL(X) \
  void * X(TORT_FIBER_VOLATILE tort_fiber_t *_tort_fiber, TORT_FIBER_VOLATILE void *data)

typedef tort_fiber_func_DECL((*tort_fiber_func));

struct tort_fiber_t {
  jmp_buf _jb;
#define _tort_setjmp(F) setjmp((F)->_jb)
#define _tort_longjmp(F, V) longjmp((F)->_jb, (V))
  struct tort_fiber_t *parent;
  tort_fiber_func func;
  void *func_result;
  void *func_data;
  void *stk_ptr;
  size_t stk_size;
  void *parent_sp;
  void *start_sp;
};

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

void *_tort_fiber_yield(TORT_FIBER_VOLATILE tort_fiber_t *fiber, TORT_FIBER_VOLATILE tort_fiber_t *other_fiber)
{
  _FLUSH_REGISTERS;
  if ( other_fiber == fiber ) {
    return fiber;
  }
  /* Pause current fiber. */
  switch ( fiber ? _tort_setjmp(fiber) : 0 ) {
  case 0:
    /* Resume other fiber. */
    _tort_longjmp(other_fiber, 1);
  case 1:
    return fiber;
  default:
    abort();
  }
  return 0;
}


static
void *_tort_fiber_begin(TORT_FIBER_VOLATILE tort_fiber_t *fiber)
{
  _FLUSH_REGISTERS;
  /* Pause parent fiber. */
  switch ( fiber->parent ? _tort_setjmp(fiber->parent) : 0 ) {
  case 0:
    /* Invoke new fiber func. */
    return fiber->func_result = fiber->func(fiber, fiber->func_data);
  case 1:
    /* Other fiber resumed parent fiber. */
    return fiber->func_result;
  default:
    abort();
  }
  return 0;
}


size_t _tort_fiber_default_stack_size = 1 * 1024 * 1024;

void *_tort_fiber_new(tort_fiber_t *fiber_parent, tort_fiber_func func, void *func_data, size_t size)
{
  tort_fiber_t *fiber = malloc(sizeof(*fiber));
  memset(fiber, 0, sizeof(*fiber));
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
  fiber_a = _tort_fiber;
  fprintf(stderr, "fiber_a = @%p\n", fiber_a);
  while ( n -- ) {
    // fprintf(stderr, "a: fiber = %p\n", _tort_fiber);
    assert(_tort_fiber == fiber_a);
    fprintf(stderr, "a(%s) @%p @%p: n = %d\n", (char*) data, _tort_fiber, &_tort_fiber, n);
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
  fiber_b = _tort_fiber;
  fprintf(stderr, "fiber_b = @%p\n", fiber_b);
  while ( n -- ) {
    // fprintf(stderr, "b: fiber = @%p\n", _tort_fiber);
    assert(_tort_fiber == fiber_b);
    fprintf(stderr, "b(%s) @ @%p @%p: n = %d\n", (char*) data, _tort_fiber, &_tort_fiber, n);
    _tort_fiber_yield(fiber_b, fiber_a);
  }
  return "b return";
}


int main(int argc, char **argv)
{
  void *result;

  fprintf(stderr, "main() @%p\n", &argv);

  result = _tort_fiber_new(0, a, "from main()", (size_t) 0);

  fprintf(stderr, "  => %s\n", (char*) result);

  return 0;
}
