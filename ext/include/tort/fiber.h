#ifndef _tort_fiber_h
#define _tort_fiber_h 1

#include "tort/core.h"

#include <setjmp.h>

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

void *_tort_fiber_yield(TORT_FIBER_VOLATILE tort_fiber_t *fiber, TORT_FIBER_VOLATILE tort_fiber_t *other_fiber);
extern size_t _tort_fiber_default_stack_size;

void *_tort_fiber_new(tort_fiber_t *fiber_parent, tort_fiber_func func, void *func_data, size_t size);

#endif
