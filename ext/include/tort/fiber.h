#ifndef _tort_fiber_h
#define _tort_fiber_h 1

#include "tort/core.h"

#include <setjmp.h>
#if defined(__APPLE__)
/* OS X annoyance: http://duriansoftware.com/joe/PSA:-avoiding-the-%22ucontext-routines-are-deprecated%22-error-on-Mac-OS-X-Snow-Leopard.html */
#include <sys/ucontext.h>
int  getcontext(ucontext_t *);
void makecontext(ucontext_t *, void (*)(), int, ...);
int  setcontext(const ucontext_t *);
int  swapcontext(ucontext_t * __restrict, const ucontext_t * __restrict);
#else
#include <ucontext.h>
#endif

#define TORT_FIBER_VOLATILE
#ifndef TORT_FIBER_VOLATILE
#define TORT_FIBER_VOLATILE volatile
#endif

struct tort_fiber_t;
typedef struct tort_fiber_t tort_fiber_t;

#define tort_fiber_func_DECL(X) \
  void * X(TORT_FIBER_VOLATILE tort_fiber_t *_tort_fiber_ptr, TORT_FIBER_VOLATILE void *data)

typedef tort_fiber_func_DECL((*tort_fiber_func));

typedef 
enum tort_fiber_status_t {
  CREATED = 0,
  RUNNING,
  PAUSED,
  TERMINATED
} tort_fiber_status_t;

struct tort_fiber_t {
  void *data;
  tort_fiber_status_t status;
#if 1
  ucontext_t _ucontext;
#define _tort_setjmp(F) getcontext(&(F)->_ucontext)
#define _tort_longjmp(F, V) setcontext(&(F)->_ucontext)
#else
  jmp_buf _jb;
#define _tort_setjmp(F) setjmp((F)->_jb)
#define _tort_longjmp(F, V) longjmp((F)->_jb, (V))
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

extern size_t _tort_fiber_default_stack_size;
extern void *(*__tort_fiber_allocate)(size_t size);

void *__tort_fiber_new(tort_fiber_t *fiber_parent, tort_fiber_func func, void *func_data, size_t size);

void *__tort_fiber_yield(TORT_FIBER_VOLATILE tort_fiber_t *fiber, TORT_FIBER_VOLATILE tort_fiber_t *other_fiber);

tort_v tort_runtime_initialize_fiber();

#endif
