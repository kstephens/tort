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
  void * X(tort_fiber_t *_tort_fiber_ptr, void *data)

typedef tort_fiber_func_DECL((*tort_fiber_func));

typedef 
enum tort_fiber_status_t {
  CREATED = 0,
  INITIALIZED,
  STARTING,
  RUNNING,
  PAUSED,
  EXITED,
  TERMINATED
} tort_fiber_status_t;

struct tort_fiber_t {
  struct tort_fiber_t *_prev, *_next;

  void *data;
  tort_fiber_status_t status;
  ucontext_t _ucontext;
  struct tort_fiber_t *parent;

  jmp_buf _func_exit;
  int _func_exit_valid;
  tort_fiber_func func;
  void *func_data;
  void *func_result;

  void *stk_base;
  void *stk_mmap;
  size_t stk_size;
  void *stk_sp;

  struct tort_fiber_t *prev;
  const char *error_str;
  int error_code;
};

extern size_t _tort_fiber_default_stack_size;
extern void  (*__tort_fiber_mmap_region)(void *addr, size_t size);
extern void  (*__tort_fiber_munmap_region)(void *addr, size_t size);

void  __tort_fiber_init(tort_fiber_t *fiber, size_t size);
void  __tort_fiber_destroy(tort_fiber_t *fiber);
void  __tort_fiber_terminate(tort_fiber_t *fiber, void *func_result);
void  __tort_fiber_begin(tort_fiber_t *fiber, tort_fiber_t *fiber_parent, tort_fiber_func func, void *func_data);
void  __tort_fiber_yield(tort_fiber_t *fiber, tort_fiber_t *other_fiber);

tort_fiber_t *__tort_fiber_current();
tort_fiber_t *__tort_fiber_main();

#endif
