#include <stdio.h>
#include <stdlib.h>

typedef enum isn_t {
  isn__BEGIN,
#define ISN(name, narg, body) isn_##name,
#include "isn.h"
  isn__END
} isn_t;

typedef void *word_t;

static void *isn_to_addr[isn__END + 1];
static char *isn_to_name[isn__END + 1];

static 
void run(word_t **pc_p, word_t **sp_p);

#define DEBUG 1

#if DEBUG
#define TRACE(NAME)  fprintf(stderr, "    %p: pc=%p sp=%p %15s (%p) \t (in %s)\n", pc_p, pc - 1, sp, #NAME, pc[-1], __FUNCTION__)
#define TRACE_JMP(X) fprintf(stderr, "    %p: pc <= %p\n", pc_p, X)
#else
#define TRACE(NAME)
#define TRACE_JMP(X)
#endif

typedef void (*run_proc_t)(word_t **pc_p, word_t **sp_p);

#define CALLX(PC, SP) \
  ((run_proc_t)*(word_t*)(PC))(&(PC), &(SP))

static void run_compiled(word_t **pc_p, word_t **sp_p);

static
void compile(word_t **pc_p)
{
  word_t *pc;
  word_t *isn_p;
  word_t *sp = 0;

  /* INIT */
  if ( ! isn_to_addr[0] ) {
    isn_to_addr[0] = (void*) 1;
    run_compiled(0, 0);
  }

  pc = *pc_p;
  
#if DEBUG
  printf("  %s(%p)\n", __FUNCTION__, pc);
#endif

  *(pc ++) = run_compiled;

#define args_0() (void) 0
#define args_1() pc += 1
#define args_2() pc += 2

 next_isn:
  switch ( (isn_t) *(isn_p = pc ++) ) {
#define ISN(name, narg, body)			\
    case isn_##name: TRACE(name);		\
      *isn_p = isn_to_addr[isn_##name]; {	\
	args_##narg();				\
      }						\
    goto next_isn;
#include "isn.h"

  case isn__BEGIN:
  case isn__END:
    break;

  default:
    fprintf(stderr, "  %s: unknown isn %p @ %p\n", __FUNCTION__, pc[-1], pc - 1);
    abort();
  }
    
#undef args_0
#undef args_1
#undef args_2
}

static
void run_compiled(word_t **pc_p, word_t **sp_p)
{
  word_t *pc;
  word_t *sp;

#define push(X) *(-- sp) = (word_t) (X)
#define pop() *(sp ++)

#define args_0() (void) 0
#define args_1() word_t arg0 = *(pc ++)
#define args_2() word_t arg0 = pc[0]; word_t arg1 = pc[1]; pc += 2;

  if ( ! pc_p ) {
#define ISN(name, narg, body)				\
    isn_to_addr[isn_##name] = &&isn_addr_##name;	\
    isn_to_name[isn_##name] = #name;
#include "isn.h"

    return;
  }

  pc = *pc_p;
  sp = *sp_p;

#if DEBUG
  printf("  %s(%p => %p, %p => %p)\n", __FUNCTION__, pc_p, pc, sp_p, sp);
#endif

 call_tail:
  if ( *pc != run_compiled ) {
    pc_p = &pc;
    compile(pc_p);
  }
  ++ pc; /* skip run_proc dispatch word. */

  goto * *(pc ++);

#define ISN(name, narg, body)			\
  isn_addr_##name: TRACE(name); {		\
    args_##narg();				\
    body;					\
    goto * *(pc ++);				\
  }
#include "isn.h"
}

static 
void run(word_t **pc_p, word_t **sp_p)
{
  word_t *pc = *pc_p;
  word_t *sp = *sp_p;

#if DEBUG
  printf("  %s(%p => %p, %p => %p)\n", __FUNCTION__, pc_p, pc, sp_p, sp);
#endif

 call_tail:
  if ( *pc != run_compiled ) {
    compile(pc_p);
  }
  return run_compiled(pc_p, sp_p);

  ++ pc; /* skip run_proc dispatch word. */

 next_isn:
  switch ( (isn_t) *(pc ++) ) {
#define ISN(name, narg, body)			\
    case isn_##name: TRACE(name);		\
      {						\
	args_##narg();				\
	body;					\
      }						\
      goto next_isn;				\

#include "isn.h"
  default:
    fprintf(stderr, "  %s: unknown isn %p @ %p\n", __FUNCTION__, pc[-1], pc - 1);
    abort();
  }
#undef args_0
#undef args_1
#undef args_2
}

int main(int argc, char **argv)
{
  static word_t stack[16];
  static word_t *top = stack + 16;

  static word_t add_3[] = { 
    run,
    isn_LIT_, 3,
    isn_ADD,
    isn_RTN,
    0
  };

  static word_t add_3_print[] = {
    run,
    isn_CALL_, add_3,
    isn_PRINT,
    isn_RTN,
    0
  };
  static word_t program[] = {
    run,
    isn_LIT_, 2,
    isn_CALL_TAIL_, add_3_print,
    0
  };
  word_t *pc = program;

  CALLX(pc, top);
  CALLX(pc, top);

  CALLX(pc, top);
  CALLX(pc, top);

  return 0;
}

