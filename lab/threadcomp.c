#include <stdio.h>

typedef enum isn_t {
  isn__BEGIN,
  isn__PREAMBLE,
  isn__PREAMBLE_,
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
#define TRACE(NAME)  fprintf(stderr, "  %p: pc=%p sp=%p %s \t (in %s)\n", pc_p, pc - 1, sp, #NAME, __FUNCTION__)
#define TRACE_JMP(X) fprintf(stderr, "  %p: pc <= %p\n", pc_p, X)
#else
#define TRACE(NAME)
#define TRACE_JMP(X)
#endif

static
void run_compiled(word_t **pc_p, word_t **sp_p)
{
  word_t *pc;
  word_t *sp;

  if ( pc_p == 0 ) {
#define ISN(name, narg, body) \
    isn_to_addr[isn_##name] = &&isn_addr_##name; \
    isn_to_name[isn_##name] = #name;
#include "isn.h"
    return;
  }
  pc = *pc_p;
  sp = *sp_p;

#if DEBUG
  printf("%s(%p => %p, %p => %p)\n", __FUNCTION__, pc_p, pc, sp_p, sp);
#endif

#define push(X) *(-- sp) = (word_t) (X)
#define pop() *(sp ++)

#define args_0() (void) 0
#define args_1() word_t arg0 = *(pc ++)

  goto * *(pc ++);

#define ISN(name, narg, body) \
  isn_addr_##name: TRACE(name); {		\
    args_##narg();	      \
    body;		      \
    goto * *(pc ++);	      \
  }
#include "isn.h"
}

static 
void run(word_t **pc_p, word_t **sp_p)
{
  word_t *pc = *pc_p;
  word_t *sp = *sp_p;
  word_t *isn_p;

#if DEBUG
  printf("%s(%p => %p, %p => %p)\n", __FUNCTION__, pc_p, pc, sp_p, sp);
#endif

 next_isn:
  if ( *(isn_p = pc ++) >= (word_t) isn__END )
    goto jmp_compiled;
  switch ( (isn_t) *isn_p ) {
  case isn__PREAMBLE:
    TRACE(_PREAMBLE);
    *isn_p = (word_t) isn__PREAMBLE_;
    if ( *pc_p != pc ) {
      fprintf(stderr, "    rewrite %p => %p\n", *pc_p, pc);
    }
    *pc_p = pc;
    *sp_p = sp;
    goto next_isn;

  case isn__PREAMBLE_:
    TRACE(_PREAMBLE_);
    if ( *pc_p != pc ) {
      fprintf(stderr, "    rewrite %p => %p\n", *pc_p, pc);
    }
    *pc_p = pc;
    *sp_p = sp;
    return run_compiled(pc_p, sp_p);

#define ISN(name, narg, body)			\
    case isn_##name: TRACE(name);		\
      *isn_p = isn_to_addr[isn_##name]; {	\
      args_##narg();				\
      body;					\
      goto next_isn;				\
    }
#include "isn.h"
  default:
  jmp_compiled:
    -- pc;
    *sp_p = sp;
#if DEBUG
    fprintf(stderr, "  jmp_compiled for isn %p: %p, %p\n", *pc, pc, sp);
#endif
    run_compiled(&pc, sp_p);
    return;
  }
}

int main(int argc, char **argv)
{
  static word_t stack[16];
  static word_t *top = stack + 16;

  static word_t add_3_print[] = {
    isn__PREAMBLE,
    isn_LIT, 3,
    isn_ADD,
    isn_PRINT,
    isn_RTN,
    0
  };
  static word_t program[] = {
    isn__PREAMBLE,
    isn_LIT, 2,
    isn_CALL_TAIL_, add_3_print,
    0
  };
  word_t *pc = program;

  /* INIT */
  run_compiled(0, 0);

  run(&pc, &top);
  run(&pc, &top);
  run_compiled(&pc, &top);

  return 0;
}

