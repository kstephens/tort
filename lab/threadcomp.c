#include <stdio.h>

typedef enum isn_t {
  isn__BEGIN,
  isn__PREAMBLE,
  isn__PREAMBLE_COMPILED,
#define ISN(name, narg, body) isn_##name,
#include "isn.h"
  isn__END
} isn_t;

typedef void *word_t;

static
void *isn_to_addr[isn__END + 1];

static word_t stack[16];
static word_t *top = stack + 16;

static
void run_compiled(word_t *pc)
{
  if ( pc == 0 ) {
#define ISN(name, narg, body) isn_to_addr[isn_##name] = &&isn_addr_##name;
#include "isn.h"
    return;
  }

#define push(X) *(-- top) = (word_t) (X)
#define pop() *(top ++)

#define args_0() (void) 0
#define args_1() word_t arg0 = *(pc ++)

  goto * *(pc ++);

#define ISN(name, narg, body) \
  isn_addr_##name: {	      \
    args_##narg();	      \
    body;		      \
    goto * *(pc ++);	      \
  }
#include "isn.h"
}

static 
void run(word_t **pc_p)
{
  word_t *isn_p;
  word_t *pc = *pc_p;

 next_isn:
  switch ( (isn_t) *(isn_p = pc ++) ) {
  case isn__PREAMBLE:
    *isn_p = (word_t) isn__PREAMBLE_COMPILED;
    goto next_isn;

  case isn__PREAMBLE_COMPILED:
    return run_compiled(*pc_p = pc);

#define ISN(name, narg, body)			\
    case isn_##name:				\
      *isn_p = isn_to_addr[isn_##name]; {	\
      args_##narg();				\
      body;					\
      goto next_isn;				\
    }
#include "isn.h"
  default:
    goto * *isn_p;
  }
}

int main(int argc, char **argv)
{
  word_t program[] = {
    isn__PREAMBLE,
    isn_LIT, 2,
    isn_LIT, 3,
    isn_ADD,
    isn_PRINT,
    isn_RTN,
    0
  };
  word_t *pc = program;

  run_compiled(0);
  run(&pc);
  run(&pc);

  return 0;
}

