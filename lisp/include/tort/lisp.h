#ifndef _tort_LISP_H
#define _tort_LISP_H

#include "tort/core.h"

extern int _tort_lisp_trace, _tort_lisp_macro_trace;

typedef struct tort_cons { tort_H; /* same layout as tort_pair. */
  tort_v car, cdr; 
} tort_cons;

#define tort_cons(A,R) tort_send(tort__s(new), tort_mt(cons), A, R)
tort_v tort_car(tort_v x);
tort_v tort_cdr(tort_v x);
tort_v tort_caar(tort_v x);
tort_v tort_cdar(tort_v x);
tort_v tort_cadr(tort_v x);
tort_v tort_cddr(tort_v x);
tort_v tort_caddr(tort_v x);
tort_v tort_cadddr(tort_v x);

#endif
