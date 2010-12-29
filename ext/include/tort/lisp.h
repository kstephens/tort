#ifndef _tort_LISP_H
#define _tort_LISP_H

#include "tort/core.h"

typedef struct tort_pair {
  tort_v car, cdr;
} tort_pair;

tort_v tort_cons(tort_v a, tort_v b);
tort_v tort_car(tort_v x);
tort_v tort_cdr(tort_v x);
tort_v tort_caar(tort_v x);
tort_v tort_cdar(tort_v x);
tort_v tort_cadr(tort_v x);
tort_v tort_cddr(tort_v x);
tort_v tort_caddr(tort_v x);

#endif
