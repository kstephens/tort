#ifndef _tort_repl_h
#define _tort_repl_h

#include "tort/tort.h"

typedef struct tort_repl {
  tort_v input;
  tort_v output;
  tort_v prompt;
  tort_v prompt_id;
  tort_v env;
  tort_v catch;
  tort_v caught;
  tort_v running;
  tort_v expr;
  tort_v result;
} tort_repl;

#endif
