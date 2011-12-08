#include "tort/tort.h"
#include <stdarg.h>

static tort_v selector_1;
static tort_v rcvr_1;
static tort_v arg_1;
tort_v _tort__send_other(tort_tp tort_v rcvr, tort_v sel)
{
  tort_send(selector_1, rcvr_1, arg_1);
  return_tort_send(sel, rcvr);
}

