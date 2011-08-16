#include "tort/tort.h"

static ssize_t the_answer = 42;
tort_v _tort_m_class__return_42(tort_tp tort_v rcvr)
{
  return tort_i(the_answer);
}

static tort_v selector_1;
static tort_v rcvr_1;
static tort_v arg_1;
tort_v _tort_m_class__call_other(tort_tp tort_v rcvr, tort_v sel)
{
  tort_send(selector_1, rcvr_1, arg_1);
  return_tort_send(sel, rcvr);
}

