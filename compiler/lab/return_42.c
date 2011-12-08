#include "tort/tort.h"
#include <stdarg.h>

static ssize_t the_answer = 42;
tort_v _tort_return_42(tort_tp tort_v rcvr)
{
  return tort_i(the_answer);
}
