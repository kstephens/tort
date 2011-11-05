#include "tort/tort.h"
#include <stdio.h>
#include <assert.h>

static tort_v not_found_rcvr = 0;
static tort_v not_found(tort_tp tort_v rcvr, ...)
{
  fprintf(stderr, "\n not_found %s\n", tort_symbol_data(_tort_message->selector));
  not_found_rcvr = rcvr;
  return rcvr;
}

int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_runtime_create();
  tort_(_m_method_not_found) = tort_method_make(not_found, 0);
  io = tort_stdout;
  tort_send(tort_s(foobar), io);
  assert(not_found_rcvr == io);
  printf("\nDONE\n");
  return 0;
}

