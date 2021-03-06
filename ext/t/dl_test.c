#include "tort/tort.h"

#include <stdio.h>
#include <assert.h>


int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v st;
  tort_v v;

  tort_runtime_create();

  io = tort_stdout;

  st = tort_send(tort_s(load), tort_mt(dynlib), tort_string_new_cstr("libtortext"));

  // tort_send(tort_s(_inspect), st, io);
  tort_send(tort_s(_inspect), tort_nil, io);
  tort_printf(io, "\n");

  tort_send(tort_s(_inspect), tort_true, io);
  tort_printf(io, "\n");

  tort_send(tort_s(_inspect), tort_false, io);
  tort_printf(io, "\n");

  v = tort_send(tort_s(new), tort_mt(pair), tort_true, tort_false);
  tort_send(tort_s(_inspect), v, io);
  tort_printf(io, "\n");

  tort_printf(io, "\nDONE\n");

  return 0;
}

