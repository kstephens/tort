#include "tort/tort.h"

#include <stdio.h>
#include <assert.h>


int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v st;

  tort_runtime_create();

  io = tort_stdout;

  st = tort_send(tort_s(_dlopen), tort_string_new_cstr(LIB_DIR "/libtortext.dylib"));

  tort_send(tort_s(_inspect), st, io);

  tort_send(tort_s(lisp_write), tort_nil, io);

  return 0;
}

