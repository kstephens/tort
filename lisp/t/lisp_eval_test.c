#include "tort/tort.h"

#include <stdio.h>

int main(int argc, char **argv, char **environ)
{
  tort_v in, out;
  tort_v v;
 
  tort_runtime_create();
  tort_send(tort_s(load), tort_mt(dynlib), tort_string_new_cstr("libtortlisp"));

  in = tort_stdin;
  out = tort_stdout;

  v = tort_send(tort_s(lisp_repl), in, out, out, tort_nil);

  tort_printf(out, "\n\nDONE\n");

  return 0;
}

