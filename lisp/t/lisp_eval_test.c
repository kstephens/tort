#include "tort/tort.h"
#include "tort/repl.h"

#include <stdio.h>

int main(int argc, char **argv, char **environ)
{
  tort_v in, out;
  tort_v v;
  tort_repl *repl = tort_nil;
 
  tort_runtime_create();
  tort_send(tort_s(load), tort_mt(dynlib), tort_string_new_cstr("libtortlisp"));

  in = tort_stdin;
  out = tort_stdout;

  repl = tort_send(tort_s(new), tort_mt(lisp_repl));
  tort_send(tort_s(new_environment), repl);
  repl->input = in;
  repl->output = out;
  repl->message = tort_nil;
  repl->prompt = tort_nil;

  v = tort_send(tort_s(run), repl);

  tort_printf(out, "\n\nDONE\n");

  return 0;
}

