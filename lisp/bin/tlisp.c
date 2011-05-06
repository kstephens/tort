#include "tort/tort.h"

int main(int argc, char **argv, char **environ)
{
  tort_v in, out;
  tort_v v;
 
  tort_runtime_create();
  tort_send(tort_s(_dlopen), tort_string_new_cstr("libtortlisp"));

  in = tort_stdin;
  out = tort_stdout;

  v = tort_send(tort_s(lisp_repl), in, out, tort_(root));

  tort_printf(tort_stdout, "%O\n", v);

  return 0;
}

