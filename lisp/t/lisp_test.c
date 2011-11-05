#include "tort/tort.h"
#include "tort/block.h"
#include <stdio.h>

int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v v;
 
  tort_runtime_create();
  // { extern int _tort_dl_debug; _tort_dl_debug = 1; }
  tort_send(tort_s(_dlopen), tort_string_new_cstr("libtortlisp"));

  io = tort_stdout;

  v = tort_vector_new(0, 10);
  tort_printf(io, "v => %T\n", v);

  tort_printf(io, "v as lisp object => %O\n", v);

#if 0
  // segfault
  {
    tort_v o, c;
    printf("\nread lisp object from popen(\"echo 12345\", \"r\") => ");
    v = tort_string_new_cstr("echo 12345");
    c = tort_string_new_cstr("r");
    o = tort_send(tort__s(create), tort_stdin);
    o = tort_send(tort__s(popen), o, v, c);
    v = tort_send(tort__s(lisp_read), o);
    tort_send(tort__s(close), o);
    tort_printf(io, "(read o) => %O\n", v);
  }
#endif

  tort_printf(io, "read lisp object from stdin: ");
  v = tort_send(tort_symbol_new("lisp_read"), tort_stdin);
  tort_printf(io, "(read o) => %O\n", v);

  tort_send(tort_symbol_new("lisp_repl"), tort_stdin, tort_stdout, tort_stdout, tort_nil);

  tort_gc_dump_stats();

  tort_printf(io, "\n\nDONE\n");

  return 0;
}

