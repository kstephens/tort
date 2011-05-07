#include "tort/tort.h"

int main(int argc, char **argv, char **environ)
{
  tort_v in, out;
  tort_v env;
 
  tort_runtime_create();
  tort_send(tort_s(_dlopen), tort_string_new_cstr("libtortlisp"));

  in = tort_stdin;
  out = tort_stdout;

  {
    const char *str = getenv("TORT_LISP_LIB_DIR");
    tort_v boot = tort_string_new_cstr(str && *str ? str : TORT_LISP_LIB_DIR);
    tort_v io;
    tort_send(tort_s(append), boot, tort_string_new_cstr("/boot.lisp"));
    tort_printf(tort_stdout, ";; %s: reading %T\n", argv[0], boot);
    io = tort_send(tort_s(__create), tort__mt(io), (FILE*) 0);
    io = tort_send(tort_s(open), io, boot, tort_string_new_cstr("r"));
    env = tort_send(tort_s(lisp_repl), io, tort_nil, tort_nil, tort_nil);
  }

  env = tort_send(tort_s(lisp_repl), in, out, out, env);

  tort_printf(tort_stdout, "%O\n", env);

  return 0;
}

