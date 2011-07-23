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
    extern int _tort_lisp_trace;
    const char *str = getenv("TORT_LISP_LIB_DIR");
    tort_v boot = tort_string_new_cstr(str && *str ? str : TORT_LISP_LIB_DIR);
    tort_v io;
    tort_v out_io;
    int boot_debug;

    str = getenv("TORT_LISP_BOOT_DEBUG");
    boot_debug = str && *str != '0';
    out_io = boot_debug ? tort_stderr : tort_nil;
    if ( boot_debug ) ++ _tort_lisp_trace;

    tort_send(tort_s(append), boot, tort_string_new_cstr("/boot.lisp"));
    tort_printf(tort_stdout, ";; %s: reading %T\n", argv[0], boot);
    io = tort_send(tort_s(__create), tort__mt(io), (FILE*) 0);
    io = tort_send(tort_s(open), io, boot, tort_string_new_cstr("r"));
    env = tort_send(tort_s(lisp_repl), io, out_io, tort_nil, tort_nil);

    if ( boot_debug ) -- _tort_lisp_trace;
  }

  env = tort_send(tort_s(lisp_repl), in, out, out, env);

  tort_printf(tort_stdout, "%O\n", env);

  return 0;
}

