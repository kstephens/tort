#include "tort/tort.h"
#include "tort/repl.h"

int main(int argc, char **argv, char **environ)
{
  int argi;
  tort_v in, out, io;
  tort_repl *repl;
  int interactive = 0, verbose = 0;

  tort_runtime_create();
  tort_send(tort_s(load), tort_mt(dynlib), tort_string_new_cstr("libtortlisp"));

  in = tort_stdin;
  out = tort_stdout;

  repl = tort_send(tort_s(new), tort_mt(lisp_repl));
  if ( repl->catch == tort_nil && tort_mt(catch) ) {
    repl->catch = tort_send(tort__s(new), tort_mt(catch));
  }

  {
    extern int _tort_lisp_trace, _tort_lisp_macro_trace;
    const char *str = getenv("TORT_LISP_LIB_DIR");
    tort_v boot = tort_string_new_cstr(str && *str ? str : TORT_LISP_LIB_DIR);
    tort_v out_io;
    int boot_debug;

    str = getenv("TORT_LISP_BOOT_DEBUG");
    boot_debug = atoi(str ? str : "0");
    out_io = boot_debug ? tort_stderr : tort_nil;
    _tort_lisp_macro_trace += boot_debug;
    _tort_lisp_trace += boot_debug;

    tort_send(tort_s(append), boot, tort_string_new_cstr("/boot.lisp"));
    tort_printf(out, ";; %s: reading %T\n", argv[0], boot);
    io = tort_send(tort_s(new), tort__mt(io));
    io = tort_send(tort_s(open), io, boot, tort_string_new_cstr("r"));
    repl->input = io;
    repl->output = out_io;
    tort_send(tort_s(run), repl);

    _tort_lisp_macro_trace -= boot_debug;
    _tort_lisp_trace -= boot_debug;
  }

  for ( argi = 1; argi < argc; ++ argi ) {
    char *arg = argv[argi];
    if ( ! strcmp(arg, "-i") ) {
      interactive ++;
    } else
    if ( ! strcmp(arg, "-v") ) {
      verbose ++;
    } else
    if ( ! strcmp(arg, "-") ) {
      in = stdin;
    } else {
      in = tort_string_new_cstr(arg);
      tort_printf(out, ";; %s: reading %T\n", argv[0], in);
      io = tort_send(tort_s(new), tort__mt(io));
      io = tort_send(tort_s(open), io, in, tort_string_new_cstr("r"));
      in = io;
    }
    out = tort_stdout;
    repl->input = io;
    repl->output = out;
    if ( verbose )
      repl->prompt = out;
    tort_send(tort_s(run), repl);
  }
  if ( argc == 1 || interactive ) {
    in = tort_stdin;
    out = tort_stdout;
    repl->input = in;
    repl->output = out;
    repl->prompt = out;
    tort_printf(out, ";; %s: READY:\n", argv[0]);
    tort_send(tort_s(run), repl);
  }
  tort_send(tort_s(print), repl, repl->result);
  return 0;
}

