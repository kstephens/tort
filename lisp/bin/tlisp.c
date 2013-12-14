#include "tort/tort.h"
#include "tort/lisp.h"
#include "tort/repl.h"

int main(int argc, char **argv, char **environ)
{
  int argi;
  tort_v in = tort_nil, out = tort_nil;
  tort_repl *repl = tort_nil;
  int interactive = 0, verbose = 0;

  (void) out;

  tort_runtime_create();
  tort_send(tort_s(load), tort_mt(dynlib), tort_string_new_cstr("libtortlisp"));

  repl = tort_send(tort_s(new), tort_mt(lisp_repl));
  tort_send(tort_s(new_environment), repl);
  if ( repl->catch == tort_nil && tort_mt(catch) ) {
    repl->catch = tort_send(tort__s(new), tort_mt(catch));
  }

  {
    tort_v boot = tort_send(tort_s(get), tort_(root), tort_s(lisp_lib_dir));
    tort_v out_io;
    char *str; int boot_debug;

    str = getenv("TORT_LISP_BOOT_DEBUG");
    boot_debug = atoi(str ? str : "0");
    out_io = boot_debug ? tort_stderr : tort_nil;
    _tort_lisp_macro_trace += boot_debug;
    _tort_lisp_trace += boot_debug;

    boot = tort_send(tort_s(clone), boot);
    tort_send(tort_s(append), boot, tort_string_new_cstr("/boot.lisp"));
    repl->message = tort_stderr;
    repl->output = out_io;
    tort_send(tort_s(load), repl, boot);
    repl->message = tort_nil;

    _tort_lisp_macro_trace -= boot_debug;
    _tort_lisp_trace -= boot_debug;
  }

  for ( argi = 1; argi < argc; ++ argi ) {
    char *arg = argv[argi];
    if ( ! strcmp(arg, "-i") ) {
      interactive ++;
      continue;
    } else
    if ( ! strcmp(arg, "-v") ) {
      verbose ++;
      continue;
    } else
    if ( ! strcmp(arg, "-") ) {
      in = tort_stdin;
      out = tort_nil;
    } else {
      in = tort_string_new_cstr(arg);
      if ( verbose ) repl->message = tort_stderr;
      tort_send(tort_s(load), repl, in);
      repl->message = tort_nil;
    }
  }
  if ( argc == 1 || interactive ) {
    repl->input = tort_stdin;
    repl->output = tort_stdout;
    repl->message = tort_stderr;
    repl->prompt = tort_stdout;
    tort_printf(repl->prompt, ";; %s: READY:\n", argv[0]);
    tort_send(tort_s(run), repl);
  }
  tort_send(tort_s(print), repl, repl->result);
  return 0;
}

int tort_hello_world()
{
  printf("Hello, World!\n");
  return 42;
}

int tort_prints(const char *str)
{
  printf("%s\n", str);
  return 42;
}

const char* tort_tlisp_version()
{
  return "tlisp 1.0";
}

