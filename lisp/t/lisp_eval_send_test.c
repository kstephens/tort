#include "tort/tort.h"
#include "tort/lisp.h"

int main(int argc, char **argv, char **environ)
{
  tort_v env, expr, closure, v;
 
  tort_runtime_create();
  tort_send(tort_s(load), tort_mt(dynlib), tort_string_new_cstr("libtortlisp"));

  env = tort_send(tort_s(new), tort_mt(lisp_environment), tort_nil, tort_nil, tort_nil, tort_nil);
  expr = /* (lambda () ('+ 1 2)) */
    tort_cons(tort_s(lambda), 
    tort_cons(tort_nil,
    tort_cons(tort_cons(tort_cons(tort_s(quote), tort_cons(tort_s(ADD), tort_nil)),
	      tort_cons(tort_i(1),
	      tort_cons(tort_i(2), tort_nil))),
    tort_nil)));
  tort_printf(tort_stderr, "     expr = %O\n", expr);
  closure = tort_send(tort_s(lisp_eval), expr, env);
  tort_printf(tort_stderr, "  closure = %O\n", closure);
  // _tort_lookup_trace ++;
  tort_printf(tort_stderr, "  (send '%O nil)\n", closure);
  v = tort_sendn(closure, 0, tort_nil);
  // _tort_lookup_trace --;
  tort_printf(tort_stderr, "  result = %O\n", v);
  assert(tort_I(v) == 3);
  tort_printf(tort_stdout, "\n\nDONE\n");

  return 0;
}

