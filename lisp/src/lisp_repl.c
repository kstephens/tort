#include "tort/lisp.h"
#include "tort/repl.h"

tort_v _tort_m_lisp_repl__read(tort_tp tort_repl *repl)
{
  repl->expr = tort_send(tort_s(lisp_read), repl->input);
  return repl;
}
tort_v _tort_m_lisp_repl__new_environment(tort_tp tort_repl *repl)
{
  tort_v env = 
    tort_send(tort__s(new), tort_mt(lisp_environment), 
	      tort_nil, tort_nil, tort_nil, tort_nil);
  tort_send(tort_s(add), env, tort_s(ANDrepl), repl);
  repl->env = env;
  return repl;
}
tort_v _tort_m_lisp_repl__caughtE(tort_tp tort_repl *repl, tort_v value)
{
  _tort_lisp_trace = _tort_lisp_macro_trace = 0;
  return repl;
}
tort_v _tort_m_lisp_repl__eval(tort_tp tort_repl *repl)
{
  tort_send(tort_s(msgSET), repl->env, _tort_message->previous_message); // FIXME?
  repl->result = tort_send(tort_s(lisp_eval_top_level), repl->expr, repl->env);
  return repl;
}
tort_v _tort_m_lisp_repl__print(tort_tp tort_repl *repl, tort_v thing)
{
  tort_send(tort_s(lisp_write), thing, repl->output);
  return repl;
}

tort_v _tort_m_lisp_repl__load(tort_tp tort_repl *repl_main, tort_v name)
{
  tort_repl *repl;
  tort_v io, result;
  repl = tort_send(tort_s(clone), repl_main);
  repl->prompt_id = name;
  if ( repl->main == tort_nil )
    repl->main = repl_main;
  io = tort_send(tort_s(new), tort__mt(io));
  io = tort_send(tort_s(open), io, name, tort_string_new_cstr("r"));
  if ( io == tort_nil )
    return tort_error(tort_ta "load: cannot open %T\n", name);
  if ( repl->message != tort_nil )
    tort_printf(repl->message, ";; load %T\n", name);
  repl->input = io;
  result = tort_send(tort_s(run), repl);
  if ( repl->message != tort_nil )
    tort_printf(repl->message, ";; load %T: done\n", name);
  return result;
}

