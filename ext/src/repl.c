#include "tort/tort.h"
#include "tort/repl.h"

tort_SETTER(repl,tort_v,input);
tort_SETTER(repl,tort_v,output);
tort_SETTER(repl,tort_v,prompt);
tort_SETTER(repl,tort_v,env);
tort_SETTER(repl,tort_v,catch);
tort_SETTER(repl,tort_v,running);
tort_SETTER(repl,tort_v,expr);
tort_SETTER(repl,tort_v,result);

tort_v _tort_M_repl__new(tort_tp tort_mtable *mtable)
{
  tort_repl *repl = tort_allocate(mtable, sizeof(*repl));
  repl->input = tort_nil;
  repl->output = tort_nil;
  repl->prompt = tort_nil;
  repl->env = tort_nil;
  repl->catch = tort_nil;
  repl->running = tort_false;
  repl->expr = tort_nil;
  repl->result = tort_nil;
  return repl;
}

tort_v _tort_m_repl__value(tort_tp tort_repl *repl, tort_v catch)
{
  tort_v error_catch_save = tort_(error_catch);
  if ( catch )
    tort_(error_catch) = catch;
  do {
    if ( repl->prompt != tort_nil ) tort_printf(repl->prompt, " > ");
    tort_send(tort_s(read), repl);
    tort_printf(tort_stderr, "  REPL expr => %O\n", repl->expr);
    if ( repl->expr == tort_eos ) {
      repl->running = tort_false;
      break;
    }
    if ( repl->prompt != tort_nil ) {
      tort_printf(repl->prompt, "  == ");
      if ( repl->output != tort_nil ) {
	tort_send(tort_s(print), repl, repl->expr);
      }
      tort_printf(repl->prompt, "\n");
    }
    tort_send(tort_s(eval), repl);
    if ( repl->output != tort_nil ) {
      if ( repl->prompt != tort_nil ) tort_printf(repl->prompt, "  => ");
      tort_send(tort_s(print), repl, repl->result);
    }
  } while ( 0 ); // break;
  if ( catch )
    tort_(error_catch) = error_catch_save;
  return repl;
}

tort_v _tort_m_repl__run(tort_tp tort_repl *repl)
{
  if ( repl->env == tort_nil ) {
    tort_send(tort_s(new_environment), repl);
  }
  repl->running = tort_true;
  do {
    // invoke repl.value(catch).
    if ( repl->catch == tort_nil ) {
      tort_send(tort_s(value), repl, repl->catch);
    } else {
      tort_send(tort_s(begin), repl->catch, repl);
    }
  } while ( repl->running != tort_false );
  return repl;
}

tort_v tort_runtime_initialize_repl()
{
  tort_mtable_make("repl", 0);
  return tort_nil;
}

