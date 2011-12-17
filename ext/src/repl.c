#include "tort/tort.h"
#include "tort/repl.h"

tort_SLOT(repl,tort_v,input);
tort_SLOT(repl,tort_v,output);
tort_SLOT(repl,tort_v,message);
tort_SLOT(repl,tort_v,prompt);
tort_SLOT(repl,tort_v,env);
tort_SLOT(repl,tort_v,catch);
tort_SLOT(repl,tort_v,caught);
tort_SLOT(repl,tort_v,running);
tort_SLOT(repl,tort_v,expr);
tort_SLOT(repl,tort_v,result);
tort_SLOT(repl,tort_v,main);

tort_v _tort_M_repl__new(tort_tp tort_mtable *mtable)
{
  tort_repl *repl = tort_send(tort__s(_allocate), mtable, tort_i(sizeof(*repl)));
  repl->input = tort_nil;
  repl->output = tort_nil;
  repl->message = tort_nil;
  repl->prompt = tort_nil;
  repl->prompt_id = tort_nil;
  repl->env = tort_nil;
  repl->catch = tort_nil;
  repl->caught = tort_nil;
  repl->running = tort_false;
  repl->expr = tort_nil;
  repl->result = tort_nil;
  repl->main = tort_nil;
  return repl;
}

tort_v _tort_m_repl__caughtE(tort_tp tort_repl *repl, tort_v catch)
{
  return repl;
}

tort_v _tort_m_repl__value(tort_tp tort_repl *repl, tort_v catch)
{
  tort_v error_catch_save = tort_(error_catch);
  if ( catch )
    tort_(error_catch) = catch;
  do {
    if ( repl->prompt != tort_nil )
      tort_printf(repl->prompt, "#|%S>|# ", repl->prompt_id);
    tort_send(tort_s(read), repl);
    // tort_printf(tort_stderr, "  REPL expr => %O\n", repl->expr);
    if ( repl->expr == tort_eos ) {
      repl->running = tort_false;
      break;
    }
    if ( repl->prompt != tort_nil ) {
      tort_printf(repl->prompt, "  #|== ");
      if ( repl->output != tort_nil )
	tort_send(tort_s(print), repl, repl->expr);
      if ( repl->prompt != tort_nil ) 
	tort_printf(repl->prompt, " |#\n");
      else
	tort_printf(repl->output, "\n");
    }
    tort_send(tort_s(eval), repl);
    if ( repl->output != tort_nil ) {
      if ( repl->prompt != tort_nil ) 
	tort_printf(repl->prompt, "  #|=> ");
      tort_send(tort_s(print), repl, repl->result);
      if ( repl->prompt != tort_nil )
	tort_printf(repl->prompt, " |#\n");
      else
	tort_printf(repl->output, "\n");
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
      repl->caught = tort_send(tort_s(begin), repl->catch, repl);
      if ( tort_send(tort_s(applied), repl->catch) != tort_false ) {
	tort_send(tort_s(caughtE), repl, repl->caught);
	tort_printf(tort_stderr, "\nExpression aborted\n");
	if ( repl->main != tort_nil ) {
	  tort_printf(tort_stderr, "Resuming REPL %T\n", repl->main->prompt_id);
	  tort_send(tort_s(run), repl->main);
	}
      }
    }
  } while ( repl->running != tort_false );
  return repl;
}

tort_v tort_runtime_initialize_repl()
{
  tort_mtable_create_class("repl", 0);
  return tort_nil;
}

