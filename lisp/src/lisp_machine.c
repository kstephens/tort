#include "tort/lisp.h"
#include "tort/repl.h"
#include <stdarg.h>

extern int _tort_lisp_trace;
extern int _tort_lisp_macro_trace;

typedef struct tort_lisp_machine { tort_H;
  tort_v exp; /* EXP is used to hold the expression or parts of the expression under evaluation. */
  tort_v env; /* ENV is used to hold the pointer to the environment structure which is the context of evaluation of the current expression. */
  tort_v val; /* VAL is used to hold the value developed in evaluation of expressions. It is set whenever a primitive operator is invoked, or when- ever a variable is evaluated, a quoted expression is evaluated, or a lambda expression is evaluated. */
  tort_v args; /* ARGS is used to hold the list of evaluated arguments (the "actual parameters") being accumulated for a combina- tion. */
  tort_v clink; /* CLINK is the pointer to the top of the list structure which is the control stack. (It is called "CLINK" for historical reasons stemming from CON- NIVER [21] and "spaghetti stacks" [4].) */
} tort_lisp_machine;

tort_SLOT(lisp_machine,tort_v,exp);
tort_SLOT(lisp_machine,tort_v,env);
tort_SLOT(lisp_machine,tort_v,val);
tort_SLOT(lisp_machine,tort_v,args);
tort_SLOT(lisp_machine,tort_v,clink);

tort_v _tort_m_lisp_machine__eval_dispatch(tort_tp tort_lisp_machine *lm)
{
#define EXP lm->exp
#define ENV lm->exp
#define VAL lm->val
#define ARGS lm->val
#define CLINK lm->clink
#define EXP_pair ((tort_cons*) EXP)
#define CAR_EXP EXP_pair->car
#define CDR_EXP EXP_pair->cdr
#define SYM(N) tort_v s_##N = tort_s(N)
  SYM(quote);
  SYM(lambda);
  SYM(if);
  SYM(ANDprocedure);
  SYM(ANDlast_arg);
  SYM(ANDevargs);
  SYM(ANDevargs2);
  SYM(ANDevif_decide);
  SYM(ANDapply_no_args);
#undef SYM
  tort_v mt_pair = tort_mt(pair);
  tort_v mt_symbol = tort_mt(symbol);
#define CONS tort_cons
#define CAR tort_car
#define CDR tort_cdr
#define CAAR tort_caar
#define CDAR tort_cdar
#define CADR tort_cadr
#define CADDR tort_caddr
#define CADDDR tort_cadddr
#define NIL tort_nil
#define EQ(X,Y) ((X) == (Y))
#define NULLQ(X) EQ(X, NIL)
#define PAIRQ(X) (tort_h_mtable(X) == mt_pair)
#define SYMBOLQ(X) (tort_h_mtable(X) == mt_symbol)
#define VALUE(sym, env) tort_send(tort_s(VALUE), lm, (env), (sym))
#define VALUE1(sym, env) tort_send(tort_s(VALUE1), lm, (env), (sym))
#define LOOKUP(sym, env) tort_send(tort_s(LOOKUP), lm, (env), (sym))
#define LOOKUP1(NAME, VARS, VALS, ENV) tort_send(tort_s(LOOKUP1), lm, (NAME), (VARS), (VALS), (ENV))
#define BIND(formals, args, env) tort_send(tort_s(bind), lm, env, formals, args)
#define PRIMOP_APPLY(op, args) tort_send(tort_s(sendv), op, args)
#define ERROR() tort_error(tort_ta "lisp_machine")
#define LENGTH(X) tort_send(tort_s(length), (X))
 EVAL_DISPATCH:
  if ( PAIRQ(EXP) ) {
    if ( CAR_EXP == s_quote ) {
      VAL = CADR(EXP);
      goto POPJ_RETURN;
    } else if ( CAR_EXP == s_lambda ) {
      VAL = CADR(EXP);
      EXP = CADDR(EXP);
      VAL = CONS(s_ANDprocedure, CONS(VAL, CONS(EXP, CONS(ENV, NIL))));
      goto POPJ_RETURN;
    } else if ( CAR_EXP == s_if ) {
      CLINK = CONS(ENV, CLINK);
      CLINK = CONS(EXP, CLINK);
      CLINK = CONS(s_ANDevif_decide, CLINK);
      EXP = CADR(EXP);
      goto EVAL_DISPATCH;
    } else if ( CDR_EXP == NIL ) {
      CLINK = CONS(s_ANDapply_no_args, CLINK);
      EXP = CAR_EXP;
      goto EVAL_DISPATCH;
      // BEGIN: HUH?
    } else if ( CAR_EXP == s_ANDevif_decide ) {
      goto EVIF_DECIDE;
    } else if ( CAR_EXP == s_ANDlast_arg ) {
      goto LAST_ARG;
    } else if ( CAR_EXP == s_ANDevargs ) {
      goto EVARGS;
    } else if ( CAR_EXP == s_ANDevargs2 ) {
      goto EVARGS2;
    } else if ( CAR_EXP == s_ANDapply_no_args ) {
      goto APPLY_NO_ARGS;
      // END: HUH?
    } else {
      CLINK = CONS(ENV, CLINK);
      CLINK = CONS(EXP, CLINK);
      CLINK = CONS(s_ANDevargs, CLINK);
      EXP = CAR_EXP;
      goto EVAL_DISPATCH;
    }
  } else if ( SYMBOLQ(EXP) ) {
    VAL = VALUE(EXP, ENV);
    goto POPJ_RETURN;
  } else {
    VAL = EXP;
    goto POPJ_RETURN;
  }
 POPJ_RETURN:
  EXP = CAR(CLINK);
  CLINK = CDR(CLINK);
  goto FUNCALL;
 FUNCALL:
  // FIXME:!!!
  goto EVAL_DISPATCH;
 EVIF_DECIDE:
  EXP = CAR(CLINK);
  CLINK = CDR(CLINK);
  ENV = CAR(CLINK);
  CLINK = CDR(CLINK);
  if ( VAL != tort_false )
    EXP = CADDR(EXP);
  else
    EXP = CADDDR(EXP);
  goto EVAL_DISPATCH;
 APPLY_NO_ARGS:
  ARGS = NIL;
  goto SAPPLY;
 EVARGS:
  EXP = CAR(CLINK); 
  CLINK = CDR(CLINK);
  ENV = CAR(CLINK);
  CLINK = CDR(CLINK);
  CLINK = CONS(VAL, CLINK);
  EXP = CDR_EXP;
  ARGS = NIL;
  goto EVARGS1;
 EVARGS1:
  if ( CDR_EXP == NIL ) {
    CLINK = CONS(ARGS, CLINK);
    CLINK = CONS(s_ANDlast_arg, CLINK);
    EXP = CAR_EXP;
    goto EVAL_DISPATCH;
  } else {
    CLINK = CONS(ENV, CLINK);
    CLINK = CONS(EXP, CLINK);
    CLINK = CONS(ARGS, CLINK);
    CLINK = CONS(s_ANDevargs, CLINK);
    EXP = CAR_EXP;
    goto EVAL_DISPATCH;
  }
 EVARGS2:
  ARGS = CAR(CLINK);
  CLINK = CDR(CLINK);
  EXP = CAR(CLINK);
  CLINK = CDR(CLINK);
  ENV = CAR(CLINK);
  CLINK = CDR(CLINK);
  ARGS = CONS(VAL, ARGS);
  EXP = CDR(EXP);
  goto EVARGS1;
 LAST_ARG:
  ARGS = CAR(CLINK);
  CLINK = CDR(CLINK);
  ARGS = CONS(VAL, ARGS);
  VAL = CAR(CLINK);
  CLINK = CDR(CLINK);
  goto SAPPLY;
 SAPPLY:
  if ( VAL == s_ANDprocedure ) {
    ENV = BIND(CADR(VAL), ARGS, CADDDR(VAL));
    EXP = CADDR(VAL);
    goto EVAL_DISPATCH;
  } else {
    VAL = PRIMOP_APPLY(VAL, ARGS);
  }
  return VAL;
}
#undef ARGS
#undef ENV
#define DEFINE(N,PARAMS...) \
  tort_v _tort_m_lisp_machine__##N(tort_tp tort_lisp_machine *lm, ##PARAMS)
DEFINE(BIND, tort_v VARS, tort_v ARGS, tort_v ENV)
{
  if ( EQ(LENGTH(VARS), LENGTH(ARGS)) ) 
    return CONS(CONS(VARS,ARGS), ENV);
  else 
    return ERROR();
}
DEFINE(VALUE, tort_v NAME, tort_v ENV)
{
  return VALUE1(NAME,LOOKUP(NAME,ENV));
}
DEFINE(VALUE1, tort_v NAME, tort_v SLOT)
{
  if ( EQ(SLOT, tort_s(ANDunbound)) )
    return ERROR();
  else
    return CAR(SLOT);
}
DEFINE(LOOKUP, tort_v NAME, tort_v ENV)
{
  if ( NULLQ(ENV) )
    return tort_s(ANDunbound);
  else
    return LOOKUP1(NAME, CAAR(ENV), CDAR(ENV), ENV);
}
DEFINE(LOOKUP1, tort_v NAME, tort_v VARS, tort_v VALS, tort_v ENV)
{
  if ( NULLQ(VARS) ) {
    return LOOKUP(NAME,CDR(ENV));
  } else { 
    if ( EQ(NAME, CAR(VARS)) ) 
      return VALS;
    else
      return LOOKUP1(NAME, CDR(VARS), CDR(VALS), ENV);
  }
}

tort_v _tort_m_initializer__lisp_machine(tort_tp tort_v init)
{
  tort_mtable_create_class("lisp_machine", 0);
  return init;
}
