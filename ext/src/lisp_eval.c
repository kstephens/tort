#include "tort/lisp.h"

typedef struct tort_lisp_environment { tort_H;
  tort_v formals;
  tort_v formals_map;
  tort_v args;
  tort_v parent;
  tort_v rest;
} tort_lisp_environment;

tort_v _tort_M_lisp_environment__new(tort_tp tort_mtable *mtable, tort_v formals, tort_v parent_env, tort_v args)
{
  tort_lisp_environment *env = tort_send(tort__s(_allocate), mtable, sizeof(*env));
  if ( args == tort_nil )
    args = tort_vector_new(0, 0);
  env->formals = formals;
  env->formals_map = tort_map_create();
  {
    tort_v *f = formals;
    int i = 0;
    while ( f != tort_nil ) {
      tort_v name = ((tort_cons*) f)->car;
      tort_send(tort__s(set), env->formals_map, name, i);
      ++ i;
    }
  }
  env->args = args;
  env->parent = env;
  env->rest = tort_true;
  return env;
}

tort_v _tort_m_lisp_environment__root(tort_tp tort_lisp_environment *env)
{
  return env->parent == tort_nil ? env : tort_send(tort_s(root), env->parent) ;
}

tort_v _tort_m_lisp_environment__get(tort_tp tort_lisp_environment *env, tort_v name)
{
  tort_v index = tort_send(tort__s(get), env->formals_map, name);
  if ( index != tort_nil ) {
    return tort_send(tort__s(get), env->args, index);
  } else {
    if ( env->parent == tort_nil ) {
      return tort_error("error");
    } else {
      return tort_send(tort__s(get), env->parent, name);
    }
  }
}

tort_v _tort_m_lisp_environment__set(tort_tp tort_lisp_environment *env, tort_v name, tort_v value)
{
  tort_v index = tort_send(tort__s(get), env->formals_map, name);
  if ( index != tort_nil ) {
    return tort_send(tort__s(set), env->args, index, value);
  } else {
    if ( env->parent == tort_nil ) {
      return tort_error("error");
    } else {
      return tort_send(tort__s(set), env->parent, name, value);
    }
  }
}

tort_v _tort_m_lisp_environment__add(tort_tp tort_lisp_environment *env, tort_v name, tort_v value)
{
  tort_v index = tort_send(tort__s(get), env->formals_map, name);
  if ( index != tort_nil ) {
    tort_send(tort__s(set), env->args, index, value);
  } else {
    index = tort_send(tort__s(size), env->args);
    tort_send(tort__s(set), env->formals_map, name, index);
    tort_send(tort__s(append), env->args, value);
  }
  return env;
}

tort_v _tort_m_lisp_environment__define(tort_tp tort_lisp_environment *env, tort_v name, tort_v value)
{
  tort_v root = tort_send(tort_s(root), env);
  return tort_send(tort__s(add), root, name, value);
}


tort_v _tort_m_symbol__lisp_eval(tort_tp tort_v sym, tort_v env)
{
  return tort_send(tort__s(get), env, sym);
}

tort_v _tort_m_object__lisp_eval(tort_tp tort_v obj, tort_v env)
{
  return obj;
}

tort_v _tort_m_cons__lisp_eval(tort_tp tort_cons *obj, tort_v env)
{
  tort_v val = obj->car;
  if ( val == tort_s(quote) ) {
    return tort_car(obj->cdr);
  }
  else if ( val == tort_s(if) ) {
    val = tort_send(tort_s(lisp_eval), tort_car(obj->cdr), env);
    if ( val == tort_false ) {
      val = tort_caddr(obj->cdr);
    } else {
      val = tort_cadr(obj->cdr);
    }
    return tort_send(tort_s(lisp_eval), val, env);
  }
  else if ( val == tort_s(while) ) {
    val = tort_nil;
    while ( tort_send(tort_s(lisp_eval), tort_car(obj->cdr), env) != tort_false ) {
      val = tort_send(tort_s(lisp_eval_body), tort_cdr(obj->cdr), env);
    }
    return val;
  }
  else if ( val == tort_s(lambda) ) {
    tort_v formals = tort_car(obj->cdr);
    tort_v body = tort_cdr(obj->cdr);
    return tort_cons(tort_s(ANDlambda), 
		     tort_cons(formals,
			       tort_cons(body, 
					 tort_cons(env, tort_nil))));
  }
  else {
    tort_v args;
    val  = tort_send(tort_s(lisp_eval_car), obj->car, env);
    args = tort_send(tort_s(lisp_eval_args), obj->cdr, env);
    return tort_send(tort_s(lisp_apply), val, args, env);
  }
}

tort_v _tort_m_symbol__lisp_eval_car(tort_tp tort_v obj, tort_v env)
{
  return obj;
}

tort_v _tort_m_object__lisp_eval_car(tort_tp tort_v obj, tort_v env)
{
  return tort_send(tort_s(lisp_eval), obj, env);
}

tort_v _tort_m_symbol__lisp_apply(tort_tp tort_v obj, tort_v args, tort_v env)
{
  return tort_send(tort_s(_sendv), obj, tort_send(tort_s(listTOvector), args));
}

tort_v _tort_m_cons__lisp_apply(tort_tp tort_cons *obj, tort_v args, tort_v env)
{
  tort_v val = obj->car;
  if ( val == tort_s(ANDlambda) ) {
    tort_v formals = tort_car(obj->cdr);
    tort_v body    = tort_cadr(obj->cdr);
    tort_v closure = tort_caddr(obj->cdr);
    env = tort_send(tort__s(new), tort_mt(lisp_environment), formals, closure, args);
    return tort_send(tort_s(lisp_eval_body), body, env);
  } else {
    return tort_send(tort_s(__debugger), obj);
  }
}

tort_v _tort_m_cons__lisp_eval_args(tort_tp tort_cons *obj, tort_v env)
{
  return tort_cons(tort_send(tort_s(lisp_eval), obj->car, env),
		   tort_send(tort_s(lisp_eval_args), obj->cdr, env));
    
}

tort_v _tort_m_nil__lisp_eval_args(tort_tp tort_cons *obj, tort_v env)
{
  return obj;
}

tort_v _tort_m_object__lisp_eval_body(tort_tp tort_cons *obj, tort_v env)
{
  tort_v val = tort_nil;
  while ( obj != tort_nil ) {
    val = tort_send(tort_s(lisp_eval), obj->car, env);
    obj = obj->cdr;
  }
  return val;
}

tort_v _tort_m_io__lisp_repl(tort_tp tort_v io, tort_v out, tort_v globals)
{
  tort_v expr, result = tort_nil;
  tort_v env = tort_send(tort_s(new), tort_mt(dmap), globals, tort_nil);
  do {
    tort_printf(out, " >  ");
    expr = tort_send(tort_s(lisp_read), io);
    if ( expr == tort_eos ) break;
    tort_printf(out, " == ");
    tort_send(tort_s(lisp_write), expr, out);
    tort_printf(out, "\n");
    result  = tort_send(tort_s(lisp_eval), expr, env);
    tort_printf(out, " => ");
    tort_send(tort_s(lisp_write), result, out);
    tort_printf(out, "\n");
  } while ( 1 );
  return result;
}


tort_v tort_runtime_initialize_lisp_eval()
{
  tort_mtable_make("lisp_environment", 0);

  return 0;
}

