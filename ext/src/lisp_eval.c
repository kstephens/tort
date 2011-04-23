#include "tort/lisp.h"

int _tort_lisp_trace = 0;

typedef struct tort_lisp_formals { tort_H;
  tort_v formals;
  tort_v formals_n;
  tort_v argc;
  tort_v map;
  tort_v rest;
} tort_lisp_formals;

tort_v _tort_M_lisp_formals__new(tort_tp tort_mtable *mtable, tort_v formals)
{
  tort_lisp_formals *obj = tort_send(tort__s(_allocate), mtable, sizeof(*obj));
  obj->formals = formals;
  obj->map = tort_map_create();
  obj->rest = tort_false;
  obj->argc = obj->formals_n = tort_i(0);
  {
    tort_v *f = formals;
    int i = 0;
    while ( f != tort_nil ) {
      if ( tort_h_mtable(f) == tort_mt(symbol) ) {
	obj->rest = f;
	obj->argc = tort_i(i);
	++ i;
	obj->formals_n = tort_i(i);
	break;
      } else {
	tort_v name = tort_car(f);
	f = tort_cdr(f);
	tort_send(tort__s(set), obj->map, name, tort_i(i));
	++ i;
	obj->argc = obj->formals_n = tort_i(i);
      }
    }
  }
  return obj;
}

tort_v _tort_m_lisp_formals__lisp_write(tort_thread_param tort_lisp_formals *rcvr, tort_v io)
{
  return tort_printf(io, "#f(%O %O)", rcvr->formals, rcvr->rest);
}

typedef struct tort_lisp_environment { tort_H;
  tort_lisp_formals *formals;
  tort_v argv;
  tort_v rest;
  tort_v parent;
  tort_v root;
} tort_lisp_environment;

tort_v _tort_m_lisp_environment__lisp_write(tort_thread_param tort_lisp_environment *rcvr, tort_v io)
{
  return tort_printf(io, "#e(%O . %O)", 
		     rcvr->formals->map, 
		     rcvr->parent);
}

tort_v _tort_M_lisp_environment__new(tort_tp tort_mtable *mtable, tort_lisp_formals *formals, tort_v parent_env, tort_v args)
{
  tort_lisp_environment *env = tort_send(tort__s(_allocate), mtable, sizeof(*env));
  tort_v argc;
  tort_v argv;

  if ( formals == tort_nil ) 
    formals = tort_send(tort__s(new), tort_mt(lisp_formals), tort_nil);
  env->formals = formals;

  argc = tort_send(tort_s(size), args);
  if ( argc < formals->argc )
    tort_error("not enough args");
  if ( formals->rest == tort_false && argc > formals->argc )
    tort_error("too many args");
  argv = tort_vector_new(0, tort_I(formals->argc));
  env->argv = argv;
  {
    int argi = 0;
    while ( args != tort_nil ) {
      if ( argi >= tort_I(formals->argc) )
	break;
      tort_vector_data(argv)[argi] = tort_car(args);
      args = tort_cdr(args);
      argi ++;
    }
    env->rest = args;
  }
  env->parent = parent_env;
  env->root = tort_false;
  return env;
}

tort_v _tort_m_lisp_environment__root(tort_tp tort_lisp_environment *env)
{
  if ( env->root == tort_false )
    env->root = env->parent == tort_nil ? env : tort_send(tort_s(root), env->parent);
  return env->root;
}

tort_v _tort_m_lisp_environment__get(tort_tp tort_lisp_environment *env, tort_v name)
{
  // tort_printf(tort_stderr, "  get(%O, %O)\n", env, name);
  if ( name == tort_s(ANDenv) ) {
    return env;
  }
  else if ( name == tort_s(ANDroot) ) {
    return tort_send(tort_s(root), env);
  }
  else if ( env->formals->rest == name ) {
    return env->rest;
  } else {
    tort_v index = tort_send(tort__s(get), env->formals->map, name);
    if ( index != tort_nil ) {
      return tort_send(tort__s(get), env->argv, index);
    } else {
      if ( env->parent == tort_nil ) {
	return tort_error("get: cannot find %O\n", name);
      } else {
	return tort_send(tort__s(get), env->parent, name);
      }
    }
  }
}

tort_v _tort_m_lisp_environment__set(tort_tp tort_lisp_environment *env, tort_v name, tort_v value)
{
  if ( env->formals->rest == name ) {
    env->rest = value;
  } else {
    tort_v index = tort_send(tort__s(get), env->formals->map, name);
    if ( index != tort_nil ) {
      tort_send(tort__s(set), env->argv, index, value);
    } else {
      if ( env->parent == tort_nil ) {
	tort_error("error");
      } else {
	tort_send(tort__s(set), env->parent, name, value);
      }
    }
  }
  return env;
}

tort_v _tort_m_lisp_environment__add(tort_tp tort_lisp_environment *env, tort_v name, tort_v value)
{
  if ( env->formals->rest == name ) {
    env->rest = value;
  } else {
    tort_v index = tort_send(tort__s(get), env->formals->map, name);
    if ( index != tort_nil ) {
      tort_send(tort__s(set), env->argv, index, value);
    } else {
      index = tort_send(tort__s(size), env->argv);
      tort_send(tort__s(set), env->formals->map, name, index);
      tort_send(tort__s(add), env->argv, value);
    }
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
  if ( _tort_lisp_trace ) tort_printf(tort_stderr, "\n  lisp_eval %O\n", obj);
  if ( val == tort_s(quote) ) {
    return tort_car(obj->cdr);
  }
  else if ( val == tort_s(define) ) {
    tort_v name = tort_car(obj->cdr);
    val = tort_car(tort_cdr(obj->cdr));
    val = tort_send(tort_s(lisp_eval), val, env);
    tort_send(tort_s(define), env, name, val);
    return name;
  }
  else if ( val == tort_s(setE) ) {
    tort_v name = tort_car(obj->cdr);
    val = tort_car(tort_cdr(obj->cdr));
    val = tort_send(tort_s(lisp_eval), val, env);
    tort_send(tort_s(set), env, name, val);
    return name;
  }
  else if ( val == tort_s(if) ) {
    val = tort_send(tort_s(lisp_eval), tort_car(obj->cdr), env);
    if ( val == tort_false ) {
      val = tort_cddr(obj->cdr);
      val = val != tort_nil ? tort_car(val) : tort_nil;
    } else {
      val = tort_cadr(obj->cdr);
    }
    return tort_send(tort_s(lisp_eval), val, env);
  }
  else if ( val == tort_s(begin) ) {
    return tort_send(tort_s(lisp_eval_body), obj->cdr, env);
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
    formals = tort_send(tort__s(new), tort_mt(lisp_formals), formals);
    body = tort_send(tort_s(list_TO_vector), body);
    return tort_cons(tort_s(ANDlambda), 
		     tort_cons(formals,
			       tort_cons(body, env)));
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
  if ( _tort_lisp_trace ) tort_printf(tort_stderr, "\n  symbol::lisp_eval_car: %O\n", obj);
  return tort_send(tort_s(get), env, obj);
}

tort_v _tort_m_object__lisp_eval_car(tort_tp tort_v obj, tort_v env)
{
  if ( _tort_lisp_trace ) tort_printf(tort_stderr, "\n  object::lisp_eval_car: %O\n", obj);
  return tort_send(tort_s(lisp_eval), obj, env);
}

tort_v _tort_m_symbol__lisp_apply(tort_tp tort_v obj, tort_v args, tort_v env)
{
  tort_vector *argv = tort_send(tort_s(list_TO_vector), args);
  if ( _tort_lisp_trace ) tort_printf(tort_stderr, "\n  symbol::lisp_apply: %O %O\n", obj, argv);
  return tort_send(tort_s(_sendv), obj, tort_vector_data(argv), tort_vector_size(argv));
}

tort_v _tort_m_cons__lisp_apply(tort_tp tort_cons *obj, tort_v args, tort_v env)
{
  tort_v val = obj->car;
  if ( _tort_lisp_trace ) tort_printf(tort_stderr, "\n  cons::lisp_apply: %O %O\n", obj, args);
  if ( val == tort_s(ANDlambda) ) {
    tort_v formals = tort_car(obj->cdr);
    tort_v body    = tort_cadr(obj->cdr);
    tort_v closure = tort_cddr(obj->cdr);
    env = tort_send(tort__s(new), tort_mt(lisp_environment), formals, closure, args);
    // tort_printf(tort_stderr, "\n  with env: %O %O\n", env, body);
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

tort_v _tort_m_vector__lisp_eval_body(tort_tp tort_vector *obj, tort_v env)
{
  tort_v val = tort_nil;
  tort_vector_loop(obj, expr); {
    val = tort_send(tort_s(lisp_eval), expr, env);
  } tort_vector_loop_end(obj);
  return val;
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
  tort_v env = tort_nil; // tort_send(tort_s(new), tort_mt(dmap), globals, tort_nil);
  // tort_v args = tort_nil;
  env = tort_send(tort__s(new), tort_mt(lisp_environment), tort_nil, tort_nil, tort_nil);
  do {
    tort_printf(out, " >  ");
    expr = tort_send(tort_s(lisp_read), io);
    if ( expr == tort_eos ) break;
    tort_printf(out, " == ");
    tort_send(tort_s(lisp_write), expr, out);
    tort_printf(out, "\n");
    result = tort_send(tort_s(lisp_eval), expr, env);
    tort_printf(out, " => ");
    tort_send(tort_s(lisp_write), result, out);
    tort_printf(out, "\n");
  } while ( 1 );
  return result;
}


tort_v tort_runtime_initialize_lisp_eval()
{
  tort_mtable_make("lisp_formals", 0);
  tort_mtable_make("lisp_environment", 0);

  return 0;
}

