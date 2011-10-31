#include "tort/lisp.h"
#include <stdarg.h>

int _tort_lisp_trace = 0;

typedef struct tort_lisp_formals { tort_H;
  tort_v formals;
  tort_v formals_n;
  tort_v argc; /* minimum number of arguments required. */
  tort_v map; /* Maps symbols to environment->argv[i]. */
  tort_v rest;
} tort_lisp_formals;

typedef struct tort_lisp_closure { tort_H;
  /* Same layout as tort_method. */
  tort_apply_decl((*applyf));
  tort_v name;
  tort_lisp_formals *formals;
  tort_v body;
  tort_v environment;
} tort_lisp_closure;

typedef struct tort_lisp_environment { tort_H;
  tort_lisp_formals *formals;
  tort_v argc;
  tort_v argv;
  tort_v rest;
  tort_v parent;
  tort_v globals;
} tort_lisp_environment;

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

tort_v _tort_m_lisp_formals__lisp_write(tort_tp tort_lisp_formals *rcvr, tort_v io)
{
  return tort_printf(io, "#f(%O . %O)", rcvr->formals, rcvr->rest);
}

tort_v _tort_M_lisp_closure___apply(tort_tp tort_v rcvr, ...)
{
  tort_lisp_closure *obj = (tort_v) _tort_message->method;
  int argc = tort_I(_tort_message->argc) >= 0 ? tort_I(_tort_message->argc) : tort_I(obj->formals->argc);
  tort_vector *argv = tort_vector_new(0, argc > 0 ? argc : 1);
  tort_lisp_environment *env;
  {
  va_list vap;
  int i = 0;
  va_start(vap, rcvr);
  tort_vector_data(argv)[i ++] = rcvr;
  while ( i < argc ) {
    tort_vector_data(argv)[i ++] = va_arg(vap, tort_v);
  }
  va_end(vap);
  }
  // tort_printf(tort_stderr, "\n  apply (%O argc %d expected-argc %d %O)\n", _tort_message->selector, (int) argc, (int) tort_I(obj->formals->argc), argv);
  env = tort_send(tort__s(new), tort_mt(lisp_environment), 
		  obj->formals, obj->environment, argv, tort_i(argc));
  return_tort_send(tort_s(lisp_eval_body), obj->body, env);
}

tort_v _tort_M_lisp_closure__new(tort_tp tort_mtable *mtable, tort_v formals, tort_v body, tort_v env)
{
  tort_lisp_closure *meth = tort_allocate(tort_mt(lisp_closure), sizeof(*meth));
  meth->applyf = _tort_M_lisp_closure___apply;
  formals = tort_send(tort__s(new), tort_mt(lisp_formals), formals);
  body = tort_send(tort_s(list_TO_vector), body);
  meth->name = tort_nil;
  meth->formals = formals;
  meth->body = body;
  meth->environment = env;
  return meth;
}

tort_v _tort_m_lisp_closure__lisp_write(tort_tp tort_lisp_closure *rcvr, tort_v io)
{
  return tort_printf(io, "(lambda %O ...)", rcvr->formals->formals);
}

tort_v _tort_m_lisp_closure__lisp_apply(tort_tp tort_lisp_closure *obj, tort_v args, tort_v env)
{
  env = tort_send(tort__s(new), tort_mt(lisp_environment), 
		  obj->formals, obj->environment, args, tort_nil);
  // tort_printf(tort_stderr, "\n  with env: %O %O\n", env, body);
  return_tort_send(tort_s(lisp_eval_body), obj->body, env);
}

tort_v _tort_m_lisp_environment__lisp_write(tort_tp tort_lisp_environment *rcvr, tort_v io)
{
  return tort_printf(io, "#e(%O . %O)", 
		     rcvr->formals->map, 
		     rcvr->parent);
}

tort_v _tort_M_lisp_environment__new(tort_tp tort_mtable *mtable, tort_lisp_formals *formals, tort_v parent_env, tort_v args, tort_v argc)
{
  tort_lisp_environment *env = tort_send(tort__s(_allocate), mtable, sizeof(*env));
  tort_v argv;

  if ( formals == tort_nil ) 
    formals = tort_send(tort__s(new), tort_mt(lisp_formals), tort_nil);
  env->formals = formals;

  if ( argc == tort_nil )
    argc = tort_send(tort_s(size), args);
  env->argc = argc;
  if ( argc < formals->argc )
    tort_error(tort_ta "not enough args");
  if ( formals->rest == tort_false && env->argc > formals->argc )
    tort_error(tort_ta "too many args");
  if ( tort_h_mtable(args) == tort_mt(vector) ) {
    env->argv = args;
    env->rest = tort_false; /* Lazy: see get below. */
  } else {
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
  }
  env->parent = parent_env;
  env->globals = tort_false;
  return env;
}

tort_v _tort_m_lisp_environment__globals(tort_tp tort_lisp_environment *env)
{
  if ( env->globals == tort_false )
    env->globals = env->parent == tort_nil ? env : tort_send(tort_s(globals), env->parent);
  return env->globals;
}

tort_v _tort_m_lisp_environment__get(tort_tp tort_lisp_environment *env, tort_v name)
{
  // tort_printf(tort_stderr, "  get(%O, %O)\n", env, name);
  if ( name == tort_s(ANDenv) ) {
    return env;
  }
  else if ( name == tort_s(ANDroot) ) {
    return tort_(root);
  }
  else if ( name == tort_s(ANDglobals) ) {
    return_tort_send(tort_s(globals), env);
  }
  else if ( env->formals->rest == name ) {
    if ( env->rest == tort_false ) {
      tort_v *tailp = &env->rest;
      int i;
      *tailp = tort_nil;
      for ( i = tort_I(env->formals->argc); i < tort_vector_size(env->argv); ++ i ) {
	*tailp = tort_cons(tort_vector_data(env->argv)[i], tort_nil);
	tailp = &((tort_cons*) *tailp)->cdr;
      }
    }
    return env->rest;
  } else {
    tort_v index = tort_send(tort__s(get), env->formals->map, name);
    if ( index != tort_nil ) {
      return_tort_send(tort__s(get), env->argv, index);
    } else {
      if ( env->parent == tort_nil ) {
	return tort_error(tort_ta "get: symbol %O is unbound", name);
      } else {
	return_tort_send(tort__s(get), env->parent, name);
      }
    }
  }
}

tort_v _tort_m_lisp_environment__set(tort_tp tort_lisp_environment *env, tort_v name, tort_v value)
{
  if ( name == tort_s(ANDtrace) ) {
    _tort_lisp_trace = tort_I(value);
  } else
  if ( env->formals->rest == name ) {
    env->rest = value;
  } else {
    tort_v index = tort_send(tort__s(get), env->formals->map, name);
    if ( index != tort_nil ) {
      tort_send(tort__s(set), env->argv, index, value);
    } else {
      if ( env->parent == tort_nil ) {
	tort_error(tort_ta "set!: symbol %O is unbound", name);
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
  tort_v globals = tort_send(tort_s(globals), env);
  return_tort_send(tort__s(add), globals, name, value);
}

tort_v _tort_m_symbol__lisp_eval(tort_tp tort_v sym, tort_v env)
{
  return_tort_send(tort__s(get), env, sym);
}

tort_v _tort_m_object__lisp_eval(tort_tp tort_v obj, tort_v env)
{
  return obj;
}

tort_v _tort_m_cons__lisp_eval(tort_tp tort_cons *obj, tort_v env)
{
  tort_v val = obj->car;
  extern tort_v _tort_debug_expr;
  if ( _tort_lisp_trace ) tort_printf(tort_stderr, "\n  lisp_eval %O\n", obj);
  if ( val == tort_s(quote) ) {
    return tort_car(obj->cdr);
  }
  _tort_debug_expr = obj;
  if ( val == tort_s(define) ) {
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
    return_tort_send(tort_s(lisp_eval), val, env);
  }
  else if ( val == tort_s(cond) ) {
    val = obj;
    while ( (obj = obj->cdr) != tort_nil ) {
      val = obj->car;
      if ( obj->cdr == tort_nil || 
	   tort_send(tort_s(lisp_eval), tort_car(val), env) != tort_false )
	break;
    }
    return_tort_send(tort_s(lisp_eval_body), tort_cdr(val), env);
  }
  else if ( val == tort_s(begin) ) {
    return_tort_send(tort_s(lisp_eval_body), obj->cdr, env);
  }
  else if ( val == tort_s(or) ) {
    val = tort_false;
    while ( (obj = obj->cdr) != tort_nil ) {
      val = tort_send(tort_s(lisp_eval), obj->car, env);
      if ( val != tort_false ) break;
    }
    return val;
  }
  else if ( val == tort_s(and) ) {
    val = tort_true;
    while ( (obj = obj->cdr) != tort_nil ) {
      val = tort_send(tort_s(lisp_eval), obj->car, env);
      if ( val == tort_false ) break;
    }
    return val;
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
    return_tort_send(tort__s(new), tort_mt(lisp_closure), formals, body, env);
  }
  else if ( val == tort_s(let) ) {
    val = tort_car(obj->cdr); /* bindings */
    if ( val != tort_nil )
      env = tort_send(tort__s(new), tort_mt(lisp_environment), 
		      tort_send(tort__s(new), tort_mt(lisp_formals), 
				tort_send(tort_s(lisp_eval_let_names), val, env) /* bindings => names => */ ), /* formals */
		      env, /* parent environment. */
		      tort_send(tort_s(lisp_eval_let_values), val, env), /* bindings => values => args */
		      tort_nil);
    return_tort_send(tort_s(lisp_eval_body), tort_cdr(obj->cdr), env);
  }
  else {
    tort_v args;
    val  = tort_send(tort_s(lisp_eval_car), obj->car, env);
    args = tort_send(tort_s(lisp_eval_args), obj->cdr, env);
    // if ( _tort_lisp_trace || 1) tort_printf(tort_stderr, "\n  lisp_eval %O\n", obj);
    // if ( _tort_lisp_trace || 1) tort_printf(tort_stderr, "\n  f = %O, args = %O\n", val, args);
    return_tort_send(tort_s(lisp_apply), val, args, env);
  }
}

tort_v _tort_m_symbol__lisp_eval_car(tort_tp tort_v obj, tort_v env)
{
  if ( _tort_lisp_trace ) tort_printf(tort_stderr, "\n  symbol::lisp_eval_car: %O\n", obj);
  return_tort_send(tort_s(get), env, obj);
}

tort_v _tort_m_object__lisp_eval_car(tort_tp tort_v obj, tort_v env)
{
  if ( _tort_lisp_trace ) tort_printf(tort_stderr, "\n  object::lisp_eval_car: %O\n", obj);
  return_tort_send(tort_s(lisp_eval), obj, env);
}

tort_v _tort_m_cons__lisp_eval_let_names(tort_tp tort_cons *obj, tort_v env)
{
  return tort_cons(tort_car(obj->car), 
		   tort_send(tort_s(lisp_eval_let_names), obj->cdr, env));
}

tort_v _tort_m_object__lisp_eval_let_names(tort_tp tort_v obj, tort_v env)
{
  return obj;
}

tort_v _tort_m_cons__lisp_eval_let_values(tort_tp tort_cons *obj, tort_v env)
{
  return tort_cons(tort_send(tort_s(lisp_eval), tort_cadr(obj->car), env), 
		   tort_send(tort_s(lisp_eval_let_values), obj->cdr, env));
}

tort_v _tort_m_object__lisp_eval_let_values(tort_tp tort_v obj, tort_v env)
{
  return obj;
}

tort_v _tort_m_symbol__lisp_apply(tort_tp tort_v obj, tort_v args, tort_v env)
{
  tort_vector *argv = tort_send(tort_s(list_TO_vector), args);
  if ( _tort_lisp_trace ) tort_printf(tort_stderr, "\n  symbol::lisp_apply: %O %O\n", obj, argv);
  return_tort_send(tort_s(_sendv), obj, tort_vector_data(argv), tort_vector_size(argv));
}

tort_v _tort_m_object__lisp_apply(tort_tp tort_cons *obj, tort_v args, tort_v env)
{
  if ( _tort_lisp_trace ) tort_printf(tort_stderr, "\n  object::lisp_apply: %O %O\n", obj, args);
  return_tort_send(tort_s(__debugger), obj);
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

tort_v _tort_m_object__lisp_eval_args(tort_tp tort_v obj, tort_v env)
{
  return_tort_send(tort_s(lisp_eval), obj, env);
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
  if ( obj == tort_nil ) return obj;
  while ( obj->cdr != tort_nil ) {
    tort_send(tort_s(lisp_eval), obj->car, env);
    obj = obj->cdr;
  }
  return_tort_send(tort_s(lisp_eval), obj->car, env);
}

tort_v _tort_m_io__lisp_repl(tort_tp tort_v io, tort_v out, tort_v prompt, tort_v env)
{
  tort_v expr, result = tort_nil;
  if ( env == tort_nil ) {
    env = tort_send(tort__s(new), tort_mt(lisp_environment), 
		    tort_nil, tort_nil, tort_nil, tort_nil);
  }
  do {
    if ( prompt != tort_nil ) tort_printf(prompt, " > ");
    expr = tort_send(tort_s(lisp_read), io);
    if ( expr == tort_eos ) break;
    if ( prompt != tort_nil ) {
      tort_printf(prompt, "  == ");
      tort_send(tort_s(lisp_write), expr, out);
      tort_printf(prompt, "\n");
    }
    result = tort_send(tort_s(lisp_eval), expr, env);
    if ( prompt != tort_nil ) tort_printf(prompt, "  => ");
    if ( out != tort_nil ) {
      tort_send(tort_s(lisp_write), result, out);
      tort_printf(out, "\n");
    }
  } while ( 1 );
  return env;
}

tort_v tort_runtime_initialize_lisp_eval()
{
  tort_mtable_make("lisp_formals", 0);
  tort_mtable_make("lisp_closure", 0);
  tort_mtable_make("lisp_environment", 0);
  return 0;
}

