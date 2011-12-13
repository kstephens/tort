#include "tort/lisp.h"
#include "tort/repl.h"
#include <stdarg.h>

int _tort_lisp_trace = 0;
int _tort_lisp_macro_trace = 0;

#define NEW_LOC(VALUE) tort_send(tort__s(new_value), tort__mt(locative), (VALUE))

typedef struct tort_lisp_formals { tort_H;
  tort_v formals;
  tort_v formals_n;
  tort_v argc; /* minimum number of arguments required. */
  tort_v map; /* Maps symbols to environment->argv[i]. */
  tort_v rest;
} tort_lisp_formals;

tort_ACCESSOR(lisp_formals,tort_v,formals);
tort_ACCESSOR(lisp_formals,tort_v,formals_n);
tort_ACCESSOR(lisp_formals,tort_v,argc);
tort_ACCESSOR(lisp_formals,tort_v,map);
tort_ACCESSOR(lisp_formals,tort_v,rest);

typedef struct tort_lisp_closure { tort_H; /* Same layout as tort_method. */
  tort_v name;
  tort_lisp_formals *formals;
  tort_v body;
  tort_v environment;
} tort_lisp_closure;

tort_ACCESSOR(lisp_closure,tort_v,name);
tort_ACCESSOR(lisp_closure,tort_v,formals);
tort_ACCESSOR(lisp_closure,tort_v,body);
tort_ACCESSOR(lisp_closure,tort_v,environment);

typedef struct tort_lisp_environment { tort_H;
  tort_lisp_formals *formals;
  tort_v argc;
  tort_v argv;
  tort_v rest;
  tort_v rest_ok;
  tort_v parent;
  tort_v _globals;
  tort_v msg;
} tort_lisp_environment;

tort_ACCESSOR(lisp_environment,tort_v,formals);
tort_ACCESSOR(lisp_environment,tort_v,argc);
tort_ACCESSOR(lisp_environment,tort_v,argv);
tort_ACCESSOR(lisp_environment,tort_v,rest);
tort_ACCESSOR(lisp_environment,tort_v,rest_ok);
tort_ACCESSOR(lisp_environment,tort_v,parent);
tort_ACCESSOR(lisp_environment,tort_v,_globals);
tort_ACCESSOR(lisp_environment,tort_v,msg);

tort_v _tort_M_lisp_formals__new(tort_tp tort_mtable *mtable, tort_v formals)
{
  tort_lisp_formals *obj = tort_send(tort__s(_allocate), mtable, tort_i(sizeof(*obj)));
  obj->formals = formals;
  obj->map = tort_map_new();
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

tort_v _tort_M_lisp_closure___applyv(tort_tp tort_lisp_closure *obj, int argc, va_list *vap)
{
  tort_vector *argv;
  argc += tort_I(_tort_message->argc) >= 0 ? tort_I(_tort_message->argc) : tort_I(obj->formals->argc);
  argv = tort_vector_new(0, argc > 0 ? argc : 1);
  tort_lisp_environment *env;
  {
    int i = 0;
    while ( i < argc )
      tort_vector_data(argv)[i ++] = va_arg(*vap, tort_v);
    va_end(*vap);
  }
  // tort_printf(tort_stderr, "\n  apply (%O argc %d expected-argc %d %O)\n", _tort_message->selector, (int) argc, (int) tort_I(obj->formals->argc), argv);
  env = tort_send(tort__s(new), tort_mt(lisp_environment), 
		  obj->formals, obj->environment, argv, tort_i(argc));
  env->msg = _tort_message;
  return_tort_send(tort_s(lisp_eval_body), obj->body, env);
}

/* method interface. */
tort_v _tort_M_lisp_closure___apply(tort_tp ...)
{
  va_list vap;
  va_start(vap, _tort_message);
  return _tort_M_lisp_closure___applyv(tort_ta (void*) _tort_message->method, 0, &vap);
}

/* block interface. */
tort_v _tort_m_lisp_closure__value(tort_tp tort_lisp_closure *obj, ...)
{
  va_list vap;
  va_start(vap, obj);
  return _tort_M_lisp_closure___applyv(tort_ta obj, -1, &vap);
}

/* non-symbol lookup interface. */
tort_v _tort_m_lisp_closure__lookup(tort_tp tort_lisp_closure *obj, tort_message *msg)
{
  msg->mtable = tort_h_mtable(obj);
  msg->method = (tort_v) obj;
  return msg;
}

tort_v _tort_M_lisp_closure__new(tort_tp tort_mtable *mtable, tort_v formals, tort_v body, tort_v env)
{
  tort_lisp_closure *meth = tort_send(tort__s(_allocate), tort_mt(lisp_closure), tort_i(sizeof(*meth)));
  meth->_h[-1].applyf = (void*) _tort_M_lisp_closure___apply;
  formals = tort_send(tort__s(new), tort_mt(lisp_formals), formals);
  body = tort_send(tort_s(list_TO_vector), body);
  if ( tort_vector_size(body) == 0 ) {
    tort_v x = tort_nil;
    body = tort_vector_new(&x, 1);
  }
  meth->name = tort_nil;
  meth->formals = formals;
  meth->body = body;
  meth->environment = env;
  return meth;
}

tort_v _tort_m_lisp_closure__lisp_write(tort_tp tort_lisp_closure *rcvr, tort_v io)
{
  return tort_printf(io, "#<lambda %O >", rcvr->formals->formals);
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
  return tort_printf(io, "#&env(%O . %O)", 
		     rcvr->formals->map, 
		     rcvr->parent);
}

tort_v _tort_M_lisp_environment__new(tort_tp tort_mtable *mtable, tort_lisp_formals *formals, tort_v parent_env, tort_v args, tort_v argc)
{
  tort_lisp_environment *env = tort_send(tort__s(_allocate), mtable, tort_i(sizeof(*env)));
  tort_v argv;

  if ( formals == tort_nil ) 
    formals = tort_send(tort__s(new), tort_mt(lisp_formals), tort_nil);
  env->formals = formals;

  if ( argc == tort_nil )
    argc = tort_send(tort_s(size), args);
  env->argc = argc;
  if ( argc < formals->argc )
    tort_error(tort_ta "not enough args: expected %O, given %O to %O", formals->argc, argc, formals);
  if ( formals->rest == tort_false && env->argc > formals->argc )
    tort_error(tort_ta "too many args: expected %O, given %O to %O", formals->argc, argc, formals);
  if ( tort_h_mtable(args) == tort_mt(vector) ) {
    env->argv = argv = args;
    env->rest = env->rest_ok = tort_nil; /* Lazy: see get below. */
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
      env->rest = NEW_LOC(args);
      env->rest_ok = tort_true;
    }
  }
  // Turn values into locatives.
  {
    int argi = tort_vector_size(argv);
    while ( -- argi >= 0 ) {
      tort_v *vp = &tort_vector_data(argv)[argi];
      *vp = NEW_LOC(*vp);
    }
  }
  env->parent = parent_env;
  if ( parent_env != tort_nil )
    env->msg = ((tort_lisp_environment *)parent_env)->msg;
  env->_globals = tort_false;
  return env;
}

tort_v _tort_m_lisp_environment__globals(tort_tp tort_lisp_environment *env)
{
  if ( env->_globals == tort_false )
    env->_globals = env->parent == tort_nil ? env : tort_send(tort_s(globals), env->parent);
  return env->_globals;
}

tort_v _tort_m_lisp_environment__globals_locative(tort_tp tort_lisp_environment *env)
{
  tort_send(tort_s(globals), env);
  return tort_l(&env->_globals);
}

tort_v _tort_m_lisp_environment__get(tort_tp tort_lisp_environment *env, tort_v name)
{
  tort_v index = tort_send(tort__s(get), env->formals->map, name);
  if ( index != tort_nil ) {
    return_tort_send(tort__s(get), env->argv, index);
  }
  else if ( env->formals->rest == name ) {
    if ( env->rest_ok == tort_nil ) {
      tort_v *tailp = &env->rest;
      int i;
      *tailp = tort_nil;
      for ( i = tort_I(env->formals->argc); i < tort_vector_size(env->argv); ++ i ) {
	tort_v v = tort_vector_data(env->argv)[i];
	*tailp = tort_cons(*tort_L(v), tort_nil);
	tailp = &((tort_cons*) *tailp)->cdr;
      }
      env->rest = NEW_LOC(env->rest);
      env->rest_ok = tort_true;
    }
    return env->rest;
  } 
  else if ( name == tort_s(ANDenv) ) {
    return NEW_LOC(env);
  }
  else if ( name == tort_s(ANDroot) ) {
    return tort_l(&tort_(root));
  }
  else if ( name == tort_s(ANDmsg) ) {
    return tort_l(&env->msg);
  }
  else if ( name == tort_s(ANDglobals) ) {
    return_tort_send(tort_s(globals_locative), env);
  }
  else {
    if ( env->parent == tort_nil ) {
      return tort_nil;
    } else {
      return_tort_send(tort__s(get), env->parent, name);
    }
  }
}

tort_v _tort_m_lisp_environment__set(tort_tp tort_lisp_environment *env, tort_v name, tort_v value)
{
  tort_v index = tort_send(tort__s(get), env->formals->map, name);
  if ( index != tort_nil ) {
    index = tort_send(tort__s(get), env->argv, index);
    *tort_L(index) = value; 
  }
  else if ( env->formals->rest == name ) {
    if ( env->rest_ok == tort_nil )
      env->rest = NEW_LOC(value);
    else 
      *tort_L(env->rest) = value;
    env->rest_ok = tort_true;
  }
  else if ( name == tort_s(ANDtrace) ) {
    _tort_lisp_trace = tort_I(value);
  }
  else if ( name == tort_s(ANDmacroSUBtrace) ) {
    _tort_lisp_macro_trace = tort_I(value);
  } else {
    if ( env->parent == tort_nil ) {
      tort_error(tort_ta "set!: symbol '%O is unbound.", name);
    } else {
      tort_send(tort__s(set), env->parent, name, value);
    }
  }
  return env;
}

tort_v _tort_m_lisp_environment__add_locative(tort_tp tort_lisp_environment *env, tort_v name, tort_v value)
{
  tort_v index = tort_send(tort__s(get), env->formals->map, name);
  if ( index != tort_nil ) {
    tort_send(tort__s(set), env->argv, index, value);
  }
  else if ( env->formals->rest == name ) {
    env->rest = value;
    env->rest_ok = tort_true;
  } else {
    index = tort_send(tort__s(size), env->argv);
    tort_send(tort__s(set), env->formals->map, name, index);
    tort_send(tort__s(add), env->argv, value);
  }
  return env;
}

tort_v _tort_m_lisp_environment__add(tort_tp tort_lisp_environment *env, tort_v name, tort_v value)
{
  return_tort_send(tort_s(add_locative), env, name, NEW_LOC(value));
}

tort_v _tort_m_lisp_environment__define(tort_tp tort_lisp_environment *env, tort_v name, tort_v value)
{
  tort_v globals = tort_send(tort_s(globals), env);
  return_tort_send(tort__s(add), globals, name, value);
}

tort_v _tort_m_object__lisp_eval_top_level(tort_tp tort_v expr, tort_v env)
{
  if ( _tort_lisp_macro_trace ) tort_printf(tort_stderr, "  ME    %T\n", expr);
  expr = tort_send(tort_s(macro_expand), env, expr);
  if ( _tort_lisp_macro_trace ) tort_printf(tort_stderr, "  ME => %T\n", expr);
  return_tort_send(tort_s(lisp_eval), expr, env);
}

tort_v _tort_m_lisp_environment__macro_expand(tort_tp tort_v env, tort_v expr)
{
  return expr;
}

tort_v _tort_m_symbol__lisp_eval(tort_tp tort_v sym, tort_v env)
{
  tort_v l = tort_send(tort__s(get), env, sym);
  if ( l == tort_nil ) return tort_error(tort_ta "get: symbol '%O is unbound.", sym);
  return *tort_L(l);
}

tort_v _tort_m_object__lisp_eval(tort_tp tort_v obj, tort_v env)
{
  return obj;
}

tort_v _tort_m_cons__lisp_eval(tort_tp tort_cons *obj, tort_v env)
{
  tort_v val;
  extern tort_v _tort_debug_expr;
  val = obj->car;
  if ( _tort_lisp_trace ) tort_printf(tort_stderr, "  E %O\n", obj);
  if ( val == tort_s(quote) ) {
    return tort_car(obj->cdr);
  }
  _tort_debug_expr = obj;
  if ( val == tort_s(define) ) {
    tort_v name = tort_car(obj->cdr);
    if ( tort_h_mtable(name) == tort__mt(symbol) ) {
      /* (define name val) */
      val = tort_car(tort_cdr(obj->cdr));
    } else {
      /* (define (name . lambda-args) . lambda-body) */
      val = tort_cons(tort_s(lambda), tort_cons(tort_cdr(name), tort_cdr(obj->cdr)));
      name = tort_car(name);
    }
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
  else if ( val == tort_s(let) || val == tort_s(ANDlet) ) {
    val = tort_car(obj->cdr); /* bindings */
    if ( val != tort_nil || obj->car == tort_s(ANDlet) )
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
    val  = tort_send(tort_s(lisp_eval_car), val, env);
    args = tort_send(tort_s(lisp_eval_args), obj->cdr, env);
    // if ( _tort_lisp_trace || 1) tort_printf(tort_stderr, "\n  lisp_eval %O\n", obj);
    // if ( _tort_lisp_trace || 1) tort_printf(tort_stderr, "\n  f = %O, args = %O\n", val, args);
    return_tort_send(tort_s(lisp_apply), val, args, env);
  }
}

tort_v _tort_m_symbol__lisp_eval_car(tort_tp tort_v obj, tort_v env)
{
  tort_v val = tort_send(tort__s(get), env, obj);
  if ( val == tort_nil ) return tort_error(tort_ta "get: symbol '%O is unbound.", obj);
  val = *tort_L(val);
  if ( _tort_lisp_trace > 1 ) tort_printf(tort_stderr, "   EC %O => %O\n", obj, val);
  return val;
}

tort_v _tort_m_object__lisp_eval_car(tort_tp tort_v obj, tort_v env)
{
  if ( _tort_lisp_trace > 1 ) {
    tort_v val = tort_send(tort_s(lisp_eval), obj, env);
    tort_printf(tort_stderr, "   EC %O => %O\n", obj, val);
    return val;
  } else
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
  if ( _tort_lisp_trace > 2 ) tort_printf(tort_stderr, "    EA %O %O\n", obj, argv);
  return_tort_send(tort_s(_sendv), obj, tort_vector_data(argv), tort_vector_size(argv));
}

tort_v _tort_m_object__lisp_apply(tort_tp tort_cons *obj, tort_v args, tort_v env)
{
  if ( _tort_lisp_trace > 2 ) tort_printf(tort_stderr, "    EA %O %O\n", obj, args);
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
  size_t i = 0;
  if ( ! obj->size ) return tort_nil; // undef
  while ( i < obj->size - 1 )
    tort_send(tort_s(lisp_eval), obj->data[i ++], env);
  return_tort_send(tort_s(lisp_eval), obj->data[i], env);
}

tort_v _tort_m_object__lisp_eval_body(tort_tp tort_cons *obj, tort_v env)
{
  if ( obj == tort_nil ) return obj; // undef
  while ( obj->cdr != tort_nil ) {
    tort_send(tort_s(lisp_eval), obj->car, env);
    obj = obj->cdr;
  }
  return_tort_send(tort_s(lisp_eval), obj->car, env);
}

//////////////////////////////////////////////////////////////////////

tort_v _tort_m_lisp_repl__read(tort_tp tort_repl *repl)
{
  repl->expr = tort_send(tort_s(lisp_read), repl->input);
  return repl;
}
tort_v _tort_m_lisp_repl__new_environment(tort_tp tort_repl *repl)
{
  tort_lisp_environment *env = 
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
  ((tort_lisp_environment*) repl->env)->msg = _tort_message->previous_message; // FIXME?
  repl->result = tort_send(tort_s(lisp_eval_top_level), repl->expr, repl->env);
  return repl;
}
tort_v _tort_m_lisp_repl__print(tort_tp tort_repl *repl, tort_v thing)
{
  tort_send(tort_s(lisp_write), thing, repl->output);
  return repl;
}

tort_v _tort_m_lisp_repl__load(tort_tp tort_repl *repl, tort_v name)
{
  tort_v io, result;
  repl = tort_send(tort_s(clone), repl);
  repl->catch = tort_nil;
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

tort_v tort_runtime_initialize_lisp_eval()
{
  tort_send(tort_s(load), tort_mt(dynlib), tort_string_new_cstr("libtortext"));
  tort_mtable_create_class("lisp_repl", tort_mt(repl));
  tort_mtable_create_class("lisp_formals", 0);
  tort_mtable_create_class("lisp_closure", 0);
  tort_mtable_create_class("lisp_environment", 0);
  {
    const char *str = getenv("TORT_LISP_LIB_DIR");
    tort_v dir = tort_string_new_cstr(str && *str ? str : TORT_LISP_LIB_DIR);
    tort_send(tort_s(set), tort_(root), tort_s(lisp_lib_dir), dir);
  }
  return 0;
}

