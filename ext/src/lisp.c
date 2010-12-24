#include "tort/core.h"


/********************************************************************/


typedef struct tort_pair {
  tort_v car, cdr;
} tort_pair;


/********************************************************************/
static tort_v _mt_pair;

tort_v tort_cons(tort_v a, tort_v d)
{
  tort_v val = tort_allocate(0, 0, sizeof(tort_pair), _mt_pair);
  tort_ref(tort_pair, val)->car = a;
  tort_ref(tort_pair, val)->cdr = d;
  return val;
}


#define ACCESSOR(X)							\
  static								\
  tort_v _tort_pair_##X(tort_thread_param tort_v rcvr)	\
  {									\
    return tort_ref(tort_pair, rcvr)->X;				\
  }									\
  static								\
  tort_v _tort_pair_set_##X##E(tort_thread_param tort_v rcvr,	\
			       tort_v val)				\
  {									\
    tort_ref(tort_pair, rcvr)->X = val;					\
    return rcvr;							\
  }

ACCESSOR(car)
ACCESSOR(cdr)

#undef ACCESSOR


tort_v _tort_list_size(tort_thread_param tort_v rcvr) /**/
{
  size_t i = 0;

  while ( rcvr != tort_nil )  {
    ++ i;
    if ( tort_h_mtable(rcvr) == tort_class_get("pair") ) {
      rcvr = tort_ref(tort_pair, rcvr)->cdr;
    } else {
      break;
    }
  }
  return tort_i(i);
}



tort_v _tort_list_lisp_write(tort_thread_param tort_v rcvr, tort_v io) /**/
{
  tort_printf(io, "(");
  while ( rcvr != tort_nil ) {
    if ( tort_h_mtable(rcvr) == tort_class_get("pair") ) {
      tort_send(tort_symbol_make("lisp_write"), tort_ref(tort_pair, rcvr)->car, io);
      rcvr = tort_ref(tort_pair, rcvr)->cdr;
      if ( rcvr == tort_nil ) {
	break;
      }
    } else {
      tort_printf(io, ". ");
      tort_send(tort_symbol_make("lisp_write"), rcvr, io);
      break;
    }
    tort_printf(io, " "); 
  } 
  tort_printf(io, ")");
  return tort_nil;
}


tort_v _tort_list_list_TO_vector(tort_thread_param tort_v rcvr, tort_v io) /**/
{
  tort_v size = tort_send(tort__s(size), rcvr);
  tort_v vec = tort_vector_new(0, tort_I(size));
  size_t i = 0;
  while ( rcvr != tort_nil ) {
    if ( tort_h_mtable(rcvr) == tort_class_get("pair") ) {
      tort_vector_data(vec)[i ++] = tort_ref(tort_pair, rcvr)->car;
      rcvr = tort_ref(tort_pair, rcvr)->cdr;
    } else {
      tort_vector_data(vec)[i ++] = rcvr;
      break;
    }
  } 
  return vec;
}


/********************************************************************/

#define IO (io ? io : tort_stdout)

#define printf(fmt, args...) tort_send(tort__s(printf), IO, fmt, ##args)


tort_v _tort_object_lisp_write(tort_thread_param tort_v rcvr, tort_v io)
{
  printf("(make <object> @%p)", (void *) rcvr);
  return tort_nil;
}


tort_v _tort_vector_lisp_write(tort_thread_param tort_v rcvr, tort_v io)
{
  printf("#(");
  tort_vector_loop(rcvr, obj) {
    if ( obj_i > 0 ) printf(" ");
    tort_send(tort_symbol_make("lisp_write"), obj, IO);
  } tort_vector_loop_end(rcvr);
  printf(")");
  return tort_nil;
}


tort_v _tort_symbol_lisp_write(tort_thread_param tort_v rcvr, tort_v io)
{
  if ( tort_ref(tort_symbol, rcvr)->name != tort_nil ) {
    printf("%s", (char *) tort_symbol_data(rcvr));
  } else {
    printf("(make <symbol> @%p)", (void*) rcvr);
  }
  return tort_nil;
}


tort_v _tort_boolean_lisp_write(tort_thread_param tort_v rcvr, tort_v io)
{
  printf(rcvr == tort_false ? "#f" : "#t");
  return tort_nil;
}


tort_v _tort_map_lisp_write(tort_thread_param tort_v rcvr, tort_v io)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry **x = map->entry, *entry;
  size_t entry_i = 0;

  printf("(make <map> ");
  while ( (entry = *(x ++)) ) {
    if ( entry_i > 0 ) printf(" ");
    tort_send(tort_symbol_make("lisp_write"), entry->key, IO);
    printf(" ");
    tort_send(tort_symbol_make("lisp_write"), entry->value, IO);
    entry_i ++;
  }
  printf(")");
  return tort_nil;
}


tort_v _tort_eos_lisp_write(tort_thread_param tort_v rcvr, tort_v io)
{
  printf("#e");
  return tort_nil;
}


#undef printf
#undef IO


/********************************************************************/

extern 
tort_v _tort_io_lisp_read (tort_thread_param tort_v stream)
  ;

#define FP(s) tort_ref(tort_io, s)->fp

#define VALUE tort_v
#define READ_DECL VALUE _tort_io_lisp_read (tort_thread_param tort_v stream)
#define READ_CALL() tort_send(tort_symbol_make("lisp_read"), stream)
#define MALLOC(s) tort_malloc(s)
#define REALLOC(p, s) tort_realloc(p, s)
#define GETC(s) fgetc(FP(s))
#define UNGETC(s, c) ungetc(c, FP(s))
#define EOS tort_eos
#define CONS(x,y) tort_cons(x, y)
#define SET_CDR(CONS,V) tort_send(tort_symbol_make("set-cdr!"), CONS, V)
#define MAKE_CHAR(I) tort_i(I)
#define STRING(b, l) tort_string_new(b, l)
#define LIST_2_VECTOR(X) tort_send(tort_symbol_make("list->vector"), X)
#define SYMBOL_DOT tort_symbol_make(".")
#define SYMBOL(NAME) tort_s(NAME)
#define STRING_2_NUMBER(s, radix) _tort_string_to_number(s, radix)
#define STRING_2_SYMBOL(s) tort_symbol_make(tort_string_data(s))
#define EQ(X, Y) ((X) == (Y))
#define NIL tort_nil
#define T  tort_true
#define F  tort_false
#define U  tort_nil
#define ERROR(format, args...) tort_error(format, ##args)

tort_v _tort_string_to_number(tort_v s, int radix) /**/
{
  long d = 0;
  if ( radix == 10 && sscanf(tort_string_data(s), "%ld", &d) == 1 ) {
    return tort_i(d);
  } else {
    return tort_false;
  }
}

#include "lispread.c"

void tort_runtime_initialize_lisp()
{
  _mt_pair = tort_class_make("pair", 0);

  /* Reused methods. */
  tort_add_method(_tort->_mt_tagged, "lisp_write", _tort_tagged__inspect);
  tort_add_method(_tort->_mt_string, "lisp_write", _tort_string__inspect);
  tort_add_method(_tort->_mt_method, "lisp_write", _tort_method__inspect);
  tort_add_method(_tort->_mt_message, "lisp_write", _tort_message__inspect);
  tort_add_method(_tort->_mt_nil,    "lisp_write", _tort_nil__inspect);

  tort_add_method(_mt_pair, "value", _tort_pair_car);

  /* FIXME */
  tort_add_method(_mt_pair, "car", _tort_pair_car);
  tort_add_method(_mt_pair, "set-car!", _tort_pair_set_carE);
  tort_add_method(_mt_pair, "cdr", _tort_pair_cdr);
  tort_add_method(_mt_pair, "set-cdr!", _tort_pair_set_cdrE);
  tort_add_method(_mt_pair, "lisp_write", _tort_list_lisp_write);
  tort_add_method(_tort->_mt_nil,  "lisp_write", _tort_list_lisp_write);
  tort_add_method(_mt_pair, "size", _tort_list_size);
  tort_add_method(_tort->_mt_nil,  "size", _tort_list_size);
  tort_add_method(_mt_pair, "list->vector", _tort_list_list_TO_vector);
  tort_add_method(_tort->_mt_nil,  "list->vector", _tort_list_list_TO_vector);

 
  tort_add_method(_tort->_mt_io, "lisp_read", _tort_io_lisp_read);

  tort_add_method(_tort->_mt_boolean, "lisp_write", _tort_boolean_lisp_write);
  tort_add_method(_tort->_mt_eos, "lisp_write", _tort_eos_lisp_write);
  tort_add_method(_tort->_mt_map, "lisp_write", _tort_map_lisp_write);
  tort_add_method(_tort->_mt_object, "lisp_write", _tort_object_lisp_write);
  tort_add_method(_tort->_mt_symbol, "lisp_write", _tort_symbol_lisp_write);
  tort_add_method(_tort->_mt_vector, "lisp_write", _tort_vector_lisp_write);
}

