#include "tort/tort.h"


typedef struct tort_pair {
  tort_val car, cdr;
} tort_pair;

tort_val tort_cons(tort_val a, tort_val d)
{
  tort_val val = tort_allocate(0, 0, sizeof(tort_pair), _tort->_mt_pair);
  tort_ref(tort_pair, val)->car = a;
  tort_ref(tort_pair, val)->cdr = d;
  return val;
}

#define ACCESSOR(X)							\
  static								\
  tort_val _tort_pair_##X(tort_val _tort_message, tort_val rcvr)	\
  {									\
    return tort_ref(tort_pair, rcvr)->X;				\
  }									\
  static								\
  tort_val _tort_pair_set_##X##E(tort_val _tort_message, tort_val rcvr,	\
			       tort_val val)				\
  {									\
    tort_ref(tort_pair, rcvr)->X = val;					\
    return rcvr;							\
  }

ACCESSOR(car)
ACCESSOR(cdr)

#undef ACCESSOR

static							    
tort_val _tort_list_size(tort_val _tort_message, tort_val rcvr) 
{
  size_t i = 0;

  while ( rcvr != tort_nil )  {
    ++ i;
    if ( tort_h_mtable(rcvr) == _tort->_mt_pair ) {
      rcvr = tort_ref(tort_pair, rcvr)->cdr;
    } else {
      break;
    }
  }
  return tort_i(i);
}


static							    
tort_val _tort_list_lisp_write(tort_val _tort_message, tort_val rcvr, tort_val io) 
{
  tort_printf(io, "(");
  while ( rcvr == tort_nil ) {
    if ( tort_h_mtable(rcvr) == _tort->_mt_pair ) {
      tort_send(tort__s(lisp_write), tort_ref(tort_pair, rcvr)->car, io);
      rcvr = tort_ref(tort_pair, rcvr)->cdr;
      if ( rcvr == tort_nil ) {
	break;
      }
    } else {
      tort_printf(io, ". ");
      tort_send(tort__s(lisp_write), rcvr, io);
      break;
    }
    tort_printf(io, " "); 
  } 
  tort_printf(io, ")");
  return tort_nil;
}


static
tort_val _tort_list_list_TO_vector(tort_val _tort_message, tort_val rcvr, tort_val io)
{
  tort_val size = tort_send(tort__s(size), rcvr);
  tort_val vec = tort_vector_new(0, tort_I(size));
  size_t i = 0;
  while ( rcvr != tort_nil ) {
    if ( tort_h_mtable(rcvr) == _tort->_mt_pair ) {
      tort_vector_data(vec)[i ++] = tort_ref(tort_pair, rcvr)->car;
      rcvr = tort_ref(tort_pair, rcvr)->cdr;
    } else {
      tort_vector_data(vec)[i ++] = rcvr;
      break;
    }
  } 
  return vec;
}

#define FP(s) tort_ref(tort_io, s)->fp

#define VALUE tort_val
#define READ_DECL VALUE _tort_io_lisp_read (tort_val _tort_message, tort_val stream)
#define READ_CALL() tort_send(tort__s(lisp_read), stream)
#define MALLOC(s) tort_malloc(s)
#define REALLOC(p, s) tort_realloc(p, s)
#define GETC(s) fgetc(FP(s))
#define UNGETC(s, c) ungetc(c, FP(s))
#define EOS tort_eos
#define CONS(x,y) tort_cons(x, y)
#define SET_CDR(CONS,V) tort_send(tort__s(set_cdrE), CONS, V)
#define MAKE_CHAR(I) tort_i(I)
#define STRING(b, l) tort_string_new(b, l)
#define LIST_2_VECTOR(X) tort_send(tort__s(list_TO_vector), X)
#define SYMBOL_DOT tort__s(DOT)
#define SYMBOL(NAME) tort_s(NAME)
#define STRING_2_NUMBER(s, radix) _tort_string_to_number(s, radix)
#define STRING_2_SYMBOL(s) tort_symbol_make(tort_string_data(s))
#define EQ(X, Y) ((X) == (Y))
#define NIL tort_nil
#define T  tort_true
#define F  tort_false
#define U  tort_nil
#define ERROR(format, args...) tort_error(format, ##args)

tort_val _tort_string_to_number(tort_val s, int radix)
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
  _tort->_mt_pair = tort_mtable_create(_tort->_mt_object);
  tort_add_method(_tort->_mt_pair, "car", _tort_pair_car);
  tort_add_method(_tort->_mt_pair, "set-car!", _tort_pair_set_carE);
  tort_add_method(_tort->_mt_pair, "cdr", _tort_pair_cdr);
  tort_add_method(_tort->_mt_pair, "set-cdr!", _tort_pair_set_cdrE);
  tort_add_method(_tort->_mt_pair, "lisp_write", _tort_list_lisp_write);
  tort_add_method(_tort->_mt_nil,  "lisp_write", _tort_list_lisp_write);
  tort_add_method(_tort->_mt_pair, "size", _tort_list_size);
  tort_add_method(_tort->_mt_nil,  "size", _tort_list_size);
  tort_add_method(_tort->_mt_pair, "list->vector", _tort_list_list_TO_vector);
  tort_add_method(_tort->_mt_nil,  "list->vector", _tort_list_list_TO_vector);

  _tort->_s_lisp_read = tort_symbol_make("lisp_read");
  _tort->_s_set_cdrE = tort_symbol_make("set-cdr!");
  _tort->_s_DOT = tort_symbol_make(".");
  _tort->_s_list_TO_vector = tort_symbol_make("list->vector");
    
  tort_add_method(_tort->_mt_io, "lisp_read", _tort_io_lisp_read);
}

