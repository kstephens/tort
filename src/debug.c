#include "tort/tort.h"


#define IO tort_stderr
#define printf(fmt, args...) tort_send(tort__s(printf), IO, fmt, ##args)

static 
tort_val _tort_object___message(tort_val _tort_message, tort_val rcvr)
{
  rcvr = _tort_message;
  // rcvr = tort_ref(tort_message, rcvr)->previous_message;
  return tort_send(tort__s(clone), rcvr);
}


static
tort_val _tort_message_backtrace(tort_val _tort_message, tort_val rcvr)
{
  tort_val v, msg;
  size_t i = 0;

  i = 0;
  msg = rcvr;
  while ( msg != tort_nil ) {
    i ++;
    msg = tort_ref(tort_message, msg)->previous_message;
  }

  v = tort_send(tort__s(new), tort_vector_null, tort_i(i));

  i = 0;
  msg = rcvr;
  while ( msg != tort_nil ) {
    msg = tort_send(tort__s(clone), msg);
    tort_vector_data(v)[i ++] = msg;
    msg = tort_ref(tort_message, msg)->previous_message; 
  }

  return v;
}

static
tort_val _tort_debugger_start(tort_val _tort_message, tort_val rcvr)
{
  tort_val bt;

  printf("\ntort debugger:\n");
  printf("rcvr = "); tort_write(IO, rcvr); printf("\n");
  bt = tort_send(tort_s(backtrace), _tort_message);
  printf("backtrace = "); 
  tort_vector_loop(bt, msg); {
    printf("  ");
    tort_write(IO, msg);
    printf("\n");
  }
  tort_vector_loop_end(bt);

  printf("\n");

  return rcvr;
}


#undef printf


const char *tort_object_name(tort_val val)
{
  static int bufi = 0;
#define S 63
  static char bufa[16][S + 1];
  char *buf = bufa[bufi = (bufi + 1) % 16];

  buf[S] = '\0';
  if ( val == 0 ) {
    snprintf(buf, S, "#<NULL>");
  }
  else if ( val == tort_nil ) {
    snprintf(buf, S, "nil");
  }
  else if ( val == tort_true ) {
    snprintf(buf, S, "true");
  }
  else if ( val == tort_false ) {
    snprintf(buf, S, "false");
  }
  else if ( tort_taggedQ(val) ) {
    snprintf(buf, S, "%ld", (long) tort_I(val));
  }
  else if ( tort_h_mtable(val) == _tort->_mt_string ) {
    snprintf(buf, S, "\"%s\"", tort_string_data(val));
  }
  else if ( tort_h_mtable(val) == _tort->_mt_symbol ) {
    if ( tort_ref(tort_symbol, val)->name != tort_nil ) {
      snprintf(buf, S, "%s", tort_symbol_data(val));
    } else {
      snprintf(buf, S, "!symbol @%p", (void*) val);
    }
  }
  else if ( val == _tort ) {
    snprintf(buf, S, "!tort");
  }
  else if ( val == _tort->root ) {
    snprintf(buf, S, "!root");
  }
#define mt(N) else if ( val == _tort->_mt_##N ) { snprintf(buf, S, "!mtable %s", #N); }
  mt(mtable)
    mt(object)
    mt(map)
    mt(string)
    mt(vector)
    mt(symbol)
    mt(method)
    mt(message)
    mt(nil)
    mt(boolean)
    mt(tagged)
    mt(io)
    mt(block)
#undef mt

#define mt(N) else if ( tort_h_mtable(val) == _tort->_mt_##N ) { snprintf(buf, S, "!%s @%p", #N, (void*) val); }
  mt(mtable)
    mt(object)
    mt(map)
    mt(string)
    mt(vector)
    mt(symbol)
    mt(method)
    mt(message)
    mt(nil)
    mt(boolean)
    mt(tagged)
    mt(io)
    mt(block)
#undef mt

  else {
    snprintf(buf, S, "%p", (void*) val);
  }

  if ( buf[S] != '\0' ) {
    buf[S - 3] = '.';
    buf[S - 2] = '.';
    buf[S - 1] = '.';
    buf[S] = '\0';
  }

  return buf;
}


static
tort_val _tort_object_name(tort_val _tort_message, tort_val rcvr)
{
  return tort_string_new_cstr(tort_object_name(rcvr));
}


void tort_runtime_initialize_debug()
{
  _tort->_s_backtrace = tort_symbol_make("backtrace");
  _tort->_s_backtrace_size = tort_symbol_make("backtrace_size");

  tort_add_method(_tort->_mt_message, "backtrace", _tort_message_backtrace);
  tort_add_method(_tort->_mt_object,  "_name", _tort_object_name);

  tort_add_method(_tort->_mt_object, "__debugger", _tort_debugger_start);
  tort_add_method(_tort->_mt_object, "__message", _tort_object___message);
}

