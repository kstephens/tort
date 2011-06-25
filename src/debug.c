#include "tort/core.h"

/********************************************************************/

void tort_debug_stop_at()
{
  /* NOTHING */
}

#define IO tort_stderr
#define printf(fmt, args...) tort_printf(IO, fmt, ##args)

tort_v _tort_m_object____message(tort_thread_param tort_v rcvr)
{
  rcvr = _tort_message;
  // rcvr = tort_ref(tort_message, rcvr)->previous_message;
  return_tort_send(tort__s(clone), rcvr);
}

tort_v _tort_m_message__backtrace(tort_thread_param tort_v rcvr)
{
  tort_v v, msg;
  size_t i = 0;

  i = 0;
  msg = rcvr;
  while ( msg != tort_nil ) {
    i ++;
    msg = tort_ref(tort_message, msg)->previous_message;
  }

  v = tort_send(tort__s(new), tort__mt(vector), tort_i(i));

  i = 0;
  msg = rcvr;
  while ( msg != tort_nil ) {
    msg = tort_send(tort__s(clone), msg);
    tort_vector_data(v)[i ++] = msg;
    msg = tort_ref(tort_message, msg)->previous_message; 
  }

  return v;
}

tort_v _tort_m_object____debugger(tort_thread_param tort_v rcvr)
{
  tort_v bt;

  printf("\ntort debugger:\n");
  printf("rcvr = "); tort_inspect(IO, rcvr); printf("\n");
  bt = tort_send(tort_s(backtrace), _tort_message);
  printf("backtrace = "); 
  tort_vector_loop(bt, msg); {
    printf("  ");
    tort_inspect(IO, msg);
    printf("\n");
  }
  tort_vector_loop_end(bt);

  printf("\n");

  return rcvr;
}
#undef printf

const char *tort_mtable_name_(tort_v val)
{
  if ( 0 ) {
  }
#define tort_d_mt(N) else if ( val == tort__mt(N) ) { return #N; }
#include "tort/d_mt.h"

  return "";
}

const char *tort_object_name_(tort_v val)
{
  char *str = 0;
  static int bufi = 0;
#define S 63
  static char bufa[16][S + 1];
  char *buf = bufa[bufi = (bufi + 1) % 16];

  buf[S] = '\0';

  if ( val == tort_nil ) {
    snprintf(str = buf, S, "nil");
  }
  else if ( val == 0 ) {
    snprintf(str = buf, S, "#<NULL>");
  }
  else if ( val == tort_true ) {
    snprintf(str = buf, S, "true");
  }
  else if ( val == tort_false ) {
    snprintf(str = buf, S, "false");
  }
  else if ( tort_taggedQ(val) ) {
    snprintf(str = buf, S, "%lld", (long long) tort_I(val));
  }
  else if ( tort_h_mtable(val) == tort__mt(string) ) {
    snprintf(str = buf, S, "\"%s\"", tort_string_data(val));
  }
  else if ( tort_h_mtable(val) == tort__mt(symbol) ) {
    if ( tort_ref(tort_symbol, val)->name != tort_nil ) {
      snprintf(str = buf, S, "%s", tort_symbol_data(val));
    } else {
      snprintf(str = buf, S, "@symbol(@%p)", (void*) val);
    }
  }
  else if ( tort_h_mtable(val) == tort__mt(method) ) {
    tort_v name;
    if ( (name = tort_ref(tort_method, val)->name) != tort_nil ) {
      snprintf(str = buf, S, "@method(%s)", tort_object_name_(name));
    } else {
      snprintf(str = buf, S, "@method(@%p)", (void*) val);
    }
  }
  else if ( val == _tort ) {
    snprintf(str = buf, S, "@tort");
  }
  else if ( val == tort_(root) ) {
    snprintf(str = buf, S, "@root");
  }
  else if ( val == tort_stdin ) {
    snprintf(str = buf, S, "@io(stdin)");
  }
  else if ( val == tort_stdout ) {
    snprintf(str = buf, S, "@io(stdout)");
  }
  else if ( val == tort_stderr ) {
    snprintf(str = buf, S, "@io(stderr)");
  }
#define tort_d_mt(N) else if ( val == tort__mt(N) ) { snprintf(str = buf, S, "@mtable(%s)", #N); }
#include "tort/d_mt.h"
#define tort_d_mt(N) else if ( tort_h_mtable(val) == tort__mt(N) ) { snprintf(str = buf, S, "@%s(@%p)", #N, (void*) val); }
#include "tort/d_mt.h"
  else if ( tort_h_mtable(val) == tort__mt(mtable) ) { snprintf(str = buf, S, "@mtable(%p)", (void*) val); }
  else {
    return 0;
  }

  if ( buf[S] != '\0' ) {
    buf[S - 3] = '.';
    buf[S - 2] = '.';
    buf[S - 1] = '.';
    buf[S] = '\0';
  }

  return str;
}


const char *tort_object_name(tort_v val)
{
  char *str = (char*) tort_object_name_(val);
  if ( ! str ) {
    static int bufi = 0;
    static char bufa[16][S + 1];
    char *buf = bufa[bufi = (bufi + 1) % 16];
    
    buf[S] = '\0';
    
    if ( 0 ) {
    }
#define tort_d_mt(N) else if ( tort_h_mtable(val) == tort__mt(N) ) { snprintf(str = buf, S, "!%s(@%p)", #N, (void*) val); }
#include "tort/d_mt.h"
    else {
      snprintf(str = buf, S, "@?(@%p)", (void*) val);
    }

    if ( buf[S] != '\0' ) {
      buf[S - 3] = '.';
      buf[S - 2] = '.';
      buf[S - 1] = '.';
      buf[S] = '\0';
    }

  }
  return str;
}
#undef S

tort_v _tort_m_object___name(tort_thread_param tort_v rcvr)
{
  return tort_string_new_cstr(tort_object_name(rcvr));
}

/********************************************************************/

tort_v tort_runtime_initialize_debug()
{
  return 0;
}

