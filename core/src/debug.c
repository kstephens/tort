#include "tort/core.h"

void tort_debug_stop_at()
{
  /* NOTHING */
}

#define IO tort_stderr
#define printf(fmt, args...) tort_printf(IO, fmt, ##args)

tort_v _tort_debug_expr;

tort_v _tort_m_object____debugger(tort_tp tort_v rcvr)
{
  tort_v bt;

  (void) tort__s(__printfs);
  printf("\ntort debugger:\n");
  printf("rcvr = "); tort_inspect(IO, rcvr); printf("\n");
  printf("type = %s\n", tort_object_name(tort_h_mtable(rcvr)));
  printf("expr = %O\n", _tort_debug_expr); 
  bt = tort_send(tort_s(backtrace), _tort_message);
  printf("backtrace =\n----\n"); 
  tort_vector_loop(bt, msg); {
    printf("  ");
    tort_inspect(IO, msg);
    printf("\n");
  }
  tort_vector_loop_end(bt);

  printf("----\n");
  tort_debug_stop_at();

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

tort_v _tort_m_object___name(tort_tp tort_v rcvr)
{
  return tort_string_new_cstr(tort_object_name(rcvr));
}

tort_v tort_runtime_initialize_debug()
{
  return 0;
}

