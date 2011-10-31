#include "tort/tort.h"

tort_ACCESSOR(message,tort_v,selector);
tort_ACCESSOR(message,tort_v,receiver);
tort_ACCESSOR(message,tort_v,previous_message);
tort_ACCESSOR(message,tort_v,fiber);
tort_ACCESSOR(message,tort_v,method);
tort_ACCESSOR(message,tort_v,mtable);
tort_ACCESSOR(message,tort_v,argc);
#if TORT_MESSAGE_FILE_LINE
tort_ACCESSOR(message,charP,file);
tort_ACCESSOR(message,int,line);
#endif

tort_v _tort_M_message__new(tort_tp tort_v mtable)
{
  tort_v val = tort_allocate(mtable, sizeof(tort_message));
  return_tort_send(tort__s(initialize), val);
}

tort_v _tort_m_message__initialize(tort_tp tort_message *rcvr)
{
  rcvr->selector = 0;
  rcvr->receiver = 0;
  rcvr->previous_message = 0;
  rcvr->fiber = 0;
  rcvr->method = 0;
  rcvr->mtable = 0;
  rcvr->argc = tort_i(0);
#if TORT_MESSAGE_FILE_LINE
  rcvr->file = "<unknown>";
  rcvr->line = 0;
#endif
  return rcvr;
}

tort_v _tort_m_object____message(tort_thread_param tort_v rcvr)
{
  rcvr = _tort_message;
  // rcvr = tort_ref(tort_message, rcvr)->previous_message;
  return_tort_send(tort__s(clone), rcvr);
}

tort_v _tort_m_message__backtrace(tort_tp tort_v rcvr)
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


