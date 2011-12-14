#include "tort/tort.h"

tort_ACCESSOR(message,tort_v,selector);
tort_ACCESSOR(message,tort_v,receiver);
tort_ACCESSOR(message,tort_v,previous_message);
tort_ACCESSOR(message,tort_v,fiber);
tort_ACCESSOR(message,tort_v,method);
tort_ACCESSOR(message,tort_v,mtable);
tort_ACCESSOR(message,tort_v,argc);
#if TORT_MESSAGE_FILE_LINE
tort_ACCESSOR(message,tort_v,caller_info);
#endif

tort_ACCESSOR(caller_info,charP,file)
tort_ACCESSOR(caller_info,int,line)
tort_ACCESSOR(caller_info,tort_v,data)

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
  rcvr->caller_info = tort_(unknown_caller_info);
#endif
  return rcvr;
}

tort_v _tort_m_object____message(tort_tp tort_v rcvr)
{
  rcvr = _tort_message;
  // rcvr = tort_ref(tort_message, rcvr)->previous_message;
  return_tort_send(tort__s(clone), rcvr);
}

tort_v _tort_m_message__backtrace(tort_tp tort_message *rcvr)
{
  tort_v v;
  tort_message *msg, **prev_msg;

  v = tort_send(tort__s(new), tort__mt(vector), tort_i(0));

  msg = tort_nil;
  prev_msg = &msg;
  while ( rcvr != tort_nil ) {
    msg = *prev_msg = tort_send(tort__s(clone), rcvr);
    tort_send(tort__s(add), v, msg);
    rcvr = rcvr->previous_message;
    prev_msg = &(msg->previous_message);
  }

  return v;
}


