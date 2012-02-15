#include "tort/core.h"

tort_v tort_error_messagev(const char *type, const char *format, va_list *vapp)
{
  if ( tort_(_in_error) ) {
    fprintf(stderr, "\ntort: too many errors\n");
  } else {
    tort_(_in_error) = tort_true;
    tort_printf(tort_stderr, "%stort %s: ", format[0] == ' ' ? "" : "\n", type);
    tort_printfv(tort_stderr, format, vapp);
    tort_printf(tort_stderr, "\n");
    tort_(_in_error) = 0;
  }
  fflush(stderr);
  return 0;
}

tort_v tort_error_message(const char *format, ...)
{
  va_list vap;
  va_start(vap, format);
  tort_error_messagev("error", format, &vap);
  va_end(vap);
  return 0;
}

tort_v _tort_fatal(tort_tp const char *format, va_list *vapp)
{
  tort_error_messagev("fatal", format, vapp);
  abort();
  return 0;
}

tort_v _tort_error(tort_tp const char *format, va_list *vapp)
{
  tort_v result;
  tort_error_messagev("error", format, vapp);
  result = tort_send(tort__s(__debugger), _tort_message);
  if ( tort_(error_catch) != tort_nil ) {
    result = tort_sendn(tort__s(value), 1, tort_(error_catch), result);
  }
  return result;
}

tort_v tort_fatal (tort_tp const char *format, ...)
{
  va_list vap;
  va_start(vap, format);
  if ( tort_(_initialized) ) {
    tort_printf(tort_stderr, "  in message: %T\n", _tort_message);
  }
  tort_(fatal)(tort_ta format, &vap);
  va_end(vap);
  return 0;
}

tort_v tort_error (tort_tp const char *format, ...)
{
  va_list vap;
  va_start(vap, format);
  tort_(error)(tort_ta format, &vap);
  va_end(vap);
  return 0;
}

tort_v _tort_m_string___error(tort_tp tort_string *fmt, ...)
{
  tort_v result;
  va_list vap;
  va_start(vap, fmt);
  result = _tort_error(tort_ta fmt->data, &vap);
  va_end(vap);
  return result;
}

tort_v _tort_m_string___fatal(tort_tp tort_string *fmt, ...)
{
  tort_v result;
  va_list vap;
  va_start(vap, fmt);
  result = _tort_fatal(tort_ta fmt->data, &vap);
  va_end(vap);
  return result;
}

tort_v _tort_m_initializer__error(tort_tp tort_v init)
{
  tort_(_in_error) = 0;
  tort_(error) = _tort_error;
  tort_(fatal) = _tort_fatal;
  tort_(error_catch) = tort_nil;
  return init;
}
