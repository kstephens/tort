#include "tort/core.h"


tort_v tort_error_messagev(const char *type, const char *format, va_list *vapp)
{
  if ( tort_(_in_error) ) {
    fprintf(stderr, "\ntort: too many errors\n");
  } else {
    tort_(_in_error) = tort_true;
    tort_printf(tort_stderr, "\ntort %s: ", type);
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

tort_v _tort_fatal(const char *format, va_list *vapp)
{
  tort_error_messagev("fatal", format, vapp);
  abort();
  return 0;
}


tort_v _tort_error(const char *format, va_list *vapp)
{
  tort_error_messagev("error", format, vapp);
  tort_send(tort__s(__debugger), _tort_message);
  return 0;
}


tort_v tort_fatal (const char *format, ...)
{
  va_list vap;
  va_start(vap, format);
  if ( tort_(_initialized) ) {
    tort_printf(tort_stderr, "  in message: %T\n", _tort_message);
  }
  tort_(fatal)(format, &vap);
  va_end(vap);
  return 0;
}


tort_v tort_error (const char *format, ...)
{
  va_list vap;
  va_start(vap, format);
  tort_(error)(format, &vap);
  va_end(vap);
  return 0;
}


tort_v tort_runtime_initialize_error()
{
  tort_(_in_error) = 0;
  tort_(error) = _tort_error;
  tort_(fatal) = _tort_fatal;

  return 0;
}


