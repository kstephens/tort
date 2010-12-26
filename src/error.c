#include "tort/core.h"


tort_v tort_error_messagev(const char *type, const char *format, va_list vap)
{
  if ( tort_(_in_error) ) {
    fprintf(stderr, "\ntort: too many errors\n");
  } else {
    tort_(_in_error) = tort_true;
    fprintf(stderr, "tort %s: ", type);
    vfprintf(stderr, format, vap);
    fprintf(stderr, "\n");
    tort_(_in_error) = 0;
  }
  fflush(stderr);
  return 0;
}


tort_v tort_error_message(const char *format, ...)
{
  va_list vap;
  va_start(vap, format);
  tort_error_messagev("error", format, vap);
  va_end(vap);
  return 0;
}

tort_v _tort_fatal(const char *format, va_list vap)
{
  tort_error_messagev("fatal", format, vap);
  abort();
  return 0;
}


tort_v _tort_error(const char *format, va_list vap)
{
  tort_error_messagev("error", format, vap);
  abort();
  return 0;
}


tort_v tort_fatal (const char *format, ...)
{
  va_list vap;
  va_start(vap, format);
  if ( tort_(_initialized) ) {
    fprintf(stderr, "  in message: ");
    tort_inspect(tort_stderr, _tort_message);
    fprintf(stderr, "\n");
  }
  tort_(fatal)(format, vap);
  va_end(vap);
  return 0;
}


tort_v tort_error (const char *format, ...)
{
  va_list vap;
  va_start(vap, format);
  tort_(error)(format, vap);
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


