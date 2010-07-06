#include "tort/core.h"


tort_v tort_error_messagev(const char *type, const char *format, va_list vap)
{
  if ( _tort->_in_error ) {
    fprintf(stderr, "\ntort: too many errors\n");
  } else {
    _tort->_in_error = tort_true;
    fprintf(stderr, "tort %s: ", type);
    vfprintf(stderr, format, vap);
    fprintf(stderr, "\n");
    _tort->_in_error = 0;
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
  if ( _tort->_initialized ) {
    fprintf(stderr, "  in message: ");
    tort_inspect(tort_stderr, _tort_message);
    fprintf(stderr, "\n");
  }
  _tort->fatal(format, vap);
  va_end(vap);
  return 0;
}


tort_v tort_error (const char *format, ...)
{
  va_list vap;
  va_start(vap, format);
  _tort->error(format, vap);
  va_end(vap);
  return 0;
}


void tort_runtime_initialize_error()
{
  _tort->_in_error = 0;
  _tort->error = _tort_error;
  _tort->fatal = _tort_fatal;
}


