#include "tort/tort.h"

#include <stdio.h>
#include <stdlib.h>


void tort_error_messagev(const char *type, const char *format, va_list vap)
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
}


void tort_error_message(const char *format, ...)
{
  va_list vap;
  va_start(vap, format);
  tort_error_messagev("error", format, vap);
  va_end(vap);
}

void _tort_fatal(const char *format, va_list vap)
{
  tort_error_messagev("fatal", format, vap);
  abort();
}


void _tort_error(const char *format, va_list vap)
{
  tort_error_messagev("error", format, vap);
  abort();
}


void tort_fatal (const char *format, ...)
{
  va_list vap;
  va_start(vap, format);
  if ( _tort->_initialized ) {
    fprintf(stderr, "  in message: ");
    tort_write(_tort_message, tort_stderr);
    fprintf(stderr, "\n");
  }
  _tort->fatal(format, vap);
  va_end(vap);
}


void tort_error (const char *format, ...)
{
  va_list vap;
  va_start(vap, format);
  _tort->error(format, vap);
  va_end(vap);
}


void tort_runtime_initialize_error()
{
  _tort->_in_error = 0;
  _tort->error = _tort_error;
  _tort->fatal = _tort_fatal;
}


