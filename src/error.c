#include "tort/tort.h"

#include <stdio.h>
#include <stdlib.h>




void tort_fatal (const char *format, ...)
{
  va_list vap;
  va_start(vap, format);
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


