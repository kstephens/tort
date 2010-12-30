#include "tort/core.h"

#define IO io
#define FP IO->fp

tort_v _tort_m_io__printfv(tort_tp tort_io* io, const char *format, va_list *vapp)
{
#define vap (*vapp)
  const char *b = format, *e;

  while ( *b ) {
    size_t size;

    e = b;
    while ( *e && *e != '%' )
      ++ e;

    if ( (size = e - b) ) {
      tort_send(tort__s(__write), io, b, size);
    }

    b = e;
    if ( ! *b ) break;

    if ( *b == '%' ) {
      int l = 0;
      int done = 0;
      char fmt[16], *fp = fmt;
      *(fp ++) = *(b ++);
#define FMT (*fp = 0, fmt)
      do {
	if ( fp >= fmt + sizeof(fmt) ) abort();
	switch ( (*(fp ++) = *(b ++)) ) {
	case 0:
	  abort();
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	  break;
	case 'l':
	  ++ l;
	  break;
	case 'c': case 'd': case 'u': case 'o': case 'x': case 'X':
	  switch ( l ) {
	  case 0:
	    fprintf(FP, FMT, va_arg(vap, int));
	    break;
	  case 1:
	    fprintf(FP, FMT, va_arg(vap, long));
	    break;
	  case 2:
	    fprintf(FP, FMT, va_arg(vap, long long));
	    break;
	  default:
	    abort();
	    break;
	  }
	  done = 1; break;
	case 'p':
	  *fp = 0;
	  fprintf(FP, FMT, va_arg(vap, void*));
	  done = 1; break;
	case 's':
	  *fp = 0;
	  fprintf(FP, FMT, va_arg(vap, char*));
	  done = 1; break;
	case 'T':
	  tort_send(tort_s(_inspect), va_arg(vap, tort_v), IO);
	  done = 1; break;
	case 'O':
	  tort_send(tort_s(lisp_write), va_arg(vap, tort_v), IO);
	  done = 1; break;
	default:
	  abort();
	  break;
	}
      } while ( ! done );
    }
  }

#undef vap
  return io;
}

tort_v _tort_m_io__printf(tort_tp tort_io* io, const char *fmt, ...)
{
  va_list vap;
  va_start(vap, fmt);
  tort_send(tort_s(printfv), io, fmt, &vap);
  va_end(vap);
  return io;
}


/********************************************************************/


#ifdef __LINUX__
int 
_tort_printf_object (FILE *stream,
		     __const struct printf_info *info,
		     __const void *__const *args
		     )
{
  tort_v v;
  int len = 128; /* ??? */

  v = *(tort_v*) args[0];
  v = tort_inspect(FP_TORT_OBJ(stream), v);

  return len;
}


static
int 
_tort_printf_object_lisp (FILE *stream,
		     __const struct printf_info *info,
		     __const void *__const *args
		     )
{
  tort_v v;
  int len = 128; /* ??? */

  v = *(tort_v*) args[0];
  v = tort_send(tort__s(lisp_write), v, FP_TORT_OBJ(stream));

  return len;
}




static int
_tort_printf_extension_arginfo (
				    const struct printf_info *info, 
				    size_t n,
				    int *argtypes, 
				    int *size
				    )
{
  if (n > 0) {
    argtypes[0] = PA_POINTER;
    size[0] = sizeof(tort_v);
  }
  return 1;
}
#endif

tort_v tort_runtime_initialize_printf()
{
#ifdef __LINUX__
  /* Register the print functions for tort_v.  */
  register_printf_specifier('T', 
			    _tort_printf_object,
			    _tort_printf_extension_arginfo);

  register_printf_specifier('O', 
			    _tort_printf_object_lisp,
			    _tort_printf_extension_arginfo);
#endif

  return tort__mt(io);
}
