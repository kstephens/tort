#include "tort/core.h"

#define FP io->fp

tort_v _tort_m_object___printfv(tort_tp tort_io *io, const char *format, const char *fe, va_list *vapp)
{
#define vap (*vapp)
  const char *b = format, *e;

  while ( b < fe ) {
    size_t size;

    e = b;
    while ( e < fe && *e != '%' )
      ++ e;

    if ( (size = e - b) )
      tort_send(tort__s(__write), io, b, size);

    b = e;
    if ( b >= fe ) break;

    if ( *b == '%' ) {
      int l = 0;
      int done = 0;
      char fmt[16], *fp = fmt;
      *(fp ++) = *(b ++);
#define FMT (*fp = 0, fmt)
      do {
	if ( fp >= fmt + sizeof(fmt) ) abort();
	if ( ! (b < fe) ) break;
	fp[1] = 0;
	switch ( (*(fp ++) = *(b ++)) ) {
	case 0:
	  abort();
	  break;
	case 'l':
	  ++ l;
	  break;
	case 'c': case 'd': case 'i': case 'u': case 'o': case 'x': case 'X':
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
	case 'g': case 'G': case 'f': case 'F': case 'a': case 'A':
	  switch ( l ) {
	  case 0:
	    fprintf(FP, FMT, va_arg(vap, double));
	    break;
	  case 1:
	    fprintf(FP, FMT, va_arg(vap, long double));
	    break;
	  default:
	    abort();
	    break;
	  }
	  done = 1; break;
	case 'p':
	  fprintf(FP, FMT, va_arg(vap, void*));
	  done = 1; break;
	case 's': case 'b':
	  fprintf(FP, FMT, va_arg(vap, char*));
	  done = 1; break;
	case 'T':
	  tort_send(tort__s(_inspect), va_arg(vap, tort_v), io);
	  done = 1; break;
	case 'O':
	  tort_send(tort_s(lisp_write), va_arg(vap, tort_v), io);
	  done = 1; break;
	default:
	  break;
	}
      } while ( ! done );
    }
  }

#undef vap
  return io;
}

static int string_read(void *str, char *buf, int size)
{
  return tort_I(tort_send(tort__s(__read), (tort_v) str, (void*) buf, (size_t) size));
}

static int string_write(void *str, const char *buf, int size)
{
  tort_send(tort__s(__write), (tort_v) str, (void*) buf, (size_t) size);
  return size;
}

static fpos_t string_seek(void *str, fpos_t pos, int dir)
{
  return 0;
}

static int string_close(void *str)
{
  return 0;
}

tort_v _tort_m_string____printfsv(tort_tp tort_string *str, const char *fmt, va_list *vapp)
{
  FILE *fp = funopen((void*) str,
		     string_read,
		     string_write,
		     string_seek,
		     string_close);
  tort_io *io = tort_send(tort__s(__create), tort__mt(io), fp);
  tort_v result = tort_send(tort__s(__printfsv), io, fmt, vapp);
  fclose(fp);
  return result;
}

tort_v _tort_m_nil____printfsv(tort_tp tort_string *str, const char *fmt, va_list *vapp)
{
  str = tort_string_new(0, 0);
  tort_send(tort__s(__printfsv), str, fmt, vapp);
  return str;
}

tort_v _tort_m_object____printfsv(tort_tp tort_v io, const char *fmt, va_list *vapp)
{
  return tort_send(tort__s(_printfv), io, fmt, strchr(fmt, 0), vapp);
}

tort_v _tort_m_object____printfs(tort_tp tort_v io, const char *fmt, ...)
{
  va_list vap;
  va_start(vap, fmt);
  io = tort_send(tort__s(__printfsv), io, fmt, &vap);
  va_end(vap);
  return io;
}

tort_v tort_runtime_initialize_printf()
{
  return tort__mt(io);
}
