#include "tort/core.h"
#include <printf.h>
#include <libio.h>


/********************************************************************/


#define IO tort_ref(tort_io, rcvr)
#define FP IO->fp


/* Use hidden slot in FILE* to point back to a tort_io object. */
#define FP_TORT_OBJ(fp) *(((tort_v*)(((struct _IO_FILE *) fp) + 1)) - 4)
// #define FP_TORT_OBJ(fp) *((tort_v*)(&((struct _IO_FILE *) fp)->_old_offset))
// #define FP_TORT_OBJ(fp) (* (tort_v *) ((struct _IO_FILE *) (fp))->_unused2 )


/********************************************************************/


size_t _tort_io_open_count, _tort_io_close_count;

tort_v _tort_io___create(tort_thread_param tort_v rcvr, FILE *fp)
{
  rcvr = _tort_allocate(tort_thread_arg rcvr, sizeof(tort_io), _tort->_mt_io);
  FP = fp;
  if ( FP ) {
    FP_TORT_OBJ(FP) = rcvr;
  }
  IO->name = IO->mode = tort_nil;
  IO->flags = 0;
  return rcvr;
}


tort_v _tort_io_open(tort_thread_param tort_v rcvr, tort_v name, tort_v mode)
{
  if ( (FP = fopen(tort_string_data(name), tort_string_data(mode))) ){
    ++ _tort_io_open_count;
    FP_TORT_OBJ(FP) = rcvr;
    IO->name = name;
    IO->mode = mode;
    IO->flags = 1;
    tort_send(tort__s(__register_finalizer), rcvr);
  }
  return rcvr;
}


tort_v _tort_io_popen(tort_thread_param tort_v rcvr, tort_v name, tort_v mode)
{
  if ( (FP = FP = popen(tort_string_data(name), tort_string_data(mode))) ) {
    ++ _tort_io_open_count;
    FP_TORT_OBJ(FP) = rcvr;
    IO->name = name;
    IO->mode = mode;
    IO->flags = 3;
    tort_send(tort__s(__register_finalizer), rcvr);
  }
  return rcvr;
}


tort_v _tort_io_close(tort_thread_param tort_v rcvr)
{
  if ( FP ) {
    // fprintf(stderr, "\n  _tort_io_close @%p\n", (void*) rcvr);
    if ( IO->flags & 2 ) {
      pclose(FP);
    } else {
      fclose(FP);
    }
    ++ _tort_io_close_count;
    FP_TORT_OBJ(FP) = 0;
    FP = 0;
  }
  return rcvr;
}


tort_v _tort_io___write(tort_thread_param tort_v rcvr, tort_v str)
{
  size_t size =
    fwrite(tort_string_data(str), 
	   sizeof(tort_string_data(str)[0]),
	   tort_string_size(str), 
	   FP);
  return tort_i(size);
}


tort_v _tort_io_flush(tort_thread_param tort_v rcvr)
{
  if ( FP ) {
    fflush(FP);
  }
  return rcvr;
}


tort_v _tort_io_printf(tort_thread_param tort_v rcvr, const char *fmt, ...)
{
  va_list vap;
  va_start(vap, fmt);
  vfprintf(FP, fmt, vap);
  va_end(vap);
  return rcvr;
}


tort_v _tort_io_read(tort_thread_param tort_v rcvr, tort_v buf)
{
  int count;

  if ( tort_taggedQ(buf) ) {
    buf = tort_string_new(0, tort_I(buf));
  }

  count = 
    fread(tort_string_data(buf), 
	  sizeof(tort_string_data(buf)[0]), 
	  tort_string_alloc_size(buf), 
	  FP);

  tort_string_size(buf) = count;
  tort_string_data(buf)[count] = '\0';

  return buf;
}


tort_v _tort_io_eof(tort_thread_param tort_v rcvr)
{
  return tort_i(FP ? feof(FP) : -1);
}


tort_v _tort_io_error(tort_thread_param tort_v rcvr)
{
  return tort_i(FP ? ferror(FP) : -1);
}


tort_v _tort_io___finalize(tort_thread_param tort_v rcvr)
{
  if ( FP && (IO->flags & 1) ) {
    // fprintf(stderr, "\n  _tort_io___finalize @%p\n", (void*) rcvr);
    IO->flags &= ~1;
    _tort_io_close(tort_thread_arg rcvr);
  }
  return tort_nil;
}


/********************************************************************/


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


/********************************************************************/


void tort_runtime_initialize_io()
{
  _tort->_s___create = tort_symbol_make("__create");
  _tort->_s_open = tort_symbol_make("open");
  _tort->_s_popen = tort_symbol_make("popen");
  _tort->_s_close = tort_symbol_make("close");
  _tort->_s_read = tort_symbol_make("read");
  _tort->_s___write = tort_symbol_make("__write");
  _tort->_s_write = tort_symbol_make("write");
  _tort->_s_printf = tort_symbol_make("printf");
  _tort->_s_eof = tort_symbol_make("eof");
  _tort->_s_error = tort_symbol_make("error");

  _tort->_mt_io = tort_mtable_create(_tort->_mt_object);

  tort_add_method(_tort->_mt_io, "__create", _tort_io___create); 
  tort_add_method(_tort->_mt_io, "open", _tort_io_open);
  tort_add_method(_tort->_mt_io, "popen", _tort_io_popen);
  tort_add_method(_tort->_mt_io, "close", _tort_io_close);
  tort_add_method(_tort->_mt_io, "read", _tort_io_read);
  tort_add_method(_tort->_mt_io, "__write", _tort_io___write);
  tort_add_method(_tort->_mt_io, "printf", _tort_io_printf);
  tort_add_method(_tort->_mt_io, "eof", _tort_io_eof);
  tort_add_method(_tort->_mt_io, "error", _tort_io_error);
  tort_add_method(_tort->_mt_io, "flush", _tort_io_flush);
  tort_add_method(_tort->_mt_io, "__finalize", _tort_io___finalize);

  _tort->_io_stdin  = _tort_io___create(0, 0, stdin);
  _tort->_io_stdout = _tort_io___create(0, 0, stdout);
  _tort->_io_stderr = _tort_io___create(0, 0, stderr);

  _tort->_mt_eos    = tort_mtable_create(_tort->_mt_object);
  _tort->_io_eos    = tort_allocate(0, 0, sizeof(tort_object), _tort->_mt_eos);

  /* Register the print functions for tort_v.  */
  register_printf_specifier('T', 
			    _tort_printf_object,
			    _tort_printf_extension_arginfo);

  register_printf_specifier('O', 
			    _tort_printf_object_lisp,
			    _tort_printf_extension_arginfo);

}

