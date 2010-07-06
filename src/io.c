#include "tort/tort.h"

#include <stdio.h>
#include <printf.h>
#include <libio.h>


#define IO tort_ref(tort_io, rcvr)
#define FP IO->fp
#define FP_TORT_OBJ(fp) *((tort_val*)&((struct _IO_FILE *)fp)->_unused2)

static
tort_val _tort_io_create(tort_val message, tort_val rcvr, FILE *fp)
{
  rcvr = tort_allocate(message, rcvr, sizeof(tort_io), _tort->_mt_io);
  FP = fp;
  FP_TORT_OBJ(fp) = rcvr;
  return rcvr;
}


static
tort_val _tort_io_open(tort_val message, tort_val rcvr, tort_val name, tort_val mode)
{
  FP = fopen(tort_string_data(name), tort_string_data(mode));
  IO->mode = mode;
  return rcvr;
}


static
tort_val _tort_io_popen(tort_val message, tort_val rcvr, tort_val name, tort_val mode)
{
  FP = popen(tort_string_data(name), tort_string_data(mode));
  IO->mode = mode;
  IO->popen = 1;
  return rcvr;
}


static
tort_val _tort_io_close(tort_val message, tort_val rcvr)
{
  if ( FP ) {
    if ( IO->popen ) {
      pclose(FP);
    } else {
      fclose(FP);
    }
    FP = 0;
  }
  return rcvr;
}

static
tort_val _tort_io_write(tort_val message, tort_val rcvr, tort_val buf)
{
  fwrite(tort_string_data(buf), 
	 sizeof(tort_string_data(buf)[0]),
	 tort_string_size(buf), FP);
  return rcvr;
}

static
tort_val _tort_io_printf(tort_val message, tort_val rcvr, const char *fmt, ...)
{
  va_list vap;
  va_start(vap, fmt);
  vfprintf(FP, fmt, vap);
  va_end(vap);
  return rcvr;
}


static
tort_val _tort_io_read(tort_val message, tort_val rcvr, tort_val buf)
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


static
tort_val _tort_io_eof(tort_val message, tort_val rcvr)
{
  return tort_i(FP ? feof(FP) : -1);
}


static
tort_val _tort_io_error(tort_val message, tort_val rcvr)
{
  return tort_i(FP ? ferror(FP) : -1);
}


static
int 
_tort_printf_object (FILE *stream,
		     __const struct printf_info *info,
		     __const void *__const *args
		     )
{
  tort_val v;
  int len = 128; /* ??? */

  /* Format the output into a string.  */
  v = *(tort_val*) args[0];
  v = tort_write(FP_TORT_OBJ(stream), v);

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
    size[0] = sizeof(tort_val);
  }
  return 1;
}


void tort_runtime_initialize_io()
{
  _tort->_s_create = tort_symbol_make("create");
  _tort->_s_open = tort_symbol_make("open");
  _tort->_s_popen = tort_symbol_make("popen");
  _tort->_s_close = tort_symbol_make("close");
  _tort->_s_read = tort_symbol_make("read");
  _tort->_s_write = tort_symbol_make("write");
  _tort->_s_printf = tort_symbol_make("printf");
  _tort->_s_eof = tort_symbol_make("eof");
  _tort->_s_error = tort_symbol_make("error");

  _tort->_mt_io = tort_map_create();
  tort_add_method(_tort->_mt_io, "create", _tort_io_create); 
  tort_add_method(_tort->_mt_io, "open", _tort_io_open);
  tort_add_method(_tort->_mt_io, "popen", _tort_io_popen);
  tort_add_method(_tort->_mt_io, "close", _tort_io_close);
  tort_add_method(_tort->_mt_io, "read", _tort_io_read);
  tort_add_method(_tort->_mt_io, "write", _tort_io_write);
  tort_add_method(_tort->_mt_io, "printf", _tort_io_printf);
  tort_add_method(_tort->_mt_io, "eof", _tort_io_eof);
  tort_add_method(_tort->_mt_io, "error", _tort_io_error);

  _tort->_io_stdin  = _tort_io_create(0, 0, stdin);
  _tort->_io_stdout = _tort_io_create(0, 0, stdout);
  _tort->_io_stderr = _tort_io_create(0, 0, stderr);

 /* Register the print function for widgets.  */
  register_printf_specifier('T', 
			    _tort_printf_object,
			    _tort_printf_extension_arginfo); /* No arginfo.   */

}

