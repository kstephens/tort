#include "tort/tort.h"

#include <stdio.h>


#define IO tort_ref(tort_io, rcvr)
#define FP IO->fp

static
tort_val _tort_io_create(tort_val message, tort_val rcvr, FILE *fp)
{
  rcvr = tort_allocate(0, 0, sizeof(tort_io), _tort->_mt_io);
  FP = fp;
  return rcvr;
}

static
tort_val _tort_io_open(tort_val message, tort_val rcvr, tort_val name, tort_val mode)
{
  FP = fopen(tort_string_data(name), tort_string_data(mode));
  return rcvr;
}

static
tort_val _tort_io_close(tort_val message, tort_val rcvr)
{
  fclose(FP);
  FP = 0;
  return rcvr;
}

static
tort_val _tort_io_write(tort_val message, tort_val rcvr, tort_val buf)
{
  fwrite(tort_string_data(buf), tort_string_size(buf), 1, FP);
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


void tort_runtime_initialize_io()
{
  _tort->_s_create = tort_symbol_make("create");
  _tort->_s_open = tort_symbol_make("open");
  _tort->_s_close = tort_symbol_make("close");
  _tort->_s_write = tort_symbol_make("write");
  _tort->_s_printf = tort_symbol_make("printf");

  _tort->_mt_io = tort_map_create();
  tort_add_method(_tort->_mt_io, "create", _tort_io_create); 
  tort_add_method(_tort->_mt_io, "open", _tort_io_open);
  tort_add_method(_tort->_mt_io, "close", _tort_io_close);
  tort_add_method(_tort->_mt_io, "write", _tort_io_write);
  tort_add_method(_tort->_mt_io, "printf", _tort_io_printf);

  _tort->_io_stdin  = _tort_io_create(0, 0, stdin);
  _tort->_io_stdout = _tort_io_create(0, 0, stdout);
  _tort->_io_stderr = _tort_io_create(0, 0, stderr);
}

