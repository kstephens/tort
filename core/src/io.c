#include "tort/core.h"

#define IO rcvr
#define FP IO->fp

#ifdef __LINUX__
#include <printf.h>
#include <libio.h>
/* Use hidden slot in FILE* to point back to a tort_io object. */
#define FP_TORT_OBJ(fp) *(((tort_v*)(((struct _IO_FILE *) fp) + 1)) - 4)
#endif

#ifdef __APPLE__
/* Use hidden slot in FILE* to point back to a tort_io object. */
#define FP_TORT_OBJ(fp) *(((tort_v*)(((struct __sFILE *) fp) + 1)) - 1)
#endif

size_t _tort_io_open_count, _tort_io_close_count;

tort_v _tort_M_io____create(tort_tp tort_mtable *mtable, FILE *fp)
{
  tort_io *rcvr = _tort_allocate(tort_ta mtable, sizeof(tort_io));
  FP = fp;
  if ( FP ) {
    // FP_TORT_OBJ(FP) = rcvr;
  }
  IO->name = IO->mode = tort_nil;
  IO->flags = 0;
  return rcvr;
}

tort_v _tort_M_io__create(tort_tp tort_mtable *mtable, FILE *fp)
{
  return _tort_M_io____create(tort_ta mtable, 0);
}

tort_v _tort_m_io__open(tort_tp tort_io *rcvr, tort_v name, tort_v mode)
{
  if ( (FP = fopen(tort_string_data(name), tort_string_data(mode))) ){
    ++ _tort_io_open_count;
    // FP_TORT_OBJ(FP) = rcvr;
    IO->name = name;
    IO->mode = mode;
    IO->flags |= 1;
    tort_send(tort__s(__register_finalizer), rcvr);
  }
  return rcvr;
}

tort_v _tort_m_io__popen(tort_tp tort_io *rcvr, tort_v name, tort_v mode)
{
  if ( (FP = popen(tort_string_data(name), tort_string_data(mode))) ) {
    ++ _tort_io_open_count;
    // FP_TORT_OBJ(FP) = rcvr;
    IO->name = name;
    IO->mode = mode;
    IO->flags |= 3;
    tort_send(tort__s(__register_finalizer), rcvr);
  }
  return rcvr;
}

tort_v _tort_m_io__close(tort_tp tort_io *rcvr)
{
  if ( FP ) {
    // fprintf(stderr, "\n  _tort_io_close @%p\n", (void*) rcvr);
    if ( IO->flags & 2 ) {
      IO->flags &= ~ 2;
      pclose(FP);
    } else {
      fclose(FP);
    }
    ++ _tort_io_close_count;
    // FP_TORT_OBJ(FP) = 0;
    FP = 0;
  }
  return rcvr;
}

tort_v _tort_m_io____write(tort_tp tort_io *rcvr, void *data, size_t size)
{
  size = fwrite(data, 1, size, FP);
  return tort_i(size);
}

tort_v _tort_m_io___write(tort_tp tort_io *rcvr, tort_v str)
{
  return_tort_send(tort__s(__write), rcvr, tort_string_data(str), tort_string_size(str));
}

tort_v _tort_m_io__flush(tort_tp tort_io *rcvr)
{
  if ( FP ) fflush(FP);
  return rcvr;
}

tort_v _tort_m_io__read(tort_tp tort_io *rcvr, tort_string *buf)
{
  int count;

  if ( tort_taggedQ(buf) )
    buf = tort_string_new(0, tort_I(buf));

  count = fread(buf->data, sizeof(buf->data[0]), buf->alloc_size - 1, FP);

  buf->size = count;
  buf->data[count] = '\0';

  return buf;
}

tort_v _tort_m_io__openQ(tort_tp tort_io *rcvr)
{
  return FP ? tort_true : tort_false;
}

tort_v _tort_m_io__eof(tort_tp tort_io *rcvr)
{
  return tort_i(FP ? feof(FP) : -1);
}

tort_v _tort_m_io__error(tort_tp tort_io *rcvr)
{
  return tort_i(FP ? ferror(FP) : -1);
}

tort_v _tort_m_io____finalize(tort_tp tort_io *rcvr)
{
  if ( FP && (IO->flags & 1) ) {
    // fprintf(stderr, "\n  _tort_io___finalize @%p\n", (void*) rcvr);
    IO->flags &= ~ 1;
    _tort_m_io__close(tort_ta rcvr);
  }
  return tort_nil;
}

tort_v _tort_m_string___write(tort_tp tort_io *rcvr, tort_v str)
{
  return_tort_send(tort__s(_append), rcvr, tort_string_data(str), tort_string_size(str));
}

tort_v _tort_m_string____write(tort_tp tort_string *rcvr, void *data, size_t size)
{
  tort_send(tort__s(_append), rcvr, data, size);
  return tort_i(size);
}

tort_v _tort_m_string__flush(tort_tp tort_string *rcvr)
{
  return rcvr;
}

tort_v _tort_m_string__close(tort_tp tort_string *rcvr)
{
  return rcvr;
}

extern tort_v tort_runtime_initialize_printf();
tort_v tort_runtime_initialize_io()
{
  tort_stdin  = _tort_M_io____create(tort_ta tort__mt(io), stdin);
  tort_stdout = _tort_M_io____create(tort_ta tort__mt(io), stdout);
  tort_stderr = _tort_M_io____create(tort_ta tort__mt(io), stderr);

  tort_send(tort__s(set), tort_(root), tort_s(stdin), tort_stdin);
  tort_send(tort__s(set), tort_(root), tort_s(stdout), tort_stdout);
  tort_send(tort__s(set), tort_(root), tort_s(stderr), tort_stderr);

  tort_eos    = tort_allocate(tort__mt(eos), sizeof(tort_object));

  tort_runtime_initialize_printf();

  return tort__mt(io);
}

