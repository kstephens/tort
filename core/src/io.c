#include "tort/core.h"
#include "sys/stat.h"

#define IO rcvr
#define FP IO->fp

#ifdef __linux__
#include <printf.h>
#include <libio.h>
#endif

tort_v _tort_M_io____stat(tort_tp tort_mtable *mtable, tort_v name)
{
  int result;
  struct stat st;
  bzero(&st, sizeof(st));
  if ( (result = stat(tort_string_data(name), &st)) == 0 ) {
    tort_v map = tort_send(tort__s(new), tort__mt(map));
#define ST(T,E,N) tort_send(tort__s(set), map, tort_s(N), tort_i((tort_vi) st.E))
    ST(dev_t           ,st_dev,  dev);         /* [XSI] ID of device containing file */
    ST(ino_t           ,st_ino,  ino);         /* [XSI] File serial number */
    ST(mode_t          ,st_mode, mode);        /* [XSI] Mode of file (see below) */
    ST(nlink_t         ,st_nlink,nlink);       /* [XSI] Number of hard links */
    ST(uid_t           ,st_uid,  uid);         /* [XSI] User ID of the file */
    ST(gid_t           ,st_gid,  gid);         /* [XSI] Group ID of the file */
    ST(dev_t           ,st_rdev, rdev);        /* [XSI] Device ID */
#if !defined(_POSIX_C_SOURCE) || defined(_DARWIN_C_SOURCE)
    ST(__darwin_time_t ,st_atimespec.tv_sec,atime);  /* time of last access */
    ST(__darwin_time_t ,st_mtimespec.tv_sec,mtime);  /* time of last data modification */
    ST(__darwin_time_t ,st_ctimespec.tv_sec,ctime);  /* time of last status change */
#else
    ST(time_t          ,st_atime,    atime);       /* [XSI] Time of last access */
#ifdef APPLE
    ST(long            ,st_atimensec,atimensec);   /* nsec of last access */
#endif
    ST(time_t          ,st_mtime,    mtime);       /* [XSI] Last data modification time */
#ifdef APPLE
    ST(long            ,st_mtimensec,mtimensec);   /* last data modification nsec */
#endif
    ST(time_t          ,st_ctime,    ctime);       /* [XSI] Time of last status change */
#ifdef APPLE
    ST(long            ,st_ctimensec,ctimensec);   /* nsec of last status change */
#endif
#endif
    ST(off_t           ,st_size,    size);        /* [XSI] file size, in bytes */
    ST(blkcnt_t        ,st_blocks,  blocks);      /* [XSI] blocks allocated for file */
    ST(blksize_t       ,st_blksize, blksize);     /* [XSI] optimal blocksize for I/O */
#if 0
        __uint32_t      st_flags;       /* user defined flags for file */
        __uint32_t      st_gen;         /* file generation number */
        __int32_t       st_lspare;      /* RESERVED: DO NOT USE! */
        __int64_t       st_qspare[2];   /* RESERVED: DO NOT USE! */
#endif
#undef ST
	return map;
  }
  return tort_nil;
}

tort_ACCESSOR(io,voidP,fp);
tort_SLOT(io,tort_v,name);
tort_SLOT(io,tort_v,mode);
tort_ACCESSOR(io,int,flags);
tort_SLOT(io,tort_v,data);

size_t _tort_io_open_count, _tort_io_close_count;

tort_v _tort_M_io____new(tort_tp tort_mtable *mtable, FILE *fp)
{
  tort_io *rcvr = _tort_allocate(tort_ta mtable, sizeof(tort_io));
  FP = fp;
  IO->name = IO->mode = tort_nil;
  IO->flags = 0;
  IO->data = tort_map_new();
  return rcvr;
}

tort_v _tort_M_io__new(tort_tp tort_mtable *mtable)
{
  return _tort_M_io____new(tort_ta mtable, 0);
}

tort_v _tort_m_io__open(tort_tp tort_io *rcvr, tort_v name, tort_v mode)
{
  tort_send(tort__s(close), rcvr);
  if ( (FP = fopen(tort_string_data(name), tort_string_data(mode))) ){
    ++ _tort_io_open_count;
    IO->name = name;
    IO->mode = mode;
    IO->flags |= 1;
    tort_send(tort__s(__register_finalizer), rcvr);
    return rcvr;
  }
  return tort_nil;
}

tort_v _tort_m_io__popen(tort_tp tort_io *rcvr, tort_v name, tort_v mode)
{
  tort_send(tort__s(close), rcvr);
  if ( (FP = popen(tort_string_data(name), tort_string_data(mode))) ) {
    ++ _tort_io_open_count;
    IO->name = name;
    IO->mode = mode;
    IO->flags |= 3;
    tort_send(tort__s(__register_finalizer), rcvr);
    return rcvr;
  }
  return tort_nil;
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
  if ( count >= 0 ) {
    buf->size = count;
    buf->data[count] = '\0';
    return buf;
  } else {
    buf->size = 0;
    buf->data[0] = '\0';
  }
  return tort_false;
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

// extern tort_v _tort_m_initializer__printf(tort_tp tort_v init);
tort_v _tort_m_initializer__io(tort_tp tort_v init)
{
  tort_stdin  = _tort_M_io____new(tort_ta tort__mt(io), stdin);
  tort_stdout = _tort_M_io____new(tort_ta tort__mt(io), stdout);
  tort_stderr = _tort_M_io____new(tort_ta tort__mt(io), stderr);

  tort_send(tort__s(set), tort_(root), tort__s(io_stdin), tort_stdin);
  tort_send(tort__s(set), tort_(root), tort__s(io_stdout), tort_stdout);
  tort_send(tort__s(set), tort_(root), tort__s(io_stderr), tort_stderr);

  tort_eos    = tort_allocate(tort__mt(eos), sizeof(tort_object));

  tort_send(tort__s(go), init, tort__s(printf));
  return init;
}

