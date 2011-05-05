#include "tort/tort.h"

#include <stdio.h>
#include <assert.h>

int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v v, o, c;
  int i;
  extern size_t _tort_gc_finalize_count, _tort_io_open_count, _tort_io_close_count;

  assert(_tort_gc_finalize_count == 0);

  tort_runtime_create();

  io = tort_stdout;

  for ( i = 0; i < 2; i ++ ) {
    printf("\nread up to 64 chars from popen(\"echo 12345\", \"r\") => ");
    v = tort_string_new_cstr("echo 12345");
    c = tort_string_new_cstr("r");
    o = tort_send(tort__s(__create), tort__mt(io), 0);
    o = tort_send(tort__s(popen), o, v, c);
    v = tort_send(tort__s(read), o, tort_i(64));
    tort_inspect(io, v);
    printf("\n  (eof o) => ");
    tort_inspect(io, c = tort_send(tort__s(eof), o));
    printf("\n  (size v) => ");
    tort_inspect(io, c = tort_send(tort__s(size), v));
    printf("\n  (alloc_size v) => ");
    tort_inspect(io, c = tort_send(tort__s(alloc_size), v));
    printf("\n  (_alloc_size v) => ");
    tort_inspect(io, c = tort_send(tort__s(_alloc_size), v));
    printf("\n\n");
  }

  printf("\nDONE\n");

  io = o = v = c = 0;
  i = 0;

  tort_gc_collect();
  if ( _tort_gc_mode ) {
    assert(_tort_gc_finalize_count >= 2);
    assert(_tort_io_open_count >= 2);
    assert(_tort_io_close_count >= 2);
  }

  return 0;
}

