#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>
#include <assert.h>
#include <fcntl.h> /* open() */
#include <unistd.h> /* close() */


int main(int argc, char **argv)
{
  tort_v io;
  tort_v v, s, o, p, m;
  int i;
  extern size_t _tort_gc_finalize_count, _tort_io_open_count, _tort_io_close_count;

  assert(_tort_gc_finalize_count == 0);

  tort_runtime_create();

  io = tort_stdout;

  for ( i = 0; i < 100; i ++ ) {
    p = tort_string_new_cstr("/dev/null");
    m = tort_string_new_cstr("r");
    v = tort_send(tort__s(create), tort_stdin);
    o = tort_send(tort__s(open), v, p, m);
    tort_printf(io, "\n  o => %T", o);
    s = tort_send(tort__s(read), o, tort_i(64));
    tort_inspect(io, s);
    printf("\n  (eof o) => ");
    tort_inspect(io, tort_send(tort__s(eof), o));
    printf("\n  (size v) => ");
    tort_inspect(io, tort_send(tort__s(size), s));
    printf("\n  (alloc_size v) => ");
    tort_inspect(io, tort_send(tort__s(alloc_size), s));
    printf("\n\n");

    tort_gc_collect();
  }

  tort_gc_collect();

  assert(_tort_gc_finalize_count >= 100);
  assert(_tort_io_open_count >= 100);
  assert(_tort_io_close_count >= 100);

  i = -1;
  assert(i = open("/dev/null", 0) < 10);
  if ( i != -1 ) close(i);

  printf("\nDONE\n");

  io = v = s = o = p = m = 0;
  i = 0;

  return 0;
}

