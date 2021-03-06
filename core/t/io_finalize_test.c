#include "tort/tort.h"

#include <stdio.h>
#include <assert.h>
#include <fcntl.h> /* open() */
#include <unistd.h> /* close() */


int main(int argc, char **argv, char **environ)
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

    tort_printf(io, "  (open v %T %T)\n", p, m);

    v = tort_send(tort__s(new), tort__mt(io));
    tort_printf(io, "  v => %T\n", v);

    o = tort_send(tort__s(open), v, p, m);
    tort_printf(io, "  o => %T\n", o);

    s = tort_send(tort__s(read), o, tort_i(64));
    tort_printf(io, "  s => %T\n", s);

    tort_printf(io, "  (eof o) => %T\n", tort_send(tort__s(eof), o));
    tort_printf(io, "  (size v) => %T\n", tort_send(tort__s(size), s));

    tort_printf(io, "\n\n");

    if ( i % 4 == 0 ) {
      tort_gc_collect();
    }
  }

  p = m = v = o = s = tort_nil;
  tort_gc_collect();

  if ( _tort_gc_mode ) {
    assert(_tort_gc_finalize_count >= 100);
    assert(_tort_io_open_count >= 100);
    assert(_tort_io_close_count >= 100);
  }

  i = -1;
  assert(i = open("/dev/null", 0) < 10);
  if ( i != -1 ) close(i);

  printf("\nDONE\n");

  io = v = s = o = p = m = 0;
  i = 0;

  return 0;
}

