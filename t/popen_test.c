#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_v io;
  tort_v v, o, c;
  int i;

  tort_runtime_create();

  io = tort_stdout;

  for ( i = 0; i < 2; i ++  ) {
  printf("\nread up to 64 chars from popen(\"echo 12345\", \"r\") => ");
  v = tort_string_new_cstr("echo 12345");
  c = tort_string_new_cstr("r");
  o = tort_send(tort__s(create), tort_stdin);
  o = tort_send(tort__s(popen), o, v, c);
  v = tort_send(tort__s(read), o, tort_i(64));
  tort_write(io, v);
  printf("\n  (eof o) => ");
  tort_write(io, c = tort_send(tort__s(eof), o));
  tort_send(tort__s(close), o);
  printf("\n  (size v) => ");
  tort_write(io, c = tort_send(tort__s(size), v));
  printf("\n  (alloc_size v) => ");
  tort_write(io, c = tort_send(tort__s(alloc_size), v));
  printf("\n\n");
  }

  printf("\nDONE\n");

  return 0;
}

