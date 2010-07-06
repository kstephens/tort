#include "tort/tort.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_val io;
  tort_val v, o, c;

  tort_runtime_create();

  io = tort_stdout;

  printf("\n  nil => ");
  tort_write(tort_nil, io);

  printf("\n  123 => ");
  tort_write(tort_i(123), io);

  printf("\n  _mt_map => ");
  tort_write(_tort->_mt_map, io);
  
  printf("\n  (v = \"new\") =>");
  tort_write(v = tort_string_new_cstr("new"), io);

  printf("\n  (get symbols \"new\") => ");
  tort_write(tort_send(tort_s(get), _tort->symbols, v = tort_string_new_cstr("new")), io);

  printf("\n  (clone v) => ");
  tort_write(v = tort_send(tort_s(clone), v), io);

  printf("\n  (size v) => ");
  tort_write(c = tort_send(tort_s(size), v), io);

  printf("\n  (alloc_size v) => ");
  tort_write(c = tort_send(tort_s(alloc_size), v), io);

  printf("\n  (get v 2) => ");
  tort_write(c = tort_send(tort_s(get), v, tort_i(1)), io);

  printf("\n  (set v 2 +1) => ");
  tort_write(tort_send(tort_s(set), v, tort_i(1), tort_i(tort_I(c) + 1)), io);

  printf("\n  symbols => ");
  tort_write(_tort->symbols, io);

  printf("\nread 1 char from popen(\"echo 12345\", \"r\") => ");
  v = tort_string_new_cstr("echo 12345");
  c = tort_string_new_cstr("r");
  o = tort_send(tort_s(create), tort_stdin);
  o = tort_send(tort_s(popen), o, v, c);
  v = tort_send(tort_s(read), o, tort_i(64));
  tort_write(v, io);
  printf("\n  (eof o) => ");
  tort_write(c = tort_send(tort_s(eof), o), io);
  tort_send(tort_s(close), o);
  printf("\n  (size v) => ");
  tort_write(c = tort_send(tort_s(size), v), io);
  printf("\n  (alloc_size v) => ");
  tort_write(c = tort_send(tort_s(alloc_size), v), io);

#if 0
  printf("\nread 1 char from stdin: ");
  v = tort_send(tort_s(read), tort_stdin, tort_i(1));
  tort_write(v, io);
#endif

  printf("\n");


  return 0;
}

