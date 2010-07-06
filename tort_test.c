#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_val io;
  tort_val v, o, b, c;
  int i;

  tort_runtime_create();

  io = tort_stdout;

  printf("\n  nil => ");
  tort_write(io, tort_nil);

  printf("\n  123 => ");
  tort_write(io, tort_i(123));

  printf("\n  _mt_map => ");
  tort_write(io, _tort->_mt_map);
  
  printf("\n  (v = \"new\") =>");
  tort_write(io, v = tort_string_new_cstr("new"));

  printf("\n  (get symbols \"new\") => ");
  tort_write(io, tort_send(tort__s(get), _tort->symbols, v = tort_string_new_cstr("new")));

  printf("\n  (clone v) => ");
  tort_write(io, v = tort_send(tort__s(clone), v));

  printf("\n  (size v) => ");
  tort_write(io, c = tort_send(tort__s(size), v));

  printf("\n  (alloc_size v) => ");
  tort_write(io, c = tort_send(tort__s(alloc_size), v));

  printf("\n  (get v 2) => ");
  tort_write(io, c = tort_send(tort__s(get), v, tort_i(1)));

  printf("\n  (set v 2 +1) => ");
  tort_write(io, tort_send(tort__s(set), v, tort_i(1), tort_i(tort_I(c) + 1)));

  printf("\n  symbols => ");
  tort_write(io, _tort->symbols);

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

  o = tort_map_create();
  tort_printf(io, "o => %T\n", o);
  v = tort_send(tort__s(size), o);
  tort_printf(io, "(size o) => %T\n", v);

  tort_send(tort_s(set), o, tort_i(1), tort_i(2));
  tort_printf(io, "o => %T\n", o);
  v = tort_send(tort__s(size), o);
  tort_printf(io, "(size o) => %T\n", v); 

  tort_send(tort_s(set), o, tort_i(1), tort_i(3));
  tort_printf(io, "o => %T\n", o);
  v = tort_send(tort__s(size), o);
  tort_printf(io, "(size o) => %T\n", v);

  tort_send(tort_s(set), o, tort_i(3), tort_i(4));
  tort_printf(io, "o => %T\n", o);
  v = tort_send(tort__s(size), o);
  tort_printf(io, "(size o) => %T\n", v);

  tort_send(tort_s(delete), o, tort_i(1));
  tort_printf(io, "o => %T\n", o);
  tort_printf(io, "(size o) => %T\n", tort_send(tort__s(size), o));

#if 0
  printf("\nread 1 char from stdin: ");
  v = tort_send(tort__s(read), tort_stdin, tort_i(1));
  tort_write(io, v);
#endif

  printf("\n");

  v = tort_send(tort_s(__message), v);
  tort_printf(io, "Some object ==> ( %T ) <== is in here!\n", tort_nil);

  v = tort_vector_new(0, 10);
  
  b = 
    tort_block_(tort_val obj) {
    return tort_printf(io, "  in each %p[%d] => %T\n", 
		       (void*) v, 
		       ++ i, 
		       obj);
  }
  tort_block_end();
  
  i = 0;
  tort_send(tort_s(each), v, b);

  b = 
    tort_block_(tort_val obj) {
    return tort_i(i ++);
  }
  tort_block_end();
  
  i = 0;
  v = tort_send(tort_s(map), v, b);
  tort_printf(io, "v = %T\n", v);

  tort_printf(io, "v as lisp object = %O\n", v);

#if 0
  // segfault
  printf("\nread lisp object from popen(\"echo 12345\", \"r\") => ");
  v = tort_string_new_cstr("echo 12345");
  c = tort_string_new_cstr("r");
  o = tort_send(tort__s(create), tort_stdin);
  o = tort_send(tort__s(popen), o, v, c);
  v = tort_send(tort__s(lisp_read), o);
  tort_send(tort__s(close), o);
  tort_printf(io, "(read o) => %O\n", v);
#endif

  tort_printf(io, "read lisp object from stdin: ");
  v = tort_send(tort__s(lisp_read), tort_stdin);
  tort_printf(io, "(read o) => %O\n", v);

  tort_send(tort_s(__debugger), v);

  return 0;
}

