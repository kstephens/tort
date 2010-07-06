#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_val io;
  tort_val v, b;
  int i;

  tort_runtime_create();

  io = tort_stdout;

  v = tort_vector_new(0, 10);

  tort_printf(io, "v => %T\n", v);
  tort_printf(io, "(size v) => %T\n", tort_send(tort__s(size), v));

  tort_printf(io, "v[1] = %T\n", tort_send(tort__s(get), v, tort_i(1)));

  tort_send(tort__s(set), v, tort_i(1), tort_i(1));
  tort_printf(io, "v => %T\n", v);

  tort_printf(io, "v[1] => %T\n", tort_send(tort__s(get), v, tort_i(1)));
  tort_printf(io, "v => %T\n", v);

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

  printf("\nDONE\n");

  return 0;
}

