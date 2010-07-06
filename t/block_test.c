#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_v io;
  tort_v v, b;
  int i;

  tort_runtime_create();

  io = tort_stdout;

  v = tort_vector_new(0, 10);
  
  b = 
    tort_block_(tort_v obj) {
    return tort_printf(io, "  in each %p[%d] => %T\n", 
		       (void*) v, 
		       ++ i, 
		       obj);
  }
  tort_block_end();
  
  i = 0;
  tort_send(tort_s(each), v, b);

  b = 
    tort_block_(tort_v obj) {
    return tort_i(i ++);
  }
  tort_block_end();
  
  i = 0;
  v = tort_send(tort_s(map), v, b);
  tort_printf(io, "v = %T\n", v);

  tort_printf(io, "v as lisp object = %O\n", v);

  printf("\nDONE\n");

  return 0;
}

