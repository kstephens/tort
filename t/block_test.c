#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>


int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v v, b;
  int i;

  tort_runtime_create();

  io = tort_stdout;

  v = tort_vector_new(0, 10);
  
  i = 0;
  b = tort_block_(tort_v obj) {
    fprintf(stderr, "  %d\n", (int) i);
    return tort_i(i ++);
  }
  tort_block_end();
  v = tort_send(tort_s(map), v, b);
  tort_printf(io, "v = %T\n", v);
  
  
  i = 0;
  b = tort_block_(tort_v obj) {
    tort_printf(io,
		"  in each [%d] => %T\n", 
		i ++, 
		obj);
    return obj;
  }
  tort_block_end();
  tort_send(tort_s(each), v, b);

  printf("\nDONE\n");

  return 0;
}

