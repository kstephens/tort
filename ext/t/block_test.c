#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>


int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v v;
  int i;

  tort_runtime_create();
  tort_runtime_initialize_block();

  io = tort_stdout;

  v = tort_vector_new(0, 10);
  
  i = 0;
  tort_block_(b, tort_v obj) {
    fprintf(stderr, "  %d\n", (int) i);
    return tort_i(i ++);
  } tort_block_END(b);
  v = tort_send(tort_s(map), v, b);
  tort_printf(io, "\nv = ", v);
  tort_inspect(io, v);
  
  i = 0;
  tort_block_(c, tort_v obj) {
    tort_printf(io,
		"  in each [%d] => %T\n", 
		i ++, 
		obj);
    return obj;
  } tort_block_END(c);
  tort_send(tort_s(each), v, c);

  printf("\nDONE\n");

  return 0;
}

