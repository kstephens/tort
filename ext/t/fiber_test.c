#include "tort/tort.h"
#include "tort/block.h"
#include "tort/fiber.h"

#include <stdio.h>


int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v block_a;
  static tort_v fiber_a, fiber_b;
  static int i;

  tort_runtime_create();
  tort_runtime_initialize_block();
  tort_runtime_initialize_fiber();

  io = tort_stdout;
  block_a = tort_block_(tort_v obj) {
    fiber_a = tort_send(tort_s(fiber), _tort_message);
    while ( i > 0 ) {
      fprintf(stderr, "fiber a %d\n", (int) i --);
      if ( fiber_b ) {
	tort_send(tort_s(yield), fiber_b);
      } else {
	tort_v block_b = tort_block_(tort_v obj) {
	  fiber_b = tort_send(tort_s(fiber), _tort_message);
	  while ( i > 0 ) {
	    fprintf(stderr, "fiber b %d\n", (int) i --);
	    tort_send(tort_s(yield), fiber_a);
	  }
	  return 0;
	} tort_block_end();
	tort_send(tort_s(_fiber_new), tort_mt(fiber), block_b); 
      }
    }
    return 0;
  } tort_block_end();

  i = 20;
  tort_send(tort_s(_fiber_new), tort_mt(fiber), block_a);

  printf("\nDONE\n");

  return 0;
}

