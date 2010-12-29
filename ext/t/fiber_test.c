#include "tort/tort.h"
#include "tort/block.h"
#include "tort/fiber.h"

#include <stdio.h>


int main(int argc, char **argv, char **environ)
{
  tort_v io;
  int i;

  tort_runtime_create();
  tort_send(tort_s(_dlopen), tort_string_new_cstr(LIB_DIR "/libtortext.dylib"));

  io = tort_stdout;

  tort_block_(block_a, tort_v obj) {
    tort_v fiber_a = tort_send(tort_s(fiber), _tort_message);
    tort_v fiber_b = 0;
    tort_block_(block_b, tort_v obj) {
      fiber_b = tort_send(tort_s(fiber), _tort_message);
      while ( i > 0 ) {
	fprintf(stderr, "fiber b @%p %d\n", fiber_b, (int) i --);
	tort_send(tort_s(yield), fiber_a);
      }
      return 0;
    } tort_block_END(block_b);

    while ( i > 0 ) {
      fprintf(stderr, "fiber a @%p %d\n", fiber_a, (int) i --);
      if ( fiber_b ) {
	tort_send(tort_s(yield), fiber_b);
      } else {
	tort_send(tort_s(_fiber_new), tort_mt(fiber), block_b); 
      }
    }
    return 0;
  } tort_block_END(block_a);

  i = 20;
  tort_send(tort_s(_fiber_new), tort_mt(fiber), block_a);

  printf("\nDONE\n");

  return 0;
}

