#include "tort/tort.h"

#include <time.h>

int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v v;

  time_t t0, t1;
  size_t i;
  size_t n = 100000000;

  tort_runtime_create();

  io = tort_stdout;

  v = tort_send(tort__s(get), _tort->symbols, tort_string_new_cstr("size"));

  t0 = time(0);
  for ( i = 0; i < n; ++ i ) {
    tort_send(tort__s(size), _tort->symbols);
  }
  t1 = time(0);
  t1 = t1 - t0;
  printf("%ld send/%ld sec = %g send/sec\n", 
	 (long) n,
	 (long) t1,
	 (double) n / (double) t1);

  printf("\nDONE\n");

  return 0;
}

