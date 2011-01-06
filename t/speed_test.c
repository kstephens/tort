#include "tort/tort.h"

#include <sys/time.h>

static
double tort_get_time()
{
  struct timeval tv;
  if ( gettimeofday(&tv, 0) == -1 ) {
    return -1;
  } else {
    return (double) tv.tv_sec + (double) tv.tv_usec / 1000000.0;
  }
}


int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v v;

  double t0, t1;
  size_t i;
  size_t n = 100000000;

  tort_runtime_create();

  io = tort_stdout;

  v = tort_send(tort__s(get), tort_(symbols), tort_string_new_cstr("size"));

  t0 = tort_get_time();
  for ( i = 0; i < n; ++ i ) {
    tort_send(tort__s(size), tort_(symbols));
  }
  t1 = tort_get_time() - t0;

  printf("%ld send / @f%f sec = @f%f send/sec\n", 
	 (long) n,
	 (double) t1,
	 (double) n / (double) t1);

  printf("\nDONE\n");

  return 0;
}

