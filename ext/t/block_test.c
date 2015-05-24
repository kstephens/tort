#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>
#include <assert.h>

int main(int argc, char **argv, char **environ)
{
  tort_v io;
  tort_v v;
  int i;

  tort_runtime_create();
  tort_send(tort_s(load), tort_mt(dynlib), tort_string_new_cstr("libtortext"));

  io = tort_stdout;

#if 0
  tort_block_(a, tort_v obj) {
    printf("  block a obj = %lld\n", (long long) tort_I(obj));
    return tort_i(tort_I(obj) << 2);
  } tort_block_END(a);
  v = tort_sendn(tort_s(value), 1, a, tort_i(5));
  tort_printf(io, "\nv = ", v);
  tort_inspect(io, v);
  tort_printf(io, "\n\n");

  v = tort_send(tort_s(_new), tort_mt(vector), 0, 10);
  
  i = 0;
  tort_block_(b, tort_v obj) {
    fprintf(stderr, "  %d\n", (int) i);
    return tort_i(i ++);
  } tort_block_END(b);
  v = tort_send(tort_s(map), v, b);
  tort_printf(io, "\nv = ", v);
  tort_inspect(io, v);
  tort_printf(io, "\n");
  
  i = 0;
  tort_block_(c, tort_v obj) {
    tort_printf(io,
		"  in each [%d] => %lld\n", 
		i ++,
		(long long) tort_I(obj));
    return obj;
  } tort_block_END(c);
  tort_send(tort_s(each), v, c);
#endif
  i = 0;
  v = 0;

  printf("\nDONE\n");

  return 0;
}

