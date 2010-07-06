#include "tort/tort.h"
#include "tort/block.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_v io;
  tort_v o, v;

  tort_runtime_create();

  io = tort_stdout;

  printf("\n  _mt_map => ");
  tort_write(io, _tort->_mt_map);

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
  
  printf("\nDONE\n");

  return 0;
}

