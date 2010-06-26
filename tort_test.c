#include "tort/tort.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_val io;

  tort_runtime_create();

  io = tort_stdout;

  printf("\n nil = ");
  tort_send(tort_s(write), tort_nil, io);

  printf("\n _mt_map = ");
  tort_send(tort_s(write), _tort->_mt_map, io);
  
  printf("\n symbols = ");
  tort_send(tort_s(write), _tort->symbols, io);

  tort_send(tort_s(write), tort_send(tort_s(get), _tort->symbols, tort_string_new_cstr("new")), io);

  printf("\n");

  return 0;
}

