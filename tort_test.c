#include "tort/tort.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_runtime_create();
  printf("\n nil = ");
  tort_send(_tort->_s_write, tort_nil, 0);
  printf("\n symbols = ");
  tort_send(_tort->_s_write, _tort->symbols, 0);
  printf("\n _mt_map = ");
  tort_send(_tort->_s_write, _tort->_mt_map, 0);
  return 0;
}

