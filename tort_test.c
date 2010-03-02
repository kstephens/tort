#include "tort/tort.h"

#include <stdio.h>


int main(int argc, char **argv)
{
  tort_runtime_create();
  printf("\n symbols = ");
  tort_send(_tort->_s_write, _tort->symbols);
  printf("\n _mt_map = ");
  tort_send(_tort->_s_write, _tort->_mt_map);

  tort_send(_tort->_s_write, tort_send(tort_s(get), _tort->symbols, tort_string_new_cstr("new")));

  return 0;
}

