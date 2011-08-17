#include "tort/tort.h"
#include <unistd.h>
#include <stdlib.h>

tort_v _tort_m_posix__fork(tort_tp tort_v rcvr)
{
  return tort_i(fork());
}

tort_v _tort_m_posix__exit(tort_tp tort_v rcvr, tort_v code)
{
  exit(tort_I(code));
  return rcvr;
}

tort_v _tort_m_posix__system(tort_tp tort_v rcvr, tort_string *str)
{
  return tort_i(system(tort_string_data(str)));
}

tort_v tort_runtime_initialize_posix()
{
  tort_mtable_make("posix", 0);
  return 0;
}

