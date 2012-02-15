#include "tort/tort.h"
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h> /* sys_nerr, sys_errlist */
#include <errno.h> /* errno */

tort_v _tort_m_posix__errno(tort_tp tort_v rcvr)
{
  return tort_i(errno);
}
tort_v _tort_m_posix__errnoSET(tort_tp tort_v rcvr, tort_v v)
{
  return tort_i(errno = tort_I(v));
}
tort_v _tort_m_posix__errstr(tort_tp tort_v rcvr, tort_v v)
{
  int i = tort_I(v);
  return 0 <= i && i < sys_nerr ? tort_string_new_cstr(sys_errlist[i]) : tort_false;
}
tort_v _tort_m_posix__getpid(tort_tp tort_v rcvr)
{
  return tort_i(getpid());
}

tort_v _tort_m_posix__getppid(tort_tp tort_v rcvr)
{
  return tort_i(getppid());
}

tort_v _tort_m_posix__geteuid(tort_tp tort_v rcvr)
{
  return tort_i(geteuid());
}

tort_v _tort_m_posix__getegid(tort_tp tort_v rcvr)
{
  return tort_i(getegid());
}

tort_v _tort_m_posix__fork(tort_tp tort_v rcvr)
{
  return tort_i(fork());
}

tort_v _tort_m_posix__exit(tort_tp tort_v rcvr, tort_v code)
{
  exit(tort_I(code));
  return rcvr;
}

tort_v _tort_m_posix__getenv(tort_tp tort_v rcvr, tort_string *name)
{
  return tort_string_new_cstr(getenv(tort_string_data(name)));
}

tort_v _tort_m_posix__setenv(tort_tp tort_v rcvr, tort_string *name, tort_string *val)
{
  return tort_i(setenv(tort_string_data(name), tort_string_data(val), 1));
}

tort_v _tort_m_posix__unsetenv(tort_tp tort_v rcvr, tort_string *name)
{
  unsetenv(tort_string_data(name));
  return rcvr;
}

tort_v _tort_m_posix__system(tort_tp tort_v rcvr, tort_string *str)
{
  return tort_i(system(tort_string_data(str)));
}

tort_v _tort_m_posix__unlink(tort_tp tort_v rcvr, tort_string *str)
{
  return tort_i(unlink(tort_string_data(str)));
}

tort_v _tort_m_initializer__posix(tort_tp tort_v init)
{
  tort_mtable_create_class("posix", 0);
  return init;
}

