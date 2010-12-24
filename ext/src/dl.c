#include "tort/core.h"
#include <dlfcn.h> /* dlopen() */

/********************************************************************/


tort_v _tort_m_string___dl_open(tort_thread_param const char *file)
{
  void *dl;
  tort_v st = 0;
  if ( (dl = dlopen(file, RTLD_LAZY | RTLD_GLOBAL | RTLD_NOLOAD | RTLD_NODELETE)) ) {
    void *ptr_base = dlsym(dl, "");
    (void) ptr_base;
    dlclose(dl);
  } else {
    perror(file);
  }
  return st;
}

tort_v tort_runtime_initialize_dl()
{
  tort_add_method(_tort->_mt_string, "_dl_open", _tort_m_string___dl_open);
  return 0;
}

