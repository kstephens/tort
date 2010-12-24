#include "tort/core.h"
#include <dlfcn.h> /* dlopen() */

/********************************************************************/


tort_v _tort_m_string___dlopen(tort_thread_param tort_v rcvr)
{
  const char *file = tort_string_data(rcvr);
  void *dl;
  tort_v st = 0;
  const char *base_sym;
  void *base_ptr;
 
  st = tort_map_create();
  _tort_m_map___load_symtab(tort_thread_arg st, file, 0);

  base_sym = tort_symbol_data(tort_ref(tort_map, st)->entry[0]->key);
  base_ptr = (void*) tort_I(tort_ref(tort_map, st)->entry[0]->value);
#if 0
  fprintf(stderr, "base_sym = '%s'\n", base_sym);
  fprintf(stderr, "base_ptr = %p\n", base_ptr);
#endif

  if ( (dl = dlopen(file, RTLD_NOW | RTLD_GLOBAL | RTLD_NODELETE)) ) {
#if 0
    fprintf(stderr, "dl = %p\n", dl);
#endif

    base_ptr = dlsym(dl, base_sym + 1);
#if 0
    fprintf(stderr, "base_ptr = %p\n", base_ptr);
#endif
    {
      Dl_info info;
      memset(&info, 0, sizeof(info));
      dladdr((void*) base_ptr, &info);
#if 0
      fprintf(stderr, "dli_fname = %s\n", info.dli_fname);
      fprintf(stderr, "dli_fbase = %p\n", info.dli_fbase);
      fprintf(stderr, "dli_sname = %s\n", info.dli_sname);
      fprintf(stderr, "dli_saddr = %p\n", info.dli_saddr);
#endif
      base_ptr = info.dli_fbase;
    }

    st = tort_map_create();
    _tort_m_map___load_symtab(tort_thread_arg st, file, base_ptr);
    
    tort_send(tort_s(_run_initializers), st);
    tort_send(tort_s(_load_methods), st);

    dlclose(dl);
  } else {
    perror(dlerror());
  }

  return st;
}


tort_v tort_m_map___run_initializers(tort_thread_param tort_v map)
{
  static const char prefix[] = "_tort_runtime_initialize_";
  tort_map_EACH(map, e); {
    if ( tort_h_mtable(e->key) != _tort->_mt_symbol ) continue;
    char *name = tort_symbol_data(e->key);
    // fprintf(stderr, "e = @%p \"%s\"\n", e, name);
    if ( strncmp(name, prefix, strlen(prefix)) == 0 ) {
      void *ptr = (void*) tort_I(e->value);
      tort_v (*func)();
      func = ptr;
      // fprintf(stderr, "  init %s => @%p\n", name,  ptr);
      func();
     }
  }
  tort_map_EACH_END();
  return 0;
}


tort_v tort_m_map___load_methods(tort_thread_param tort_v map)
{
  static const char prefix[] = "__tort_m_";
  tort_map_EACH(map, e); {
    if ( tort_h_mtable(e->key) != _tort->_mt_symbol ) continue;
    char *name = tort_symbol_data(e->key);
    // fprintf(stderr, "e = @%p \"%s\"\n", e, name);
    if ( strncmp(name, prefix, strlen(prefix)) == 0 ) {
      char *cls = name + strlen(prefix);
      char *meth = cls;
      while ( *meth ) {
	if ( meth[0] == '_' && meth[1] == '_' ) {
	  char cls_buf[meth - cls + 1];
	  strncpy(cls_buf, cls, meth - cls);
	  cls_buf[meth - cls] = 0;
	  cls = cls_buf;
	  meth += 2;
	  void *ptr = (void*) tort_I(e->value);
	  // fprintf(stderr, "  %s.%s => @%p\n", cls, meth, ptr);
	  tort_v cls_obj = tort_class_get(cls_buf);
	  tort_add_method(cls_obj, meth, ptr);
	  break;
	}
	++ meth;
      }
    }
  }
  tort_map_EACH_END();
  return 0;
}

tort_v tort_runtime_initialize_dl()
{
  tort_add_method(_tort->_mt_string, "_dlopen", _tort_m_string___dlopen);
  tort_add_method(_tort->_mt_map, "_run_initializers", tort_m_map___run_initializers);
  tort_add_method(_tort->_mt_map, "_load_methods", tort_m_map___load_methods);
  return 0;
}

