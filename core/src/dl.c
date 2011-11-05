#include "tort/core.h"
#ifdef __linux__
#define __USE_GNU 1 /* Need Dl_info. */
#define TORT_DLIB_SUFFIX ".so"
#endif
#ifdef __APPLE__
#define TORT_DLIB_SUFFIX ".dylib"
#endif

#include <dlfcn.h> /* dlopen() */

int _tort_dl_debug = 0;

tort_v _tort_m_string___dlopen(tort_tp tort_string *rcvr)
{
  char file_buffer[1024];
  const char *file;
  void *dl;
  tort_v st = 0;
  const char *base_sym;
  void *base_ptr;

  file = tort_string_data(rcvr);
  if ( ! strchr(file, '/') ) {
    sprintf(file_buffer, "%s/%s", TORT_DLIB_DIR, file);
    file = file_buffer;
  }
  if ( strlen(file) < strlen(TORT_DLIB_SUFFIX) || strcmp(strchr(file, '\0') - strlen(TORT_DLIB_SUFFIX), TORT_DLIB_SUFFIX) ) {
    sprintf(file_buffer, "%s%s", file, TORT_DLIB_SUFFIX);
    file = file_buffer;
  }
  if ( _tort_dl_debug ) {
    fprintf(stderr, "  _dlopen: file = %s\n", file);
  }

  st = tort_map_create();
  tort_send(tort_s(_load_symtab), st, file, 0);

  if ( ! tort_map_size(st) )
    return st;

  //#ifdef __APPLE__
  base_sym = tort_symbol_data(tort_map_data(st)[0]->first);
  base_ptr = (void*) tort_I(tort_map_data(st)[0]->second);
  if ( _tort_dl_debug ) {
    fprintf(stderr, "base_sym = '%s'\n", base_sym);
    fprintf(stderr, "base_ptr = %p\n", base_ptr);
  }

  if ( (dl = dlopen(file, RTLD_NOW | RTLD_GLOBAL | RTLD_NODELETE)) ) {
    if ( _tort_dl_debug >= 2 ) {
    fprintf(stderr, "dl = @%p\n", dl);
    }

    base_ptr = dlsym(dl, base_sym);
    if ( _tort_dl_debug ) {
      fprintf(stderr, "dlsym(@%p, \"%s\") => @%p\n", dl, base_sym, base_ptr);
    }
    {
      Dl_info info;
      memset(&info, 0, sizeof(info));
      dladdr((void*) base_ptr, &info);
      if ( _tort_dl_debug > 1 ) {
      fprintf(stderr, "dli_fname = %s\n", info.dli_fname);
      fprintf(stderr, "dli_fbase = %p\n", info.dli_fbase);
      fprintf(stderr, "dli_sname = %s\n", info.dli_sname);
      fprintf(stderr, "dli_saddr = %p\n", info.dli_saddr);
      }
      base_ptr = info.dli_fbase;
    }

    st = tort_map_create();
    tort_send(tort_s(_load_symtab), st, file, base_ptr);
    
    tort_send(tort_s(set), tort_(dl_maps), tort_symbol_new(file), st);

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
  static const char prefix[] = "tort_runtime_initialize_";
  tort_map_EACH(map, e) {
    if ( tort_h_mtable(e->first) != tort__mt(symbol) ) continue;
    const char *name = tort_symbol_data(e->first);
    // fprintf(stderr, "e = @%p \"%s\"\n", e, name);
    if ( strncmp(name, prefix, strlen(prefix)) == 0 ) {
      void *ptr = tort_ptr_data(e->second);
      tort_v (*func)();
      func = ptr;
      if ( _tort_dl_debug ) 
	fprintf(stderr, "  init %s => @%p\n", name,  ptr);
      func();
     }
  }
  tort_map_EACH_END();
  return 0;
}


static int process_method(int cmeth, const char *prefix, tort_pair *e)
{
  const char *name = tort_symbol_data(e->first);
  if ( strncmp(name, prefix, strlen(prefix)) == 0 ) {
    const char *cls = name + strlen(prefix);
    const char *meth = cls;
    while ( *meth ) {
      if ( meth[0] == '_' && meth[1] == '_' ) {
	char cls_buf[meth - cls + 1];
	strncpy(cls_buf, cls, meth - cls);
	cls_buf[meth - cls] = 0;
	cls = cls_buf;
	meth += 2;
	void *ptr = tort_ptr_data(e->second);
	meth = tort_symbol_encode(meth);
	if ( _tort_dl_debug ) 
	  fprintf(stderr, "  method %s%c%s => @%p\n", cls, cmeth ? '.' : '#', meth, ptr);
	tort_v mtable = tort_mtable_get(cls_buf);
	if ( ! mtable ) {
	  tort_error(tort_ta "dl: cannot find mtable %s", cls_buf);
	  return -1;
	}
	if ( cmeth ) {
	  mtable = tort_h_ref(mtable)->mtable;
	}
	tort_add_method(mtable, meth, ptr);
	return 1;
      }
      ++ meth;
    }
  }
  return 0;
}

tort_v tort_m_map___load_methods(tort_thread_param tort_v map)
{
  int result = 0;
  static const char obj_prefix[] = "_tort_m_";
  static const char cls_prefix[] = "_tort_M_";
  tort_map_EACH(map, e) {
    if ( tort_h_mtable(e->first) != tort__mt(symbol) ) continue;
    // fprintf(stderr, "e = @%p \"%s\"\n", e, name);
    if ( (result = process_method(0, obj_prefix, e)) == 0 )
      result = process_method(1, cls_prefix, e);
    if ( result < 0 ) break;
  }
  tort_map_EACH_END();
  return result >= 0 ? map : tort_nil;
}

tort_v tort_runtime_initialize_dl()
{
  {
    const char *s = getenv("TORT_DL_DEBUG");
    _tort_dl_debug = s && *s ? atoi(s) : 0;
  }
  /* Required here for bootstrapping. */
  tort_add_method(tort_mt(map), "_run_initializers", tort_m_map___run_initializers);
  tort_add_method(tort_mt(map), "_load_methods", tort_m_map___load_methods);
  tort_(dl_maps) = tort_map_create();
  tort_send(tort__s(set), tort_(root), tort_s(dl_maps), tort_(dl_maps));

  return 0;
}

