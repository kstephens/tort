#include "tort/core.h"
#ifdef __linux__
#define __USE_GNU 1 /* Need Dl_info. */
#define TORT_DLIB_SUFFIX ".so"
#endif
#ifdef __APPLE__
#define TORT_DLIB_SUFFIX ".dylib"
#endif

#include <dlfcn.h> /* dlopen() */

tort_v _tort_M_dynlib__new(tort_tp tort_mtable *mtable)
{
  tort_dynlib *o = tort_allocate(mtable, sizeof(*o));
  o->name = o->path = o->error =
    o->base_sym = o->base_ptr = tort_nil;
  return _tort_m_map__initialize(tort_ta (tort_v) o);
}

int _tort_dl_debug = 0;

tort_v _tort_m_dynlib__dlopen(tort_tp struct tort_dynlib *rcvr, tort_v name)
{
  char file_buffer[1024];
  const char *file;
  void *dl;
  const char *base_sym;
  void *base_ptr;

  rcvr->name = name;
  file = tort_string_data(name);
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
  rcvr->path = tort_string_new_cstr(file);
  rcvr->base_sym = rcvr->base_ptr = tort_nil;
  tort_send(tort_s(emptyE), rcvr);
  tort_send(tort_s(_load_symtab), rcvr, file, 0);
  if ( ! tort_map_size(rcvr) )
    return rcvr;
  //#ifdef __APPLE__
  base_sym = tort_symbol_data(tort_map_data(rcvr)[0]->first);
  rcvr->base_sym = tort_string_new_cstr(base_sym);
  base_ptr = tort_ptr_data(tort_map_data(rcvr)[0]->second);
  rcvr->base_ptr = tort_ptr_new(base_ptr);
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
    tort_send(tort_s(emptyE), rcvr);
    tort_send(tort_s(_load_symtab), rcvr, file, base_ptr);
    tort_send(tort_s(set), tort_(dl_maps), tort_string_new_cstr(file), rcvr);
    tort_send(tort_s(_run_initializers), rcvr);
    tort_send(tort_s(_load_methods), rcvr);
    dlclose(dl);
  } else {
    rcvr->error = tort_string_new_cstr(dlerror());
    perror(dlerror());
  }
  return rcvr;
}

tort_v _tort_m_string___dlopen(tort_tp tort_string *rcvr)
{
  tort_v st = tort_send(tort__s(new), tort__mt(dynlib));
  return_tort_send(tort__s(dlopen), st, rcvr);
}

tort_v tort_m_dynlib___run_initializers(tort_tp tort_v map)
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

tort_v tort_m_dynlib___load_methods(tort_tp tort_v map)
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

static
int _getline(char **linep, size_t *sizep, FILE *fp)
{
  int rtn = -1;
  size_t size = 0;
  char *line = 0;
  if ( ! feof(fp) ) {
    int c;
    rtn = 1;
    line = malloc(size + 1);
    while ( (c = fgetc(fp)) != -1 ) {
      line = realloc(line, size + 1);
      if ( c == '\n' ) break;
      line[size ++] = c;
    }
    line[size] = 0;
  }
#if 0
  fprintf(stderr, "  '%s'\n", line);
#endif
  *linep = line;
  *sizep = size;
  return rtn;
}

tort_v _tort_m_dynlib___load_symtab(tort_tp tort_v st, const char *file, void *ptr_base)
{
  FILE *fp;
  char cmd[1024];
  
  snprintf(cmd, sizeof(cmd), "nm '%s' > '%s.sym' 2>&1",
	   file, file);
  system(cmd);

  if ( _tort_dl_debug ) {
  fprintf(stderr, "  load_symtab: cmd => %s\n", cmd);
  }

  snprintf(cmd, sizeof(cmd), "%s.sym", file);

  if ( _tort_dl_debug ) {
  fprintf(stderr, "  load_symtab: .sym file => %s\n", cmd);
  }

  if ( (fp = fopen(cmd, "r")) ) {
    char *line = 0;
    size_t line_size = 0;
    
    while ( _getline(&line, &line_size, fp) != -1 ) {
      void *c_addr;
      char c_mode = 0;
      char c_name_buf[128];
      char *c_name = c_name_buf;
      char c_fileline[1024];
      int  c_tokens = 0;

      if ( _tort_dl_debug ) {
      fprintf(stderr, "  line => @%p '%s'\n", line, line);
      }

      c_name[0] = c_fileline[0] = 0;
      // line[line_size] = '\0';
      c_tokens = sscanf(line, "%p %c %128s %1024s", 
			&c_addr,
			&c_mode,
			c_name,
			c_fileline
			);
      if ( (c_tokens == 3 || c_tokens == 4) &&
	   c_mode == 'T' && 
	   ! strchr(c_name, '.')
	   ) {
	c_addr = ptr_base + (size_t) c_addr;
#ifdef __APPLE__
	if ( *c_name == '_' ) ++ c_name;
#endif

#if 0
	fprintf(stderr, "   => %p %c %s %s\n", 
		c_addr,
		c_mode,
		c_name,
		c_fileline);
#endif
	tort_v t_name = tort_symbol_new(c_name);
	tort_v t_addr = tort_ptr_new(c_addr);
	if ( c_addr == (void*) tort_ptr_data(t_addr) ) {
	  tort_send(tort__s(set), st, t_name, t_addr);
	  tort_send(tort__s(set), st, t_addr, t_name);
	} else {
	  fprintf(stderr, "  tort: cannot store %s -> %p pointer in tort_ptr_data", c_name, c_addr);
	}
      }
#if 0
      fprintf(stderr, " c_tokens = %d\n", c_tokens);
#endif

      free(line);
    }

#if 0
    fprintf(stderr, "  DONE @%p\n", fp);
#endif

    fclose(fp);
  }

  return st;
}

tort_v tort_runtime_initialize_dynlib()
{
  tort_v st;

  _tort_dl_debug = 9;

  {
    const char *s = getenv("TORT_DL_DEBUG");
    _tort_dl_debug = s && *s ? atoi(s) : 0;
  }
  tort_add_method(tort__mt(dynlib), "_run_initializers", tort_m_dynlib___run_initializers);
  tort_add_method(tort__mt(dynlib), "_load_methods", tort_m_dynlib___load_methods);
  tort_(dl_maps) = tort_map_create();
  tort_send(tort__s(set), tort_(root), tort_s(dl_maps), tort_(dl_maps));

  st = tort_send(tort__s(new), tort__mt(dynlib));
  _tort_m_dynlib___load_symtab(tort_ta st, tort_(_argv)[0], 0);
  tort_send(tort__s(set), tort_(root), tort_s(core_symtab), st);

  return st;
}

