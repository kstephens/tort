#include "tort/core.h"
#ifdef __linux__
#define __USE_GNU 1 /* Need Dl_info. */
#define TORT_DLIB_SUFFIX ".so"
#define TORT_DYNLIB_GLOBAL_PREFIX ""
#endif
#ifdef __APPLE__
#define TORT_DLIB_SUFFIX ".dylib"
#define TORT_DYNLIB_GLOBAL_PREFIX "_"
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

void *tort_dlopen(const char *file, char *file_buffer)
{
  void *dl;
  char _file_buffer[1024];
  if ( ! file_buffer )
    file_buffer = _file_buffer;
  if ( ! strchr(file, '/') ) {
    sprintf(file_buffer, "%s/%s", TORT_DLIB_DIR, file);
    file = file_buffer;
  }
  if ( strlen(file) < strlen(TORT_DLIB_SUFFIX) || strcmp(strchr(file, '\0') - strlen(TORT_DLIB_SUFFIX), TORT_DLIB_SUFFIX) ) {
    sprintf(file_buffer, "%s%s", file, TORT_DLIB_SUFFIX);
    file = file_buffer;
  }
  if ( file_buffer != file ) strcpy(file_buffer, file);
  if ( _tort_dl_debug )
    fprintf(stderr, "  tort_dlopen: file = %s\n", file);
  if ( (dl = dlopen(file, RTLD_NOW | RTLD_GLOBAL | RTLD_NODELETE)) ) {
    if ( _tort_dl_debug >= 2 )
      fprintf(stderr, "    dl = @%p\n", dl);
    return dl;
  }
  return 0;
}

void *tort_dlsym(void *dl, const char *symbol)
{
  void *ptr;
#if 0 // def __APPLE__
  char *_symbol = alloca(strlen(symbol) + 2);
  _symbol[0] = '_';
  strcpy(_symbol + 1, symbol);
  symbol = _symbol;
#endif
  ptr = dlsym(dl, symbol);
  if ( _tort_dl_debug )
    fprintf(stderr, "    tort_dlsym(@%p, \"%s\") => @%p\n", dl, symbol, ptr);
  return ptr;
}

void tort_dlclose(void *dl)
{
  dlclose(dl);
}

tort_v _tort_m_dynlib__dlopen(tort_tp struct tort_dynlib *rcvr, tort_v name)
{
  char file_buffer[1024];
  const char *file;
  void *dl;
  const char *base_sym;
  void *base_ptr;

  rcvr->name = name;
  file = tort_string_data(name);
  if ( ! (dl = tort_dlopen(file, file_buffer)) ) {
    const char *str = dlerror();
    str = str ? str : "UNKNOWN ERROR";
    rcvr->error = tort_string_new_cstr(str);
    perror(str);
    return rcvr;
  }
  file = file_buffer;
  rcvr->path = tort_string_new_cstr(file);
  rcvr->base_sym = rcvr->base_ptr = tort_nil;
  tort_send(tort__s(emptyE), rcvr);
  tort_send(tort__s(_load_symtab), rcvr, file, 0);
  if ( ! tort_map_size(rcvr) )
    return rcvr;
  base_sym = tort_symbol_data(tort_map_data(rcvr)[0]->first);
  rcvr->base_sym = tort_string_new_cstr(base_sym);
  base_ptr = tort_ptr_data(tort_map_data(rcvr)[0]->second);
  rcvr->base_ptr = tort_ptr_new(base_ptr);
  if ( _tort_dl_debug ) {
    fprintf(stderr, "    base_sym = '%s'\n", base_sym);
    fprintf(stderr, "    base_ptr = %p\n", base_ptr);
  }
  base_ptr = dlsym(dl, base_sym);
  if ( _tort_dl_debug )
    fprintf(stderr, "    dlsym(@%p, \"%s\") => @%p\n", dl, base_sym, base_ptr);
  {
    Dl_info info;
    bzero(&info, sizeof(info));
    dladdr((void*) base_ptr, &info);
    if ( _tort_dl_debug > 1 ) {
      fprintf(stderr, "    dli_fname = %s\n", info.dli_fname);
      fprintf(stderr, "    dli_fbase = %p\n", info.dli_fbase);
      fprintf(stderr, "    dli_sname = %s\n", info.dli_sname);
      fprintf(stderr, "    dli_saddr = %p\n", info.dli_saddr);
    }
    base_ptr = info.dli_fbase;
  }
  tort_dlclose(dl);
  tort_send(tort__s(emptyE), rcvr);
  tort_send(tort__s(_load_symtab), rcvr, file, base_ptr);
  tort_send(tort__s(set), tort_(dl_maps), tort_string_new_cstr(file), rcvr);
  return rcvr;
}

tort_v _tort_m_dynlib__load(tort_tp struct tort_dynlib *rcvr, tort_v name)
{
  tort_send(tort__s(dlopen), rcvr, name);
  tort_send(tort__s(_run_initializers), rcvr);
  tort_send(tort__s(_load_methods), rcvr);
  tort_send(tort__s(_load_slots), rcvr);
  return rcvr;
}

tort_v _tort_M_dynlib__load(tort_tp tort_mtable *mtable, tort_v name)
{
  tort_dynlib *o = tort_send(tort__s(new), mtable);
  return_tort_send(tort__s(load), o, name);
}

tort_v tort_m_dynlib___run_initializers(tort_tp tort_v map)
{
  static const char prefix[] = "_tort_m_initializer__";
  tort_map_EACH(map, e) {
    if ( tort_h_mtable(e->first) != tort__mt(symbol) ) continue;
    const char *name = tort_symbol_data(e->first);
    // fprintf(stderr, "e = @%p \"%s\"\n", e, name);
    if ( strncmp(name, prefix, strlen(prefix)) == 0 ) {
      void *ptr = tort_ptr_data(e->second);
      tort_v (*func)(tort_tp tort_v);
      func = ptr;
      if ( _tort_dl_debug ) 
	fprintf(stderr, "  init %s => @%p\n", name,  ptr);
      func(tort_ta tort_nil);
    }
  } tort_map_EACH_END();
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
	if ( cmeth )
	  mtable = tort_h_ref(mtable)->mtable;
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
  } tort_map_EACH_END();
  return result >= 0 ? map : tort_nil;
}


static int process_slot(const char *prefix, tort_pair *e)
{
  const char *name = tort_symbol_data(e->first);
  // fprintf(stderr, "  slot? %s (%s)\n", name, prefix);
  if ( strncmp(name, prefix, strlen(prefix)) == 0 ) {
    void *ptr = tort_ptr_data(e->second);
    void * (*func)() = ptr;
    tort_slot *slot;
    if ( _tort_dl_debug >= 2 ) 
      fprintf(stderr, "  slot %s => func @%p\n", name, ptr);
    ptr = func();
    if ( _tort_dl_debug >= 2 ) 
      fprintf(stderr, "  slot %s => tort_slot_ @%p\n", name, ptr);
    slot = tort_slot_attach(ptr);
    if ( _tort_dl_debug >= 2 ) 
      fprintf(stderr, "  %p slot %s => tort_slot @%p\n", e, name, slot);
    return 1;
  }
  return 0;
}

tort_v tort_m_dynlib___load_slots(tort_tp tort_v map)
{
  int result = 0;
  static const char slot_prefix[] = "_tort_slot_";
  tort_map_EACH(map, e) {
    if ( tort_h_mtable(e->first) != tort__mt(symbol) ) continue;
    // fprintf(stderr, "e = @%p \"%s\"\n", e, name);
    result = process_slot(slot_prefix, e);
    if ( result < 0 ) break;
  } tort_map_EACH_END();
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
  snprintf(cmd, sizeof(cmd), "nm '%s' 2>&1",
	   file);
  if ( _tort_dl_debug >= 2 )
    fprintf(stderr, "  load_symtab: cmd => %s\n", cmd);
  if ( ! (fp = popen(cmd, "r")) ) {
    return tort_error(tort_ta "cannot run %s", cmd);
  } else {
    char *line = 0;
    size_t line_size = 0;
    while ( _getline(&line, &line_size, fp) != -1 ) {
      void *c_addr;
      char c_mode = 0;
      char c_name_buf[128];
      char *c_name = c_name_buf;
      char c_fileline[1024];
      int  c_tokens = 0;
      if ( _tort_dl_debug >= 3 )
	fprintf(stderr, "  line => @%p '%s'\n", line, line);
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
	tort_v t_name = tort_symbol_new(c_name);
	tort_v t_addr = tort_ptr_new(c_addr);
	if ( c_addr == (void*) tort_ptr_data(t_addr) ) {
	  tort_send(tort__s(set), st, t_name, t_addr);
	  tort_send(tort__s(set), st, t_addr, t_name);
	} else {
	  fprintf(stderr, "  tort: cannot store %s -> %p pointer in tort_ptr_data", c_name, c_addr);
	}
      }
      // fprintf(stderr, " c_tokens = %d\n", c_tokens);
      free(line);
    }
    // fprintf(stderr, "  DONE @%p\n", fp);
    pclose(fp);
    {
      tort_v all = tort_send(tort__s(get), tort_(dl_maps), tort__s(all));
      tort_send(tort__s(emit), st, all);
    }
  }
  return st;
}

tort_v _tort_m_initializer__dynlib(tort_tp tort_v init)
{
  tort_v st;
  {
    const char *s = getenv("TORT_DL_DEBUG");
    _tort_dl_debug = s && *s ? atoi(s) : 0;
  }
  tort_send(tort__s(set), tort_(root), tort__s(dynlib_suffix), tort_string_new_cstr(TORT_DLIB_SUFFIX));
  tort_send(tort__s(set), tort_(root), tort__s(dynlib_global_prefix), tort_string_new_cstr(TORT_DYNLIB_GLOBAL_PREFIX));
  tort_add_method(tort__mt(dynlib), "_run_initializers", tort_m_dynlib___run_initializers);
  tort_add_method(tort__mt(dynlib), "_load_methods", tort_m_dynlib___load_methods);
  tort_add_method(tort__mt(dynlib), "_load_slots", tort_m_dynlib___load_slots);
  tort_(dl_maps) = tort_map_new();
  tort_send(tort__s(set), tort_(root), tort__s(dl_maps), tort_(dl_maps));
  tort_v all = tort_map_new();
  tort_send(tort__s(set), tort_(dl_maps), tort__s(all), all);

  st = tort_send(tort__s(new), tort__mt(dynlib));
  _tort_m_dynlib___load_symtab(tort_ta st, tort_(_argv)[0], 0);
  tort_send(tort__s(set), tort_(root), tort__s(prog_symtab), st);

  st = tort_send(tort__s(new), tort__mt(dynlib));
  tort_send(tort__s(dlopen), st, tort_string_new_cstr("libtortcore"));

  return init;
}

