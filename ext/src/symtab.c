#include "tort/core.h"


/********************************************************************/


#if 0
tort_val _tort_m_mtable____import (tort_thread_param tort_v rcvr, tort_v sym)
{
  tort_val slot = tort_send(tort__s(get), _tort->_symtab, sym);
  if ( slot != tort_nil ) {
    _tort_mtable_add_method(tort_thread_arg rcvr, sym, slot);
  } else {
    return tort_nil;
  }
}
#endif


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
  *linep = line;
  *sizep = size;
  return rtn;
}


tort_v _tort_load_symtab(const char *file, void *ptr_base)
{
  FILE *fp;
  char cmd[1024];
  tort_v st;
  
  snprintf(cmd, sizeof(cmd), "nm -l %s 2>&1", file);
  
  st = tort_map_create();
  
  if ( (fp = popen(cmd, "r")) ) {
    char *line = 0;
    size_t line_size = 0;
    
    while ( _getline(&line, &line_size, fp) != -1 ) {
      void *c_addr;
      char c_mode = 0;
      char c_name[128];
      char c_fileline[1024];
      int  c_tokens = 0;

#if 0
      fprintf(stderr, "  line => %s\n", line);
#endif

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
#if 0
	fprintf(stderr, "   => %p %c %s %s\n", 
		c_addr,
		c_mode,
		c_name,
		c_fileline);
#endif
	c_addr = ptr_base + (size_t) c_addr;
	tort_v t_name = tort_symbol_make(c_name);
	tort_v t_addr = tort_i((size_t) c_addr);
	tort_send(tort__s(set), st, t_name, t_addr);
	tort_send(tort__s(set), st, t_addr, t_name);
      }
      // fprintf(stderr, " c_tokens = %d\n", c_tokens);

      free(line);
    }

    fclose(fp);
  }

  return st;
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
	  fprintf(stderr, "  %s.%s => @%p\n", cls, meth, ptr);
#if 0
	  tort_v cls_obj = tort_class_get(cls_buf);
	  tort_add_method(cls_obj, meth, e->value);
#endif
	  break;
	}
	++ meth;
      }
    }
  }
  tort_map_EACH_END();
  return 0;
}


tort_v tort_runtime_initialize_symtab()
{
  tort_v st = _tort_load_symtab(_tort->_argv[0], 0);
  tort_send(tort__s(set), _tort->root, tort_s(core_symtab), st);
  tort_m_map___load_methods(tort_thread_arg st);
  // tort_add_method(tort__mt(mtable), "__import", _tort_mtable___import);
  return st;
}

