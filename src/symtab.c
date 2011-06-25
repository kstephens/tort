#include "tort/core.h"

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

tort_v _tort_m_map___load_symtab(tort_thread_param tort_v st, const char *file, void *ptr_base)
{
  FILE *fp;
  char cmd[1024];
  
  snprintf(cmd, sizeof(cmd), "nm '%s' > '%s.sym' 2>&1",
	   file, file);
  system(cmd);

  snprintf(cmd, sizeof(cmd), "%s.sym", file);

#if 0
  fprintf(stderr, "  cmd => %s\n", cmd);
#endif

  if ( (fp = fopen(cmd, "r")) ) {
    char *line = 0;
    size_t line_size = 0;
    
    while ( _getline(&line, &line_size, fp) != -1 ) {
      void *c_addr;
      char c_mode = 0;
      char c_name[128];
      char c_fileline[1024];
      int  c_tokens = 0;

#if 0
      fprintf(stderr, "  line => @%p '%s'\n", line, line);
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
	c_addr = ptr_base + (size_t) c_addr;
#if 0
	fprintf(stderr, "   => %p %c %s %s\n", 
		c_addr,
		c_mode,
		c_name,
		c_fileline);
#endif
	tort_v t_name = tort_symbol_make(c_name);
	tort_v t_addr = tort_i((size_t) c_addr);
	if ( c_addr == (void*) tort_I(t_addr) ) {
	  tort_send(tort__s(set), st, t_name, t_addr);
	  tort_send(tort__s(set), st, t_addr, t_name);
	} else {
	  fprintf(stderr, "cannot store %s->%p pointer in integer", c_name, c_addr);
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

tort_v tort_runtime_initialize_symtab()
{
  tort_v st = tort_map_create();
  _tort_m_map___load_symtab(tort_thread_arg st, tort_(_argv)[0], 0);
  tort_send(tort__s(set), tort_(root), tort_s(core_symtab), st);
  // tort_add_method(tort__mt(mtable), "__import", _tort_mtable___import);
  return st;
}

