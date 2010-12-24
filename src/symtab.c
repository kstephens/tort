#include "tort/core.h"


/********************************************************************/


#if 0
tort_val _tort_mtable___import (tort_thread_param tort_v rcvr, tort_v sym)
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
      line[size ++] = c;
    }
    line[size] = 0;
  }
  *linep = line;
  *sizep = size;
  return rtn;
}

void _tort_load_symtab()
{
  FILE *fp;
  char cmd[1024];
  tort_v st;
  
  snprintf(cmd, sizeof(cmd), "nm -l %s 2>&1", _tort->_argv[0]);
  
  st = _tort->_symtab = tort_map_create();
  
  if ( (fp = popen(cmd, "r")) ) {
    char *line = 0;
    size_t line_size = 0;
    
    while ( _getline(&line, &line_size, fp) != -1 ) {
      void *c_addr;
      char c_mode = 0;
      char c_name[128];
      char c_fileline[1024];
      int  c_tokens = 0;

      //      fprintf(stderr, "  line => %s\n", line);

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
	tort_v t_name = tort_symbol_make(c_name);
	tort_v t_addr = tort_i((unsigned long) c_addr);
	tort_send(tort__s(set), st, t_name, t_addr);
	tort_send(tort__s(set), st, t_addr, t_name);
      }
      // fprintf(stderr, " c_tokens = %d\n", c_tokens);

      free(line);
    }

    fclose(fp);
  }
}


void tort_runtime_initialize_symtab()
{
  _tort_load_symtab();
  // tort_add_method(tort__mt(mtable), "__import", _tort_mtable___import);
}

