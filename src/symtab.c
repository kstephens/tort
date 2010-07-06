#include "tort/core.h"


/********************************************************************/


void tort_runtime_initialize_symtab()
{
  FILE *fp;
  char cmd[1024];
  tort_v st;

  snprintf(cmd, sizeof(cmd), "nm -l %s", _tort->_argv[0]);

  st = _tort->_symtab = tort_map_create();

  if ( (fp = popen(cmd, "r")) ) {
    char *line = 0;
    size_t line_size = 0;

    while ( getline(&line, &line_size, fp) != -1 ) {
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

      // free(line);
    }

    fclose(fp);
  }
}

