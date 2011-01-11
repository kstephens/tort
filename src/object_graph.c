#include "tort/tort.h"
#include <stdlib.h>
#include <string.h>

typedef struct tort_og_context
{
  FILE *fp;
  int edgeid;
  struct visited {
    void *ptr;
    struct visited *next;
  } *visited;
} tort_og_context;

#define FP context->fp
#define EDGEID context->edgeid

static int visitedQ(tort_og_context *context, void *ptr)
{
  struct visited *v = context->visited;
  while ( v ) {
    if ( v->ptr == ptr ) 
      return 1;
    v = v->next;
  }
  return 0;
}

static void visited(tort_og_context *context, void *ptr)
{
  struct visited *v = malloc(sizeof(*v));
  v->ptr = ptr;
  v->next = context->visited;
  context->visited = v;
}

static void g1(tort_og_context *context, tort_v obj)
{
  tort_mtable *mt = tort_h_mtable(obj);
  tort_mtable *cls_mt = tort_h_mtable(mt);
  const char *obj_name;
  void *links[16][2];
  int linksn = 0;

#define LINK_PTR(NAME, PTR)						\
  if ( linksn >= 15 ) {							\
    links[linksn][0] = "...";						\
    fprintf(FP, " | ...");						\
  } else {								\
    links[linksn][0] = (void*) (NAME);					\
    fprintf(FP, " | <%s> %s", (char*) links[linksn][0], (char*) links[linksn][0]); \
    if ( (PTR) == tort_nil ) fprintf(FP, "=nil");			\
    else if ( (PTR) == 0 ) fprintf(FP, "=0");				\
    links[linksn ++][1] = (PTR);					\
  }									\

#define LINK(NAME) LINK_PTR(#NAME, o->NAME)

  if ( obj == tort_nil ) {
    fprintf(FP, "node \"nil\";\n");
    return;
  }
  else if ( obj == 0 ) {
    fprintf(FP, "node \"NULL\";\n");
    return;
  }

  if ( visitedQ(context, obj) )
    return;

  visited(context, obj);

  obj_name = tort_object_name(obj);

  fprintf(FP, "\
\"node%p\" [ \n\
",
	  (void*) obj);

  fprintf(FP, "\
  label = \"<alloc_size> alloc_size=%lld",
	  (unsigned long long) tort_h(obj)->alloc_size);

  LINK_PTR("mtable", mt);

  fprintf(FP, " | <_> %p %s", 
	  (void*) obj, obj_name ? obj_name : "");
    
  
  if ( 0 ) {
  } else if ( tort__mt(mtable) == mt ) {
  }

  if ( 0 ) {
  }
  else if ( tort__mt(string) == mt ) {
    fprintf(FP, " | <data> data=\\\"%s\\\"", tort_string_data(obj));
    fprintf(FP, " | <size> size=%lld", (long long) tort_vector_base_size(obj));
    fprintf(FP, " | <alloc_size> alloc_size=%lld", (long long) tort_vector_base_alloc_size(obj));
  }
  else if ( tort__mt(vector) == mt ) {
    fprintf(FP, " | <data> data=%p", tort_vector_data(obj));
    fprintf(FP, " | <size> size=%lld", (long long) tort_vector_base_size(obj));
    fprintf(FP, " | <alloc_size> alloc_size=%lld", (long long) tort_vector_base_alloc_size(obj));
  }
  else if ( tort__mt(symbol) == mt ) {
    fprintf(FP, " | <name> name=%s", tort_symbol_data(obj));
  }
  else if ( tort__mt(message) == mt ) {
    tort_message *o = obj;
    LINK(selector);
    LINK(receiver);
    LINK(previous_message);
    LINK(fiber);
    LINK(method);
    LINK(mtable);
  }
  else if ( tort__mt(method) == mt ) {
    tort_method *o = obj;
    fprintf(FP, " | <applyf> applyf=%p", o->applyf);
    fprintf(FP, " | <data> data=%p", o->data);
    fprintf(FP, " | <name> name=%s", o->name != tort_nil ? tort_symbol_data(o->name) : "nil");
    // LINK(name);
  }
  else if ( tort__mt(mtable) == mt || cls_mt == tort__mt(mtable) ) {
    tort_mtable *o = obj;
    LINK(delegate);
    tort_map_EACH(obj, me) { 
      LINK_PTR(tort_symbol_data(me->first), me->second); 
    } tort_map_EACH_END();
  }

  fprintf(FP, "\
\" \n\
  shape = \"record\" \n\
]; \n\
");

  {
    int i; void *ptr;

    for ( i = 0; i < linksn; ++ i ) {
      if ( ((char*)links[i][0])[0] == '.' ) continue;
      if ( ! (ptr = links[i][1]) ) continue;
      g1(context, ptr);
    }

    for ( i = 0; i < linksn; ++ i ) {
      if ( ((char*)links[i][0])[0] == '.' ) continue;
      if ( ! (ptr = links[i][1]) ) continue;
      fprintf(FP, "\
\"node%p\":%s -> \"node%p\":_ [ \n\
  id = %d  \n\
]; \n\
",
	      obj, (const char*) links[i][0], 
	      ptr,
	      EDGEID ++);
    
    }
  }

}

static void g0(tort_v obj, FILE *fp)
{
  tort_og_context _context, *context = &_context;
  FP = fp;
  EDGEID = 0;
  context->visited = 0;

  fprintf(FP, "\
digraph g { \n\
  graph [ \n\
    rankdir = \"LR\" \n\
  ]; \n\
  node [ \n\
    fontsize = \"16\" \n\
    shape = \"ellipse\" \n\
  ]; \n\
  edge [ \n\
  ]; \n\
");

  g1(context, obj);

  fprintf(FP,"\
} \n\
");

  while ( context->visited ) {
    struct visited *v = context->visited->next;
    free(context->visited);
    context->visited = v;
  }
}

void tog(tort_v obj)
{
  char cmd[1024];
  char graph_gv[1024] = "/tmp/tort.gv";
  char graph_svg[1024] = "/tmp/tort.svg";
  FILE *fp;

  fp = fopen(graph_gv, "w+");
  g0(obj, fp);
  fclose(fp);

  sprintf(cmd, "dot -Tsvg:cairo:cairo -o %s %s", graph_svg, graph_gv);
  system(cmd);

  sprintf(cmd, "open %s", graph_svg);  
  system(cmd);
}

