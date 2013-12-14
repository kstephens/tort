#include "tort/tort.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef struct tort_og_context
{
  FILE *fp;
  int edgeid;

  struct slot {
    tort_v obj;
    tort_v name;
    const char *name_format;
    const char *sep;
    tort_v value;
    const char *format;
    const char *style;
    const char *link_style;
    int port;
    struct slot *next;
  } *slots, **slots_next;

  struct link {
    tort_v src_obj;
    int src_port;
    tort_v dst_obj;
    int dst_port;
    const char *style;
    int edge_id;
    struct link *next;
  } *links, **links_next;

  struct visited {
    void *ptr;
    struct visited *next;
  } *visited;

  unsigned int port_id;
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
  bzero(v, sizeof(*v));
  v->ptr = ptr;
  v->next = context->visited;
  context->visited = v;
}

static void link_make(tort_og_context *context, tort_v src_obj, int src_port, tort_v dst_obj, int dst_port, const char *style)
{
  struct link *link = malloc(sizeof(*link));
  bzero(link, sizeof(*link));
  link->src_obj = src_obj;
  link->src_port = src_port;
  link->dst_obj = dst_obj;
  link->dst_port = dst_port;
  link->style = style;
  link->edge_id = EDGEID ++;
  link->next = 0;
  *context->links_next = link;
  context->links_next = &link->next;
}

static char *sgml_encode(const char *s)
{
  if ( s ) {
    size_t size = strlen(s) * 6 + 1;
    char *t = malloc(size); // "&amp;" "&#255;"
    char *o = t;
    int c;
    
    bzero(t, size);
    while ( (c = *(s ++)) ) {
      /**/ if ( c == '&' ) strcpy(o, "&amp;");
      else if ( c == '<' ) strcpy(o, "&lt;");
      else if ( c == '>' ) strcpy(o, "&gt;");
      else if ( c == '"' ) strcpy(o, "&quot;");
      else if ( c < ' ' || *s >= 127 ) sprintf(o, "&#%d;", c);
      else *(o ++) = c;
      o = strchr(o, 0);
    }
    *o = 0;
    
    return t;
  } else {
    return (char*) s;
  }
}

static char *slot_str(tort_og_context *context, struct slot *slot, tort_v val, int port)
{
  tort_mtable *mt = tort_h_mtable(val);
  char buf[128], *e = buf + sizeof(buf) - 2;
  bzero(buf, sizeof(buf));

  if ( ! port ) port = slot->port;
  if ( 0 ) {
  } else if ( val == tort_nil ) {
    strcpy(buf, "nil");
  } else if ( val == 0 ) {
    strcpy(buf, "NULL");
  } else if ( mt == tort__mt(fixnum) ) {
    snprintf(buf, sizeof(buf), "%lld", (long long) tort_I(val));
  } else if ( mt == tort__mt(ptr) ) {
    snprintf(buf, sizeof(buf), "@%p", tort_P(val));
  } else if ( mt == tort__mt(string) ) {
    snprintf(buf, sizeof(buf), "\"%s\"", tort_string_charP(val));
  } else if ( mt == tort__mt(symbol) ) {
    tort_symbol *obj = val;
    if ( obj->name == tort_nil ) {
      snprintf(buf, sizeof(buf), "@symbol(%p)", val);
    } else {
      snprintf(buf, sizeof(buf), "%s", tort_symbol_charP(val));
    }
  } else {
    if ( port && tort_h_mtable(val) != tort__mt(method) ) 
      link_make(context, slot->obj, port, val, 0, slot->link_style);
    snprintf(buf, sizeof(buf), "%s", tort_object_name(val));
  }
  if ( e[0] != 0 ) {
    e[0] = e[-1] = e[-2] = '.';
  }
  return sgml_encode(buf);
}

static 
void og_slot_to_dot(tort_og_context *context, struct slot *slot)
{
  tort_v val = slot->value;
  // tort_mtable *mt = tort_h_mtable(val);
  const char *style = slot->style ? slot->style : "";

  fprintf(FP, "  <TR>");
  if ( slot->name == 0 ) {
    fprintf(FP, "<TD BGCOLOR=\"black\" ALIGN=\"LEFT\" PORT=\"-1\">");
  } else {
    fprintf(FP, "<TD %s ALIGN=\"RIGHT\">", style);
    if ( slot->name_format ) {
      fprintf(FP, slot->name_format, slot->name);
    } else {
      //fprintf(stderr, "slot %p slot->name %p\n", slot, slot->name);
      char *str = slot_str(context, slot, slot->name, 0);
      fprintf(FP, "%s", str);
      free(str);
    }
    slot->port = ++ context->port_id;
  }
  fprintf(FP, "%s</TD>", slot->sep ? slot->sep : "");

  if ( slot->port ) {
    fprintf(FP, "<TD ALIGN=\"LEFT\" PORT=\"%d\">", slot->port);
  } else {
    fprintf(FP, "<TD ALIGN=\"LEFT\">");
  }
  // fprintf(stderr, "slot %p slot->format %s\n", slot, slot->format);
  if ( slot->format ) {
    fprintf(FP, slot->format, val);
  } else {
    char *str = slot_str(context, slot, val, 0);
    // fprintf(stderr, "slot %p slot_str %s\n", slot, str);
    fprintf(FP, "%s", str);
    free(str);
  }
  fprintf(FP, "</TD>");
  fprintf(FP, "</TR>\n");
}

#if 0
static void *tog_stop_slot = 0;
static void gdb_stop_here() { }
#endif

static
void og_slot(tort_og_context *context, 
	     tort_v obj, 
	     tort_v name, const char *name_format, 
	     const char *sep,
	     tort_v value, const char *format, 
	     const char *style,
	     const char *link_style)
{
  struct slot *slot = malloc(sizeof(*slot));
  bzero(slot, sizeof(*slot));
  assert(context);
  slot->obj = obj;
  slot->name = name;
  slot->name_format = name_format;
  slot->sep = sep;
  slot->value = value;
  slot->format = format;
  slot->style = style;
  slot->link_style = link_style;
  slot->next = 0;
  *context->slots_next = slot;
  context->slots_next = &slot->next;
#if 0
  fprintf(stderr, "og_slot(...) = %p name %s\n", slot, slot->name);
  if ( slot == tog_stop_slot )
    gdb_stop_here();
#endif
}

static
void og_object(tort_og_context *context, tort_v obj)
{
  tort_mtable *mt = tort_h_mtable(obj);
  tort_mtable *cls_mt = tort_h_mtable(mt);
  const char *obj_name;
  const char *h_style = "BGCOLOR=\"#BBBBBB\"";
  const char *s_style = "BGCOLOR=\"#DDDDDD\"";
  const char *sl_style = "color=\"#888888\"";

  if ( obj == tort_nil ) {
    fprintf(FP, "node [ label=\"<0> nil\" ];\n");
    return;
  }
  else if ( obj == 0 ) {
    fprintf(FP, "node [ label=\"<0> NULL\" ];\n");
    return;
  } 
  else if ( mt == tort__mt(fixnum) ) {
    fprintf(FP, "node [ label=\"<0> %lld\" ];\n",
	    (long long) tort_I(obj));
    return;
  }
  else if ( mt == tort__mt(ptr) ) {
    fprintf(FP, "node [ label=\"<0> @%p\" ];\n",
	    tort_P(obj));
    return;
  }

  if ( visitedQ(context, obj) )
    return;

  visited(context, obj);

  obj_name = tort_object_name(obj);

#define SLOT(NAME) og_slot(context, obj, #NAME, "%s", 0, o->NAME, 0, s_style, sl_style)

  og_slot(context, obj, "applyf",     "%s", 0, tort_h(obj)->applyf, "%p", h_style, 0);
  og_slot(context, obj, "mtable",     "%s", 0, mt,                  0,    h_style, "style=\"dotted\"");
  og_slot(context, obj, 0,            "%s", 0, obj,                 0,    "BGCOLOR=\"black\" COLOR=\"WHITE\"", 0);

  if ( 0 ) {
  } else if ( mt == tort__mt(string) ) {
    og_slot(context, obj, "data", "%s", 0, obj, 0, s_style, sl_style);
  } else if ( mt == tort__mt(vector)  ) {
    og_slot(context, obj, "data", "%s", 0, obj, "%p", s_style, sl_style);
  }

  if ( mt == tort__mt(string) || mt == tort__mt(vector) || mt == tort__mt(map) || mt == tort__mt(mtable) || cls_mt == tort__mt(mtable) ) {
    og_slot(context, obj, "size",         "%s", 0, tort_i(tort_vector_base_size(obj)), 0, s_style, sl_style);
    og_slot(context, obj, "alloc_size",   "%s", 0, tort_i(tort_vector_base_alloc_size(obj)), 0, s_style, sl_style);
    og_slot(context, obj, "element_size", "%s", 0, tort_i(tort_vector_base_element_size(obj)), 0, s_style, sl_style);
  }
  
  if ( mt == tort__mt(symbol) ) {
    tort_symbol *o = obj;
    SLOT(name);
    SLOT(version);
#if TORT_ANON_SYMBOL_MTABLE
    SLOT(mtable_method_map);
#endif
  }
  else if ( mt == tort__mt(message) ) {
    tort_message *o = obj;
    SLOT(selector);
    SLOT(receiver);
    SLOT(previous_message);
    SLOT(fiber);
    SLOT(method);
    SLOT(mtable);
    SLOT(argc);
    // og_slot(context, obj, "argc", "%s", 0, tort_i(o->argc), 0, s_style, sl_style);
  }
  else if ( mt == tort__mt(method) ) {
    tort_method *o = obj;
    og_slot(context, obj, "data",   "%s", 0, o->data, "%p", s_style, sl_style);
    og_slot(context, obj, "name",   "%s", 0, o->name, 0, s_style, sl_style);
  }
  else if ( mt == tort__mt(io) ) {
    tort_io *o = obj;
    og_slot(context, obj, "fp", "%s", 0, o->fp, "%p", s_style, sl_style);
    SLOT(name);
    SLOT(mode);
    og_slot(context, obj, "flags", "%s", 0, tort_i(o->flags), 0, s_style, sl_style);
  }
  else if ( mt == tort__mt(mtable) || cls_mt == tort__mt(mtable) ) {
    tort_mtable *o = obj;
    // fprintf(stderr, "mtable %s\n", tort_object_name(obj));
    SLOT(delegate);
    og_slot(context, obj, "instance_size", "%s", 0, tort_i(o->instance_size), 0, s_style, sl_style);
    goto map;
  }
  else if ( mt == tort__mt(dynlib) ) {
    tort_dynlib *o = obj;
    SLOT(name);
    SLOT(path);
    SLOT(error);
    SLOT(base_sym);
    SLOT(base_ptr);
    goto map;
  }
  else if ( mt == tort__mt(map) ) {
    int slot_i;
    tort_map *o;
  map: 
    o = obj;
    SLOT(equality);
    slot_i = -1;
    tort_map_EACH(obj, me) { 
      ++ slot_i;
      // fprintf(stderr, "  slot %d me=%p mt(%p) = %s\n", ++ slot_i, me, me->first, tort_object_name(tort_h_mtable(me->first)));
      og_slot(context, obj, me->first, 0, " =", me->second, 0, 0, 0);
    } tort_map_EACH_END();
  }

#undef SLOT

  /* Render node and slots. */
  {
    struct slot *slot = context->slots;

    fprintf(FP, "\
\"node%p\" [ \n\
  shape = \"none\" \n\
  label=<\n\
<TABLE>\n\
",
	  (void*) obj);

    while ( slot ) {
      struct slot *slot_next = slot->next;
      og_slot_to_dot(context, slot); 
      free(slot);
      slot = slot_next;
    }

    fprintf(FP, "\n\
</TABLE>\n\
> \n\
]; \n\
");

    context->slots = 0;
    context->slots_next = &context->slots;
  }

  /* Render links */
  {
    struct link *link = context->links;
    while ( link ) {
      struct link *link_next = link->next;
      fprintf(FP, "\
\"node%p\":\"%d\":e -> \"node%p\":\"%d\":w [ \n\
  id = %d \n\
  %s \n\
]; \n\
",
	      link->src_obj, link->src_port,
	      link->dst_obj, link->dst_port ? link->dst_port : -1,
	      link->edge_id,
	      (link->style ? link->style : ""));
      link = link_next;
    }

    /* Render link destinations. */
    link = context->links;
    context->links = 0;
    context->links_next = &context->links;

    while ( link ) {
      struct link *link_next = link->next;
      og_object(context, link->dst_obj);
      free(link);
      link = link_next;
    }
  }
}

void og_digraph(tort_v obj, FILE *fp)
{
  tort_og_context _context, *context = &_context;
  FP = fp;
  EDGEID = 0;
  context->visited = 0;
  context->links = 0;
  context->links_next = &context->links;
  context->slots = 0;
  context->slots_next = &context->slots;

  fprintf(FP, "\
digraph g { \n\
  graph [ \n\
    rankdir = \"LR\" \n\
  ]; \n\
  node [ \n\
    fontsize = \"12\" \n\
    shape = \"record\" \n\
  ]; \n\
  edge [ \n\
  ]; \n\
");

  og_object(context, obj);

  fprintf(FP,"\
} \n\
");

  while ( context->visited ) {
    struct visited *v = context->visited->next;
    free(context->visited);
    context->visited = v;
  }
}

static
int _system(const char *cmd)
{
  int result;
  if ( (result = system(cmd)) != 0 ) {
    fprintf(stderr, "%s: error: command %s failed: %d\n", __FILE__, cmd, result);
  }
  return result;
}

void tog(tort_v obj)
{
  char cmd[1024];
  char graph_gv[1024] = "/tmp/tort.gv";
  char graph_svg[1024] = "/tmp/tort.svg";
  FILE *fp;

  fp = fopen(graph_gv, "w+");
  og_digraph(obj, fp);
  fclose(fp);

#if 0
#define SVG_OPTS ":cairo:cairo" // crashes sometimes on macports.
#else
#define SVG_OPTS ""
#endif
  sprintf(cmd, "dot -Tsvg%s -o %s %s", SVG_OPTS, graph_svg, graph_gv);
  if ( _system(cmd) != 0 ) return;

  sprintf(cmd, "open -a Firefox %s", graph_svg);  
  if ( _system(cmd) != 0 ) return;
}

tort_v _tort_M_object_graph__graph(tort_tp tort_v mtable, tort_v obj)
{
  tog(obj);
  return obj;
}

tort_v _tort_m_initializer__object_graph(tort_tp tort_v init)
{
  tort_mtable_create_class("object_graph", 0);
  return init;
}

