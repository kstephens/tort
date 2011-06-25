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
    const char *name;
    const char *name_format;
    tort_v value;
    const char *format;
    const char *style;
    const char *link_style;
    struct slot *next;
  } *slots, **slots_next;

  struct link {
    tort_v src_obj;
    const char *src_slot;
    tort_v dst_obj;
    const char *dst_slot;
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
  v->ptr = ptr;
  v->next = context->visited;
  context->visited = v;
}

static void link(tort_og_context *context, tort_v src_obj, const char *src_slot, tort_v dst_obj, const char *dst_slot, const char *style)
{
  struct link *link = malloc(sizeof(*link));
  link->src_obj = src_obj;
  link->src_slot = src_slot;
  link->dst_obj = dst_obj;
  link->dst_slot = dst_slot;
  link->style = style;
  link->edge_id = EDGEID ++;
  link->next = 0;
  *context->links_next = link;
  context->links_next = &link->next;
}

static char *sgml_encode(const char *s)
{
  size_t size = strlen(s) * 6 + 1;
  char *t = malloc(size); // "&amp;" "&#255;"
  char *o = t;
  int c;

  memset(t, 0, size);
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
}

static const char *slot_str(tort_og_context *context, struct slot *slot, tort_v val, const char *port)
{
  tort_mtable *mt = tort_h_mtable(val);
  char buf[128], *e = buf + sizeof(buf) - 2;
  memset(buf, 0, sizeof(buf));

  if ( ! port ) port = slot->name;
  if ( 0 ) {
  } else if ( val == tort_nil ) {
    return "nil";
  } else if ( val == 0 ) {
    return "NULL";
  } else if ( mt == tort__mt(tagged) ) {
    snprintf(buf, sizeof(buf), "%lld", (long long) tort_I(val));
  } else if ( mt == tort__mt(string) ) {
    snprintf(buf, sizeof(buf), "\"%s\"", tort_string_charP(val));
  } else if ( mt == tort__mt(symbol) ) {
    snprintf(buf, sizeof(buf), "%s", tort_symbol_charP(val));
  } else {
    if ( port && tort_h_mtable(val) != tort__mt(method) ) 
      link(context, slot->obj, port, val, 0, slot->link_style);
    snprintf(buf, sizeof(buf), "%s", tort_object_name_(val));
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
  tort_mtable *mt = tort_h_mtable(val);
  const char *style = slot->style ? slot->style : "";

  fprintf(FP, "  <TR>");
  if ( 0 ) {
  } else if ( mt == tort__mt(pair) ) {
    tort_pair *o = val;
    char _port_first[128], _port_second[128];
    char *port_first, *port_second;
   
    ++ context->port_id;
    snprintf(_port_first,  sizeof(_port_first),  "%u_first",  context->port_id);
    snprintf(_port_second, sizeof(_port_second), "%u_second", context->port_id);

    port_first = strdup(_port_first);
    port_second = strdup(_port_second);

    fprintf(FP, "<TD %s ALIGN=\"RIGHT\" PORT=\"%s\">%s =</TD><TD %s ALIGN=\"LEFT\" PORT=\"%s\">%s</TD>",
	    style, port_first,
	    slot_str(context, slot, o->first, port_first),
	    style, port_second,
	    slot_str(context, slot, o->second, port_second));
  } else {
    if ( slot->name == 0 ) {
      fprintf(FP, "<TD PORT=\"0\" BGCOLOR=\"black\"></TD>");
      fprintf(FP, "<TD ALIGN=\"LEFT\">");
    } else {
      //fprintf(stderr, "slot %p slot->name %p\n", slot, slot->name);
      char *slot_ = sgml_encode(slot->name);
      fprintf(FP, "<TD ALIGN=\"RIGHT\">");
      fprintf(FP, slot->name_format ? slot->name_format : "%s", slot_);
      fprintf(FP, "</TD>");
      fprintf(FP, "<TD ALIGN=\"LEFT\" PORT=\"%s\">", slot_);
      free(slot_);
    }
    // fprintf(stderr, "slot %p slot->format %s\n", slot, slot->format);
    if ( slot->format ) {
      fprintf(FP, slot->format, val);
    } else {
      const char *str = slot_str(context, slot, val, 0);
      // fprintf(stderr, "slot %p slot_str %s\n", slot, str);
      fprintf(FP, "%s", str);
    }
    fprintf(FP, "</TD>");
  }
  fprintf(FP, "</TR>\n");
}

#if 0
static void *tog_stop_slot = 0;
static void gdb_stop_here() { }
#endif

static
void og_slot(tort_og_context *context, 
	     tort_v obj, const char *name, const char *name_format, 
	     tort_v value, const char *format, 
	     const char *style,
	     const char *link_style)
{
  struct slot *slot = malloc(sizeof(*slot));
  assert(context);
  slot->obj = obj;
  slot->name = name;
  slot->name_format = name_format;
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
  // tort_mtable *cls_mt = tort_h_mtable(mt);
  const char *obj_name;

  if ( obj == tort_nil ) {
    fprintf(FP, "node [ label=\"<0> nil\" ];\n");
    return;
  }
  else if ( obj == 0 ) {
    fprintf(FP, "node [ label=\"<0> NULL\" ];\n");
    return;
  } 
  else if ( mt == tort__mt(tagged) ) {
    fprintf(FP, "node [ label=\"<0> %lld\" ];\n",
	    (long long) tort_I(obj));
    return;
  }

  if ( visitedQ(context, obj) )
    return;

  visited(context, obj);

  obj_name = tort_object_name(obj);

#define SLOT(NAME) og_slot(context, obj, #NAME, 0, o->NAME, 0, 0, 0)

  og_slot(context, obj, "alloc_size", 0, tort_i(tort_h(obj)->alloc_size), 0, 0, 0);
  og_slot(context, obj, "mtable",     0, mt, 0, 0, "style=\"dotted\"");
  og_slot(context, obj, 0,            0, obj, 0, "BGCOLOR=\"black\" COLOR=\"WHITE\"", 0);

  if ( 0 ) {
  } else if ( mt == tort__mt(string) ) {
    og_slot(context, obj, "data", 0, obj, 0, 0, 0);
  } else if ( mt == tort__mt(vector)  ) {
    og_slot(context, obj, "data", 0, obj, "%p", 0, 0);
  }

  if ( mt == tort__mt(string) || mt == tort__mt(vector) || mt == tort_mt(map) || mt == tort_mt(mtable) ) {
    og_slot(context, obj, "size",         0, tort_i(tort_vector_base_size(obj)), 0, 0, 0);
    og_slot(context, obj, "alloc_size",   0, tort_i(tort_vector_base_alloc_size(obj)), 0, 0, 0);
    og_slot(context, obj, "element_size", 0, tort_i(tort_vector_base_element_size(obj)), 0, 0, 0);
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
    og_slot(context, obj, "argc", 0, tort_i(o->argc), "%ld", 0, 0);
  }
  else if ( mt == tort__mt(method) ) {
    tort_method *o = obj;
    og_slot(context, obj, "applyf", 0, o->applyf, "%p", 0, 0);
    og_slot(context, obj, "data",   0, o->data, "%p", 0, 0);
    og_slot(context, obj, "name",   0, o->name, 0, 0, 0);
  }
  else if ( mt == tort__mt(mtable) ) {
    tort_mtable *o = obj;
    int slot_i = -1;
    // fprintf(stderr, "mtable %s\n", tort_object_name(obj));
    SLOT(delegate);
    tort_map_EACH(obj, me) {
      slot_i ++;
      // fprintf(stderr, "  slot %d me=%p mt(%p) = %s\n", slot_i, me, me->first, tort_object_name(tort_h_mtable(me->first)));
      if ( tort_h_mtable(me->first) != tort__mt(symbol) ) {
	fprintf(stderr, "  obj=%p slot=%d : UNEXPECTED NON-SYMBOL key %s in mtable\n", 
		obj, 
		slot_i, 
		tort_object_name(me->first)
		);
	og_slot(context, obj, tort_object_name(me->first), "%s =", me->second, 0, 0, 0);
      } else 
	og_slot(context, obj, tort_object_name(me->first), 0, me, 0, 0, 0);
    } tort_map_EACH_END();
  }
  else if ( mt == tort__mt(map) ) {
    int slot_i = -1;
    tort_map_EACH(obj, me) { 
      ++ slot_i;
      // fprintf(stderr, "  slot %d me=%p mt(%p) = %s\n", ++ slot_i, me, me->first, tort_object_name(tort_h_mtable(me->first)));
      og_slot(context, obj, tort_object_name(me->first), "%s =", me->second, 0, 0, 0);
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
\"node%p\":\"%s\":e -> \"node%p\":\"%s\":w [ \n\
  id = %d \n\
  %s \n\
]; \n\
",
	      link->src_obj, link->src_slot,
	      link->dst_obj, link->dst_slot ? link->dst_slot : "0",
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

void tog(tort_v obj)
{
  char cmd[1024];
  char graph_gv[1024] = "/tmp/tort.gv";
  char graph_svg[1024] = "/tmp/tort.svg";
  FILE *fp;

  fp = fopen(graph_gv, "w+");
  og_digraph(obj, fp);
  fclose(fp);

  sprintf(cmd, "dot -Tsvg:cairo:cairo -o %s %s", graph_svg, graph_gv);
  system(cmd);

  sprintf(cmd, "open -a Firefox %s", graph_svg);  
  system(cmd);
}

