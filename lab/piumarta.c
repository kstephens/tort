/* A sample implementation in GNU C of the object model described in this paper.
  * This code, and that of the benchmarks discussed in the text, can be downloaded from: http://piumarta.com/oopsla07
  */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef ICACHE
#define ICACHE 1          /* nonzero to enable point-of-send inline cache */
#endif
#ifndef MCACHE
#define MCACHE 1          /* nonzero to enable global method cache        */
#endif

struct vtable;
struct object;
struct symbol;

typedef struct object *(*method_t)(struct object *receiver, ...);

struct vtable
{
   struct vtable *_vt[0];
   int              size;
   int              tally;
   struct object **keys;
   struct object **values;
   struct vtable *parent;
};

struct object {
   struct vtable *_vt[0];
};

struct symbol
{
  struct vtable *_vt[0];
  char *string;
  int version;
};

struct closure
{
  struct vtable *_vt[0];
  method_t method;
  struct object *data;
};

struct vtable *vtable_vt      = 0;
struct vtable *object_vt      = 0;
struct vtable *symbol_vt      = 0;
struct vtable *closure_vt     = 0;

struct object *s_addMethod  = 0;
struct object *s_allocate   = 0;
struct object *s_delegated  = 0;
struct object *s_lookup     = 0;
struct object *s_intern     = 0;
struct object *s_setParent  = 0;

struct object *symbol         = 0;
struct vtable *SymbolList     = 0;

static inline void *alloc(size_t size)
{
  struct vtable **ppvt = calloc(1, sizeof(struct vtable *) + size);
  return (void *)(ppvt + 1);
}

struct object *symbol_new(char *string)
{
  struct symbol *symbol = alloc(sizeof(*symbol));
  symbol->_vt[-1] = symbol_vt;
  symbol->string = strdup(string);
  symbol->version = 0;
  return (struct object *)symbol;
}

struct object *closure_new(method_t method, struct object *data)
{
  struct closure *closure = alloc(sizeof(*closure));
  closure->_vt[-1] = closure_vt;
  closure->method = method;
  closure->data = data;
  return (struct object *)closure;
}

struct mcache_entry {
  struct vtable *vtable;
  struct symbol *selector;
  long /* long */ selector_version;
  struct closure *closure;
};

struct object *vtable_lookup(struct vtable *self, struct object *key);

#if ICACHE
# define _send(RCV, SEL_AND_ARGS...) \
  ({									\
    static struct mcache_entry cache;					\
    struct object *r = (struct object *)(RCV);				\
    struct vtable *vt = r->_vt[-1];					\
    struct symbol *sel = (void*) _send_SEL(SEL_AND_ARGS);		\
    (cache.vtable == vt &&						\
     cache.selector == sel && cache.selector_version == sel->version	\
     ? cache.closure							\
     : (cache.vtable = vt,						\
	cache.selector = sel,						\
	cache.selector_version = sel->version,				\
	cache.closure = _bind(r, (void*) sel)))				\
      ->method(_send_RCV_ARGS(r, SEL_AND_ARGS));			\
  })
#else
# define _send(RCV, SEL_AND_ARGS...)					\
  ({									\
    struct object *r      = (struct object *)(RCV);			\
    _bind(r, (struct symbol*) _send_SEL(SEL_AND_ARGS))			\
      ->method(_send_RCV_ARGS(r, SEL_AND_ARGS));			\
  })
#endif
#define _send_SEL(SEL, ARGS...)(SEL)
#define __send_RCV_ARGS(RCV, ARGS...)RCV, ##ARGS
#define _send_RCV_ARGS(RCV, EATEN, ARGS...)__send_RCV_ARGS(RCV, ##ARGS)
#define send(RCV, SEL_AND_ARGS...)_send(RCV, SEL_AND_ARGS)

#if MCACHE
#define MCACHE_SIZE 8192
struct mcache_entry MethodCache[MCACHE_SIZE];
#define MHASH(vt, msg) (((unsigned)vt << 2) ^ ((unsigned)msg >> 3)) % MCACHE_SIZE;
#endif

struct closure * _bind(struct object *rcv, struct symbol *msg)
{
  struct closure *method;
  struct vtable *vt = rcv->_vt[-1];

#if MCACHE
  unsigned int    hash = MHASH(vt, msg);
  struct mcache_entry *line = MethodCache + hash;
  if ( line->vtable == vt && 
       line->selector == msg && line->selector_version == msg->version
       )
    return line->closure;
#endif

  method = (((void *) msg == (void*) s_lookup) && (rcv == (struct object *)vtable_vt))
    ? (struct closure *)vtable_lookup(vt, (void*) msg)
    : (struct closure *)send(vt, s_lookup, (void*) msg);

#if MCACHE
  line->vtable    = vt;
  line->selector  = msg;
  line->selector_version = msg->version;
  line->closure    = method;
#endif

  return method;
}

#if MCACHE
static void _flush_mcache(struct vtable *vt, struct object *msg)
{
  int i;
  for ( i = 0; i < MCACHE_SIZE; ++ i ) {
    struct mcache_entry *line = MethodCache + i;
    if ( ! msg || (void*) line->selector == (void*) msg ) {
      line->vtable = 0;
      line->selector = 0;
      line->selector_version = 0;
      line->closure = 0;
    }
  }
}
#endif

struct object *vtable_allocate(struct vtable *self, int payloadSize)
{
  struct object *object = alloc(payloadSize);
  object->_vt[-1] = self;
  return object;
}

struct vtable *vtable_delegated(struct vtable *self)
{
  struct vtable *child = (void *)vtable_allocate(self, sizeof(*child));
  child->_vt[-1] = self ? self->_vt[-1] : 0;
  child->size    = 2;
  child->tally   = 0;
  child->keys    = calloc(child->size, sizeof(child->keys[0]));
  child->values  = calloc(child->size, sizeof(child->values[0]));
  child->parent  = self;
  return child;
}


struct object *vtable_setParent(struct vtable *self, struct vtable *value)
{
#if MCACHE
  if ( self->parent != value ) {
    _flush_mcache(self, 0);
  }
#endif
  self->parent = value;
  return (void *) self;
}


struct object *vtable_addMethod(struct vtable *self, struct object *key, struct object *value)
{
  int i;
  for (i = 0; i < self->tally; ++i)
    if (key == self->keys[i]) {
       self->values[i] = (struct object *)value;
       goto done;
    }

  if (self->tally == self->size)
    {
       self->size *= 2;
       self->keys   = realloc(self->keys,   sizeof(self->keys[0])   * self->size);
       self->values = realloc(self->values, sizeof(self->values[0]) * self->size);
    }
  self->keys[self->tally] = key;
  self->values[self->tally++] = value;

 done:
  if ( self != SymbolList ) {
    /* If the key is a symbol, invalidate mcache and icaches. */
    if ( symbol_vt && key->_vt[-1] == symbol_vt ) {
#if MCACHE || ICACHE
      ((struct symbol *) key)->version ++;
#endif
    }
  }

  return value;
}

struct object *vtable_lookup(struct vtable *self, struct object *key)
{
  int i;

  for (i = 0; i < self->tally; ++i)
    if (key == self->keys[i])
       return self->values[i];

  if (self->parent)
    return send(self->parent, s_lookup, key);

  return 0;
}

struct object *symbol_intern(struct object *self, char *string)
{
  struct object *symbol;
  int i;
  for (i = 0; i < SymbolList->tally; ++i) {
    symbol = SymbolList->keys[i];
    if (!strcmp(string, ((struct symbol *)symbol)->string))
      return symbol;
  }
  symbol = symbol_new(string);
  vtable_addMethod(SymbolList, symbol, 0);
  return symbol;
}
                                                            
#define trace() printf("%s %d\n", __FUNCTION__, __LINE__); fflush(stdout)

void init(void)
{
  vtable_vt = vtable_delegated(0);
  vtable_vt->_vt[-1] = vtable_vt;

  object_vt = vtable_delegated(0);
  object_vt->_vt[-1] = vtable_vt;

  vtable_vt->parent = object_vt;

  symbol_vt = vtable_delegated(object_vt);

  SymbolList = vtable_delegated(0);

  closure_vt = vtable_delegated(object_vt);

  s_lookup = symbol_intern(0, "lookup");
  vtable_addMethod(vtable_vt, s_lookup,    closure_new((method_t) vtable_lookup, 0));

  s_addMethod = symbol_intern(0, "addMethod");
  vtable_addMethod(vtable_vt, s_addMethod, closure_new((method_t) vtable_addMethod, 0));

  s_allocate = symbol_intern(0, "allocate");
  send(vtable_vt, s_addMethod, s_allocate, closure_new((method_t) vtable_allocate, 0));

  s_setParent = symbol_intern(0, "setParent");
  send(vtable_vt, s_addMethod, s_setParent, closure_new((method_t) vtable_setParent, 0));

  symbol = send(symbol_vt, s_allocate, sizeof(struct symbol));

  s_intern = symbol_intern(0, "intern");
  send(symbol_vt, s_addMethod, s_intern, closure_new((method_t) symbol_intern, 0));

  s_delegated = send(symbol, s_intern, "delegated");
  send(vtable_vt, s_addMethod, s_delegated, closure_new((method_t) vtable_delegated, 0));
}

/********************************************************************/

struct takker
{
  struct vtable *_vt[0];
  int result;
};

struct vtable *takker_vt      = 0;
struct object *s_tak  = 0;

#ifndef TAK_C_CALL
#define TAK_C_CALL 0
#endif
#if TAK_C_CALL
#define TAK(X,Y,Z)tak(0, (X), (Y), (Z))
#else
#define TAK(X,Y,Z)((int) send(self, s_tak, (X), (Y), (Z)))
#endif

int tak(struct takker *self, int x, int y, int z)
{
  // printf("tak(%p, %d, %d, %d)\n", self, x, y, z);
  if ( ! (y < x) ) {
    return z;
  } else {
    return TAK(
	       TAK(x - 1, y, z),
	       TAK(y - 1, z, x),
	       TAK(z - 1, x, y));
  }
}


int main(int argc, char **argv)
{
  struct takker *t;
  init();

  takker_vt = (void*) send(vtable_vt, s_delegated, object_vt);
  s_tak = (void*) send(symbol, s_intern, (struct object*) "tak");
  send(takker_vt, s_addMethod, s_tak, closure_new((method_t) tak, 0));
  t = (void*) send(takker_vt, s_allocate, sizeof(*t));
  
  {
    int i = 0;
    int r = 0;
    for ( i = 0; i < 100; ++ i ) {
      r = (int) send(t, s_tak, 40, 15, 9);
    }
    printf("r = %d\n", r);
  }
  
  return 0;
}


