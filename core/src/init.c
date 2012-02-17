#include "tort/tort.h"
#include "tort/internal.h"
#include <assert.h>

#if TORT_MULTIPLICITY
tort_runtime *_tort;
#else
tort_runtime_ __tort;
tort_runtime *_tort = &__tort._;
#endif

#define INIT(N) {                                                       \
    extern tort_v _tort_m_initializer__##N(tort_tp tort_v init);	\
    if ( tort_(initializer) ) {						\
      tort_send(tort__s(N), tort_(initializer));			\
    } else {								\
      if ( _tort_init_debug )						\
	fprintf(stderr, "  tort: init %s (bootstrap)\n", #N);		\
      init_backlog[init_backlog_n ++] = tort__s(N);			\
      _tort_m_initializer__##N(tort_ta 0);				\
    }									\
  }

static int _tort_init_debug;

tort_v tort_runtime_create_ (int *argcp, char ***argvp, char ***envp)
{
  tort_mtable *obj_mt, *cls_mt;
  tort_v init_backlog[10]; int init_backlog_n = 0;

  {
    char *s;
    if ( (s = getenv("TORT_INIT_DEBUG")) && *s )
      _tort_init_debug = atoi(s);
  }

  assert(sizeof(tort_header) % sizeof(tort_v) == 0);
  INIT(malloc);

  /* Create runtime object. */
#if TORT_MULTIPLICITY
  _tort = tort_ref(tort_runtime, tort_allocate(0, sizeof(tort_runtime)));
#endif

  tort_(_initialized) = 0;

  INIT(error);

  /* Setup environment from main. */
  tort_(_argc) = *argcp;
  tort_(_argv) = *argvp;
  tort_(_env)  = *envp;
  tort_(stack_bottom) = envp;
  
  /* Allocate and initialize mtables */
  INIT(mtable);

  tort_h(_tort)->mtable = tort__mt(runtime);
  tort_h(_tort)->applyf = _tort_m_object___cannot_apply;

#if ! TORT_NIL_IS_ZERO
  /* Create the nil object. */
  tort_nil = tort_allocate(tort__mt(nil), sizeof(tort_object));
#endif

  /* Create the boolean objects. */
  tort_true = tort_allocate(tort__mt(boolean), sizeof(tort_object));
#if ! TORT_FALSE_IS_NIL
  tort_false = tort_allocate(tort__mt(boolean), sizeof(tort_object));
#endif

  /* Backpatch object delegate as nil. */
  tort_ref(tort_mtable, tort__mt(object))->delegate = tort_nil;

  /* Initialize the message reference. */
  _tort_message = tort_nil;
  tort_(message) = tort_nil;

  /* Initialize lookup(). */
  INIT(lookup);
  
  /*******************************************************/
  /* Messaging Boot strap. */

  /* Create the symbol table. */
  tort_(symbols) = tort_map_new();
  
  obj_mt = tort__mt(mtable);
  cls_mt = tort_h_mtable(obj_mt);
  
  tort__s(lookup) = tort_symbol_new("lookup");
  tort_add_method(cls_mt, "lookup", _tort_m_mtable__lookup);

  tort__s(add_method) = tort_symbol_new("add_method");
  tort_add_method(cls_mt, "add_method", _tort_m_mtable__add_method);

  tort__s(allocate) = tort_symbol_new("allocate");
  tort_send(tort__s(add_method), cls_mt, tort__s(allocate), tort_method_new(_tort_M_object__allocate, 0));

  /******************************************************/

  /* Create the core symbols. */
  INIT(symbol);

  /* Create the mtable map. */
  tort_(m_mtable) = tort_map_new();
#define tort_d_mt(X) \
  if ( tort__mt(X) )  _tort_m_map__set(tort_ta tort_(m_mtable), tort_symbol_new(#X), tort__mt(X));
#include "tort/d_mt.h"

  /* Install core methods. */
  INIT(method);

  /* Start initializer. */
  tort_(initializer) = tort_send(tort__s(new), tort__mt(initializer));
  while ( init_backlog_n > 0 )
    tort_send(tort__s(set), tort_(initializer), init_backlog[-- init_backlog_n], tort__s(initialized));

  INIT(eq);
  INIT(cmp);

  /* Add core slots. */
  INIT(slot);

  /* Create the root table. */
  tort_(root) = tort_map_new();

  /* Symbol Encoder. */
  INIT(symbol_encoder);

  /* Uncloneable objects. */
  tort_add_method(tort__mt(symbol),  "clone", _tort_m_object__identity);  
  tort_add_method(tort__mt(nil),     "clone", _tort_m_object__identity);
  tort_add_method(tort__mt(ptr),     "clone", _tort_m_object__identity);
  tort_add_method(tort__mt(tagged),  "clone", _tort_m_object__identity);
  tort_add_method(tort__mt(boolean), "clone", _tort_m_object__identity);

  /* Initialize system method table. */
  tort_h(_tort)->mtable = tort_mtable_new_class(tort__mt(object));

  /* Subsystem initialization. */
  INIT(gc);

  /* unknown caller_info */
  tort_(unknown_caller_info) = tort_send(tort__s(_allocate), tort__mt(caller_info), tort_i(sizeof(tort_caller_info)));
  tort_(unknown_caller_info)->file = "<unknown>";

  /* Setup the root namespace. */
#define ROOT(N,V) tort_send(tort__s(set), tort_(root), tort_symbol_new(#N), (V))
  ROOT(runtime, tort_ref_box(_tort));
  ROOT(initializer, tort_(initializer));
  ROOT(nil, tort_nil);
  ROOT(true, tort_true);
  ROOT(false, tort_false);
  ROOT(symbols, tort_(symbols));
  ROOT(mtable, tort_(m_mtable));
  ROOT(unknown_caller_info, tort_(unknown_caller_info));
  ROOT(tag_bits, tort_i(TORT_TAG_BITS));
  ROOT(word_size, tort_i(sizeof(tort_v)));
  ROOT(object_header_size, tort_i(sizeof(tort_header)));

  INIT(io);
  INIT(printf);
  INIT(debug);
  INIT(dynlib);

  {
    int i; tort_v m = tort_map_new();
    for ( i = 0; i < 1 << TORT_TAG_BITS; ++ i ) {
      tort_v k = tort_i(i), v = tort_(tagged_header)[i].mtable;
      if ( ! v ) v = tort_nil;
      tort_send(tort__s(set), m, k, v);
      tort_send(tort__s(set), m, v, k);
    }
    ROOT(tagged_mtables, m);
  }

  {
    int i; tort_v v = tort_vector_new(0, 0);
    for ( i = 0; i < tort_(_argc) && tort_(_argv)[i]; ++ i ) {
      tort_send(tort__s(add), v, tort_string_new_cstr(tort_(_argv)[i]));
    }
    ROOT(argv, v);
  }

  INIT(gc_ready);

  tort_(_initialized) = tort_true;

  // fprintf(stderr, "\ntort: initialized\n");
#undef ROOT
#undef INIT

  return tort_ref_box(_tort);
}

tort_v _tort_m_initializer__go(tort_tp tort_v init, tort_v name)
{
  tort_v state = tort_send(tort__s(get), init, name);
  if ( state == tort_nil ) {
    tort_send(tort__s(set), init, name, tort__s(initializing));
    if ( _tort_init_debug >= 2 )
      fprintf(stderr, "  tort: init %s\n", tort_symbol_data(name));
    tort_send(name, init);
    if ( _tort_init_debug )
      fprintf(stderr, "  tort: init %s : DONE\n", tort_symbol_data(name));
    tort_send(tort__s(set), init, name, tort__s(initialized));
  }
  return init;
}
