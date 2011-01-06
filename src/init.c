#include "tort/tort.h"
#include "tort/internal.h"
#include "tort/init.h"
#include "tort/symtab.h"


#if TORT_MULTIPLICITY
tort_runtime *_tort;
#else
_tort_runtime_data __tort = { { sizeof(tort_runtime), _tort_object_lookupf, _tort_object_applyf, 0 } };
tort_runtime *_tort = &__tort._runtime;
#endif

tort_v tort_runtime_create_ (int *argcp, char ***argvp, char ***envp)
{
  tort_runtime_initialize_malloc();

  /* Create runtime object. */
#if TORT_MULTIPLICITY
  _tort = 
#else
    (void)
#endif
    tort_ref(tort_runtime, tort_allocate(0, sizeof(tort_runtime)));

  tort_runtime_initialize_error();

  /* Setup environment from main. */
  tort_(_argc) = *argcp;
  tort_(_argv) = *argvp;
  tort_(_env)  = *envp;

  /* Allocate and initialize mtables */
  tort_runtime_initialize_mtable();

  tort_h_mtable(_tort) = tort__mt(runtime);

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

  /* Create the empty containers. */
  tort_string_null = _tort_m_string__new(0, 0, 0);
  tort_vector_null = _tort_m_vector__new(0, 0, 0);

  /* Create the symbol table. */
  tort_(symbols) = tort_map_create();
  
  /* Create the root table. */
  tort_(root) = tort_map_create();

  /* Create the core symbols. */
  tort_runtime_initialize_symbol();

  /* Uncloneable objects. */
  tort_add_method(tort__mt(symbol),  "clone", _tort_m_object__identity);
  tort_add_method(tort__mt(nil),     "clone", _tort_m_object__identity);
  tort_add_method(tort__mt(tagged),  "clone", _tort_m_object__identity);
  tort_add_method(tort__mt(boolean), "clone", _tort_m_object__identity);

  /* Initialize system method table. */
  tort_h(_tort)->mtable = tort_mtable_create(tort__mt(object));

  /* Subsystem initialization. */
  tort_runtime_initialize_gc();

  /* Create the mtable map. */
  tort_(m_mtable) = tort_map_create();
  
  tort_runtime_initialize_io();
  tort_runtime_initialize_write();
  tort_runtime_initialize_debug();
  tort_runtime_initialize_method();

  /* Setup the root namespace. */
  // tort_send(tort__s(set), tort_(root), tort_symbol_make("root"), tort_(root));
  tort_send(tort__s(set), tort_(root), tort_symbol_make("nil"), tort_nil);
  tort_send(tort__s(set), tort_(root), tort_symbol_make("true"), tort_true);
  tort_send(tort__s(set), tort_(root), tort_symbol_make("false"), tort_false);
  tort_send(tort__s(set), tort_(root), tort_symbol_make("mtable"), tort_(m_mtable));
  tort_send(tort__s(set), tort_(root), tort_symbol_make("stdin"), tort_stdin);
  tort_send(tort__s(set), tort_(root), tort_symbol_make("stdout"), tort_stdout);
  tort_send(tort__s(set), tort_(root), tort_symbol_make("stderr"), tort_stderr);

#define tort_d_mt(X) \
  if ( tort__mt(X) ) tort_send(tort__s(set), tort_(m_mtable), tort_symbol_make(#X), tort__mt(X));
#include "tort/d_mt.h"

  tort_(_initialized) = tort_true;

  tort_runtime_initialize_symtab();
  tort_runtime_initialize_dl();
  tort_runtime_initialize_tort();

  // fprintf(stderr, "\ntort: initialized\n");

  return tort_ref_box(_tort);
}


