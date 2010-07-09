#include "tort/tort.h"
#include "tort/internal.h"
#include "tort/init.h"


tort_v tort_runtime_create_ (int *argcp, char ***argvp, char ***envp)
{
  tort_runtime_initialize_malloc();

  /* Create runtime object. */
  _tort = tort_ref(tort_runtime, tort_allocate(0, 0, sizeof(tort_runtime), 0));
  tort_runtime_initialize_error();

  /* Setup environment from main. */
  _tort->_argc = *argcp;
  _tort->_argv = *argvp;
  _tort->_env  = *envp;

  /* Allocate and initialize mtables */
  tort_runtime_initialize_mtable();

  /* Create the nil object. */
  tort_nil = tort_allocate(0, 0, sizeof(tort_object), _tort->_mt_nil);

  /* Create the boolean objects. */
  tort_true = tort_allocate(0, 0, sizeof(tort_object), _tort->_mt_boolean);
  tort_false = tort_allocate(0, 0, sizeof(tort_object), _tort->_mt_boolean);

  /* Backpatch object delegate as nil. */
  tort_ref(tort_mtable, _tort->_mt_object)->delegate = tort_nil;

  /* Initialize the message reference. */
  _tort_message = tort_nil;
  _tort->message = tort_nil;

  /* Create the empty containers. */
  tort_string_null = _tort_string_new(0, 0, 0);
  tort_vector_null = _tort_vector_new(0, 0, 0);

  /* Create the symbol table. */
  _tort->symbols = tort_map_create();
  
  /* Create the root table. */
  _tort->root = tort_map_create();

  /* Create the core symbols. */
  tort_runtime_initialize_symbol();

  /* Uncloneable objects. */
  tort_add_method(_tort->_mt_symbol,  "clone", _tort_object_identity);
  tort_add_method(_tort->_mt_nil,     "clone", _tort_object_identity);
  tort_add_method(_tort->_mt_tagged,  "clone", _tort_object_identity);
  tort_add_method(_tort->_mt_boolean, "clone", _tort_object_identity);

  /* Basic object methods. */

  /* String methods shared with vector.. */
  tort_add_method(_tort->_mt_string, "size", _tort_vector_size);
  tort_add_method(_tort->_mt_string, "alloc_size", _tort_vector_alloc_size);

  /* Initialize system method table. */
  tort_h(_tort)->mtable = tort_mtable_create(_tort->_mt_object);

  /* Prepare special symbol table get method. */
  tort_h(_tort->symbols)->mtable = tort_mtable_create(tort_h_mtable(_tort->symbols));
  tort_add_method(tort_h_mtable(_tort->symbols), "get", _tort_map_get_string);
  tort_add_method(tort_h_mtable(_tort->symbols), "set", _tort_object_identity);
  tort_add_method(tort_h_mtable(_tort->symbols), "delete", _tort_object_identity);

  /* Subsystem initialization. */
  tort_runtime_initialize_gc();
  tort_runtime_initialize_io();
  tort_runtime_initialize_write();
  tort_runtime_initialize_block();
  tort_runtime_initialize_debug();
  tort_runtime_initialize_method();

  /* Addons. */
  tort_runtime_initialize_lisp();
  tort_runtime_initialize_symtab();

  _tort->_initialized = tort_true;

  // fprintf(stderr, "\ntort: initialized\n");

  return tort_ref_box(_tort);
}


