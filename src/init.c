#include "tort/tort.h"
#include "tort/internal.h"
#include "tort/init.h"


tort_v tort_runtime_create()
{
  /* Create runtime object. */
  _tort = tort_ref(tort_runtime, tort_allocate(0, 0, sizeof(tort_runtime), 0));
  tort_runtime_initialize_error();

  /* Create mtable method table. */
  _tort->_mt_mtable      = tort_mtable_create(0);

  /* Create core method tables. */
  _tort->_mt_object      = tort_mtable_create(0);

  /* Backpatch mtable method table as object. */
  tort_h_ref(_tort->_mt_mtable)->mtable = _tort->_mt_mtable;

  _tort->_mt_map         = tort_mtable_create(_tort->_mt_object);
  /* Backpatch mtable to map delegation. */
  tort_ref(tort_mtable, _tort->_mt_mtable)->delegate = _tort->_mt_map;

  _tort->_mt_string      = tort_mtable_create(_tort->_mt_object);
  _tort->_mt_vector      = tort_mtable_create(_tort->_mt_object);
  _tort->_mt_symbol      = tort_mtable_create(_tort->_mt_object);
  _tort->_mt_method      = tort_mtable_create(_tort->_mt_object);
  _tort->_mt_message     = tort_mtable_create(_tort->_mt_object);
  _tort->_mt_nil         = tort_mtable_create(_tort->_mt_object);
  _tort->_mt_boolean     = tort_mtable_create(_tort->_mt_object);

  /* Initialize tagged object header. */
  _tort->_mt_tagged      = tort_mtable_create(_tort->_mt_object);
  _tort->_tagged_header.alloc_size = 0;
  _tort->_tagged_header.lookupf = _tort_object_lookupf;
  _tort->_tagged_header.applyf  = _tort_object_applyf;
  _tort->_tagged_header.mtable = _tort->_mt_tagged;

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
  _tort->_s_new    = tort_symbol_make("new");
  _tort->_s_clone  = tort_symbol_make("clone");
  _tort->_s_lookup = tort_symbol_make("lookup");
  _tort->_s_apply  = tort_symbol_make("apply");
  _tort->_s_get    = tort_symbol_make("get");
  _tort->_s_get_key = tort_symbol_make("get_key");
  _tort->_s_set    = tort_symbol_make("set");
  _tort->_s_value  = tort_symbol_make("value");
  _tort->_s_true   = tort_symbol_make("true");
  _tort->_s_false  = tort_symbol_make("false");
  _tort->_s_size  = tort_symbol_make("size");
  _tort->_s_alloc_size  = tort_symbol_make("alloc_size");
  _tort->_s_each  = tort_symbol_make("each");
  _tort->_s_map  = tort_symbol_make("map");

  /* Uncloneable objects. */
  tort_add_method(_tort->_mt_symbol,  "clone", _tort_object_identity);
  tort_add_method(_tort->_mt_nil,     "clone", _tort_object_identity);
  tort_add_method(_tort->_mt_tagged,  "clone", _tort_object_identity);
  tort_add_method(_tort->_mt_boolean, "clone", _tort_object_identity);

  /* Basic object methods. */
  tort_add_method(_tort->_mt_object, "clone", _tort_object_clone);
#if 0
  tort_add_method(_tort->_mt_object, "lookup", _tort_object_lookupf);
  tort_add_method(_tort->_mt_object, "apply", _tort_object_applyf);
#endif

  // tort_add_method(_tort->_mt_method, "apply", _tort_method_applyf);

  /* Basic map methods. */
  tort_add_method(_tort->_mt_map, "initialize", _tort_map_initialize);
  tort_add_method(_tort->_mt_map, "get", _tort_map_get);
  tort_add_method(_tort->_mt_map, "get_key", _tort_map_get_key);
  tort_add_method(_tort->_mt_map, "set", _tort_map_set);
  tort_add_method(_tort->_mt_map, "delete", _tort_map_delete);
  tort_add_method(_tort->_mt_map, "clone", _tort_map_clone);
  tort_add_method(_tort->_mt_map, "size", _tort_map_size);

  /* Vector methods. */
  tort_add_method(_tort->_mt_vector, "new", _tort_vector_new);
  tort_add_method(_tort->_mt_vector, "clone", _tort_vector_clone);
  tort_add_method(_tort->_mt_vector, "get", _tort_vector_get);
  tort_add_method(_tort->_mt_vector, "set", _tort_vector_set);
  tort_add_method(_tort->_mt_vector, "size", _tort_vector_size);
  tort_add_method(_tort->_mt_vector, "alloc_size", _tort_vector_alloc_size);
  tort_add_method(_tort->_mt_vector, "each", _tort_vector_each);
  tort_add_method(_tort->_mt_vector, "map", _tort_vector_map);

  /* String methods. */
  tort_add_method(_tort->_mt_string, "new", _tort_string_new);
  tort_add_method(_tort->_mt_string, "clone", _tort_string_clone);
  tort_add_method(_tort->_mt_string, "get", _tort_string_get);
  tort_add_method(_tort->_mt_string, "set", _tort_string_set);
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
  tort_runtime_initialize_lisp();
  // tort_runtime_initialize_address();

  _tort->_initialized = tort_true;

  return tort_ref_box(_tort);
}


