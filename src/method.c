#include "tort/core.h"


/********************************************************************/



void tort_runtime_initialize_method()
{

  tort_add_method(_tort->_mt_boolean, "_inspect", _tort_boolean__inspect);
  tort_add_method(_tort->_mt_boolean, "lisp_write", _tort_boolean_lisp_write);
  tort_add_method(_tort->_mt_eos, "lisp_write", _tort_eos_lisp_write);
  tort_add_method(_tort->_mt_io, "__create", _tort_io___create);
  tort_add_method(_tort->_mt_io, "__finalize", _tort_io___finalize);
  tort_add_method(_tort->_mt_io, "__write", _tort_io___write);
  tort_add_method(_tort->_mt_io, "close", _tort_io_close);
  tort_add_method(_tort->_mt_io, "eof", _tort_io_eof);
  tort_add_method(_tort->_mt_io, "error", _tort_io_error);
  tort_add_method(_tort->_mt_io, "flush", _tort_io_flush);
  tort_add_method(_tort->_mt_io, "lisp_read", _tort_io_lisp_read);
  tort_add_method(_tort->_mt_io, "open", _tort_io_open);
  tort_add_method(_tort->_mt_io, "popen", _tort_io_popen);
  tort_add_method(_tort->_mt_io, "printf", _tort_io_printf);
  tort_add_method(_tort->_mt_io, "read", _tort_io_read);
  tort_add_method(_tort->_mt_map, "_inspect", _tort_map__inspect);
  tort_add_method(_tort->_mt_map, "add", _tort_map_add);
  tort_add_method(_tort->_mt_map, "clone", _tort_map_clone);
  tort_add_method(_tort->_mt_map, "delete", _tort_map_delete);
  tort_add_method(_tort->_mt_map, "get", _tort_map_get);
  tort_add_method(_tort->_mt_map, "get_key", _tort_map_get_key);
  tort_add_method(_tort->_mt_map, "get_string", _tort_map_get_string);
  tort_add_method(_tort->_mt_map, "initialize", _tort_map_initialize);
  tort_add_method(_tort->_mt_map, "lisp_write", _tort_map_lisp_write);
  tort_add_method(_tort->_mt_map, "set", _tort_map_set);
  tort_add_method(_tort->_mt_map, "size", _tort_map_size);
  tort_add_method(_tort->_mt_message, "_inspect", _tort_message__inspect);
  tort_add_method(_tort->_mt_message, "backtrace", _tort_message_backtrace);
  tort_add_method(_tort->_mt_method, "_inspect", _tort_method__inspect);
  tort_add_method(_tort->_mt_mtable, "add_method", _tort_mtable_add_method);
  tort_add_method(_tort->_mt_nil, "_inspect", _tort_nil__inspect);
  tort_add_method(_tort->_mt_object, "__debugger", _tort_object___debugger);
  tort_add_method(_tort->_mt_object, "__message", _tort_object___message);
  tort_add_method(_tort->_mt_object, "__register_finalizer", _tort_object___register_finalizer);
  tort_add_method(_tort->_mt_object, "_inspect", _tort_object__inspect);
  tort_add_method(_tort->_mt_object, "_name", _tort_object__name);
  tort_add_method(_tort->_mt_object, "clone", _tort_object_clone);
  tort_add_method(_tort->_mt_object, "identity", _tort_object_identity);
  tort_add_method(_tort->_mt_object, "lisp_write", _tort_object_lisp_write);
  tort_add_method(_tort->_mt_string, "_inspect", _tort_string__inspect);
  tort_add_method(_tort->_mt_string, "clone", _tort_string_clone);
  tort_add_method(_tort->_mt_string, "get", _tort_string_get);
  tort_add_method(_tort->_mt_string, "new", _tort_string_new);
  tort_add_method(_tort->_mt_string, "set", _tort_string_set);
  tort_add_method(_tort->_mt_symbol, "_inspect", _tort_symbol__inspect);
  tort_add_method(_tort->_mt_symbol, "lisp_write", _tort_symbol_lisp_write);
  tort_add_method(_tort->_mt_tagged, "_inspect", _tort_tagged__inspect);
  tort_add_method(_tort->_mt_vector, "_inspect", _tort_vector__inspect);
  tort_add_method(_tort->_mt_vector, "alloc_size", _tort_vector_alloc_size);
  tort_add_method(_tort->_mt_vector, "clone", _tort_vector_clone);
  tort_add_method(_tort->_mt_vector, "each", _tort_vector_each);
  tort_add_method(_tort->_mt_vector, "get", _tort_vector_get);
  tort_add_method(_tort->_mt_vector, "lisp_write", _tort_vector_lisp_write);
  tort_add_method(_tort->_mt_vector, "map", _tort_vector_map);
  tort_add_method(_tort->_mt_vector, "new", _tort_vector_new);
  tort_add_method(_tort->_mt_vector, "set", _tort_vector_set);
  tort_add_method(_tort->_mt_vector, "size", _tort_vector_size);

}

