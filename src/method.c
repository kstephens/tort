#include "tort/core.h"


/********************************************************************/



tort_v tort_runtime_initialize_method()
{

  tort_add_method(_tort->_mt_boolean, "_inspect", _tort_m_boolean___inspect);
  tort_add_method(_tort->_mt_io, "__create", _tort_m_io____create);
  tort_add_method(_tort->_mt_io, "__finalize", _tort_m_io____finalize);
  tort_add_method(_tort->_mt_io, "__write", _tort_m_io____write);
  tort_add_method(_tort->_mt_io, "close", _tort_m_io__close);
  tort_add_method(_tort->_mt_io, "eof", _tort_m_io__eof);
  tort_add_method(_tort->_mt_io, "error", _tort_m_io__error);
  tort_add_method(_tort->_mt_io, "flush", _tort_m_io__flush);
  tort_add_method(_tort->_mt_io, "open", _tort_m_io__open);
  tort_add_method(_tort->_mt_io, "popen", _tort_m_io__popen);
  tort_add_method(_tort->_mt_io, "printf", _tort_m_io__printf);
  tort_add_method(_tort->_mt_io, "read", _tort_m_io__read);
  tort_add_method(_tort->_mt_map, "_inspect", _tort_m_map___inspect);
  tort_add_method(_tort->_mt_map, "add", _tort_m_map__add);
  tort_add_method(_tort->_mt_map, "clone", _tort_m_map__clone);
  tort_add_method(_tort->_mt_map, "delete", _tort_m_map__delete);
  tort_add_method(_tort->_mt_map, "get", _tort_m_map__get);
  tort_add_method(_tort->_mt_map, "get_key", _tort_m_map__get_key);
  tort_add_method(_tort->_mt_map, "get_string", _tort_m_map__get_string);
  tort_add_method(_tort->_mt_map, "initialize", _tort_m_map__initialize);
  tort_add_method(_tort->_mt_map, "set", _tort_m_map__set);
  tort_add_method(_tort->_mt_map, "size", _tort_m_map__size);
  tort_add_method(_tort->_mt_message, "_inspect", _tort_m_message___inspect);
  tort_add_method(_tort->_mt_message, "backtrace", _tort_m_message__backtrace);
  tort_add_method(_tort->_mt_method, "_inspect", _tort_m_method___inspect);
  tort_add_method(_tort->_mt_mtable, "add_method", _tort_m_mtable__add_method);
  tort_add_method(_tort->_mt_nil, "_inspect", _tort_m_nil___inspect);
  tort_add_method(_tort->_mt_object, "__debugger", _tort_m_object____debugger);
  tort_add_method(_tort->_mt_object, "__message", _tort_m_object____message);
  tort_add_method(_tort->_mt_object, "__register_finalizer", _tort_m_object____register_finalizer);
  tort_add_method(_tort->_mt_object, "_inspect", _tort_m_object___inspect);
  tort_add_method(_tort->_mt_object, "_name", _tort_m_object___name);
  tort_add_method(_tort->_mt_object, "clone", _tort_m_object__clone);
  tort_add_method(_tort->_mt_object, "identity", _tort_m_object__identity);
  tort_add_method(_tort->_mt_string, "_inspect", _tort_m_string___inspect);
  tort_add_method(_tort->_mt_string, "clone", _tort_m_string__clone);
  tort_add_method(_tort->_mt_string, "get", _tort_m_string__get);
  tort_add_method(_tort->_mt_string, "new", _tort_m_string__new);
  tort_add_method(_tort->_mt_string, "set", _tort_m_string__set);
  tort_add_method(_tort->_mt_symbol, "_inspect", _tort_m_symbol___inspect);
  tort_add_method(_tort->_mt_tagged, "_inspect", _tort_m_tagged___inspect);
  tort_add_method(_tort->_mt_vector, "_inspect", _tort_m_vector___inspect);
  tort_add_method(_tort->_mt_vector, "alloc_size", _tort_m_vector__alloc_size);
  tort_add_method(_tort->_mt_vector, "clone", _tort_m_vector__clone);
  tort_add_method(_tort->_mt_vector, "each", _tort_m_vector__each);
  tort_add_method(_tort->_mt_vector, "get", _tort_m_vector__get);
  tort_add_method(_tort->_mt_vector, "map", _tort_m_vector__map);
  tort_add_method(_tort->_mt_vector, "new", _tort_m_vector__new);
  tort_add_method(_tort->_mt_vector, "set", _tort_m_vector__set);
  tort_add_method(_tort->_mt_vector, "size", _tort_m_vector__size);

  return 0;
}

