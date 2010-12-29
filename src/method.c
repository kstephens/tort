#include "tort/core.h"


/********************************************************************/



tort_v tort_runtime_initialize_method()
{

  tort_add_method(tort__mt(boolean), "_inspect", _tort_m_boolean___inspect);
  tort_add_method(tort__mt(io), "__create", _tort_m_io____create);
  tort_add_method(tort__mt(io), "__finalize", _tort_m_io____finalize);
  tort_add_method(tort__mt(io), "__write", _tort_m_io____write);
  tort_add_method(tort__mt(io), "close", _tort_m_io__close);
  tort_add_method(tort__mt(io), "eof", _tort_m_io__eof);
  tort_add_method(tort__mt(io), "error", _tort_m_io__error);
  tort_add_method(tort__mt(io), "flush", _tort_m_io__flush);
  tort_add_method(tort__mt(io), "open", _tort_m_io__open);
  tort_add_method(tort__mt(io), "popen", _tort_m_io__popen);
  tort_add_method(tort__mt(io), "printf", _tort_m_io__printf);
  tort_add_method(tort__mt(io), "read", _tort_m_io__read);
  tort_add_method(tort__mt(map), "_inspect", _tort_m_map___inspect);
  tort_add_method(tort__mt(map), "_load_symtab", _tort_m_map___load_symtab);
  tort_add_method(tort__mt(map), "add", _tort_m_map__add);
  tort_add_method(tort__mt(map), "clone", _tort_m_map__clone);
  tort_add_method(tort__mt(map), "delete", _tort_m_map__delete);
  tort_add_method(tort__mt(map), "get", _tort_m_map__get);
  tort_add_method(tort__mt(map), "get_entry", _tort_m_map__get_entry);
  tort_add_method(tort__mt(map), "get_entry_by_value", _tort_m_map__get_entry_by_value);
  tort_add_method(tort__mt(map), "get_entry_cstr", _tort_m_map__get_entry_cstr);
  tort_add_method(tort__mt(map), "get_entry_string", _tort_m_map__get_entry_string);
  tort_add_method(tort__mt(map), "get_key", _tort_m_map__get_key);
  tort_add_method(tort__mt(map), "get_string", _tort_m_map__get_string);
  tort_add_method(tort__mt(map), "initialize", _tort_m_map__initialize);
  tort_add_method(tort__mt(map), "set", _tort_m_map__set);
  tort_add_method(tort__mt(message), "_inspect", _tort_m_message___inspect);
  tort_add_method(tort__mt(message), "backtrace", _tort_m_message__backtrace);
  tort_add_method(tort__mt(method), "_inspect", _tort_m_method___inspect);
  tort_add_method(tort__mt(mtable), "_delegate_changed", _tort_m_mtable___delegate_changed);
  tort_add_method(tort__mt(mtable), "_method_changed", _tort_m_mtable___method_changed);
  tort_add_method(tort__mt(mtable), "add_method", _tort_m_mtable__add_method);
  tort_add_method(tort__mt(mtable), "delegate", _tort_m_mtable__delegate);
  tort_add_method(tort__mt(mtable), "set_delegate", _tort_m_mtable__set_delegate);
  tort_add_method(tort__mt(nil), "_inspect", _tort_m_nil___inspect);
  tort_add_method(tort__mt(object), "__debugger", _tort_m_object____debugger);
  tort_add_method(tort__mt(object), "__message", _tort_m_object____message);
  tort_add_method(tort__mt(object), "__register_finalizer", _tort_m_object____register_finalizer);
  tort_add_method(tort__mt(object), "_inspect", _tort_m_object___inspect);
  tort_add_method(tort__mt(object), "_name", _tort_m_object___name);
  tort_add_method(tort__mt(object), "clone", _tort_m_object__clone);
  tort_add_method(tort__mt(object), "identity", _tort_m_object__identity);
  tort_add_method(tort__mt(string), "_dlopen", _tort_m_string___dlopen);
  tort_add_method(tort__mt(string), "_inspect", _tort_m_string___inspect);
  tort_add_method(tort__mt(string), "get", _tort_m_string__get);
  tort_add_method(tort__mt(string), "new", _tort_m_string__new);
  tort_add_method(tort__mt(string), "set", _tort_m_string__set);
  tort_add_method(tort__mt(symbol), "_create", _tort_m_symbol___create);
  tort_add_method(tort__mt(symbol), "_inspect", _tort_m_symbol___inspect);
  tort_add_method(tort__mt(tagged), "_inspect", _tort_m_tagged___inspect);
  tort_add_method(tort__mt(vector), "_inspect", _tort_m_vector___inspect);
  tort_add_method(tort__mt(vector), "each", _tort_m_vector__each);
  tort_add_method(tort__mt(vector), "get", _tort_m_vector__get);
  tort_add_method(tort__mt(vector), "map", _tort_m_vector__map);
  tort_add_method(tort__mt(vector), "new", _tort_m_vector__new);
  tort_add_method(tort__mt(vector), "set", _tort_m_vector__set);
  tort_add_method(tort__mt(vector_base), "_add", _tort_m_vector_base___add);
  tort_add_method(tort__mt(vector_base), "_append", _tort_m_vector_base___append);
  tort_add_method(tort__mt(vector_base), "_data", _tort_m_vector_base___data);
  tort_add_method(tort__mt(vector_base), "_initialize", _tort_m_vector_base___initialize);
  tort_add_method(tort__mt(vector_base), "_ref", _tort_m_vector_base___ref);
  tort_add_method(tort__mt(vector_base), "_resize", _tort_m_vector_base___resize);
  tort_add_method(tort__mt(vector_base), "alloc_size", _tort_m_vector_base__alloc_size);
  tort_add_method(tort__mt(vector_base), "append", _tort_m_vector_base__append);
  tort_add_method(tort__mt(vector_base), "clone", _tort_m_vector_base__clone);
  tort_add_method(tort__mt(vector_base), "element_size", _tort_m_vector_base__element_size);
  tort_add_method(tort__mt(vector_base), "size", _tort_m_vector_base__size);

  return 0;
}

