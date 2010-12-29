/* -*- c -*- */
#include "tort/core.h"

/********************************************************************/

const char *_tort_symbol_data(tort_v sym) 
{ 
  return tort_string_data(tort_ref(tort_symbol, sym)->name);
}

tort_symbol* _tort_m_symbol___create(tort_thread_param tort_v name)
{
  tort_symbol *value = tort_allocate(0, 0, sizeof(tort_symbol), tort__mt(symbol));
  value->name = name;
  value->version = tort_i(0);
  return value;
}

tort_v tort_symbol_make(const char *string)
{
  if ( string ) {
    tort_map_entry *e = _tort_m_map__get_entry_cstr(tort_thread_arg tort_(symbols), string);
    if ( e ) {
      // fprintf(stderr, "\n old symbol = %s %p\n", tort_symbol_data(e->value), (void *) e->value);
      return e->value;
    } else {
      tort_string *name = tort_string_new_cstr(string);
      tort_symbol *sym = _tort_m_symbol___create(tort_thread_arg name);
      _tort_m_map__add(tort_thread_arg tort_(symbols), name, sym);
      // fprintf(stderr, "\n new symbol = %s %p\n", tort_symbol_data(value), (void *) value);
      return sym;
    }
  } else {
    return _tort_m_symbol___create(tort_thread_arg tort_nil);
  }
}


tort_v tort_runtime_initialize_symbol()
{

  tort__s(__create) = tort_symbol_make("__create");
  tort__s(__finalize) = tort_symbol_make("__finalize");
  tort__s(__register_finalizer) = tort_symbol_make("__register_finalizer");
  tort__s(__write) = tort_symbol_make("__write");
  tort__s(_data) = tort_symbol_make("_data");
  tort__s(_delegate_changed) = tort_symbol_make("_delegate_changed");
  tort__s(_inspect) = tort_symbol_make("_inspect");
  tort__s(_method_changed) = tort_symbol_make("_method_changed");
  tort__s(alloc_size) = tort_symbol_make("alloc_size");
  tort__s(append) = tort_symbol_make("append");
  tort__s(apply) = tort_symbol_make("apply");
  tort__s(backtrace) = tort_symbol_make("backtrace");
  tort__s(backtrace_size) = tort_symbol_make("backtrace_size");
  tort__s(clone) = tort_symbol_make("clone");
  tort__s(close) = tort_symbol_make("close");
  tort__s(create) = tort_symbol_make("create");
  tort__s(delegate) = tort_symbol_make("delegate");
  tort__s(each) = tort_symbol_make("each");
  tort__s(element_size) = tort_symbol_make("element_size");
  tort__s(eof) = tort_symbol_make("eof");
  tort__s(error) = tort_symbol_make("error");
  tort__s(false) = tort_symbol_make("false");
  tort__s(format) = tort_symbol_make("format");
  tort__s(get) = tort_symbol_make("get");
  tort__s(get_key) = tort_symbol_make("get_key");
  tort__s(lookup) = tort_symbol_make("lookup");
  tort__s(map) = tort_symbol_make("map");
  tort__s(new) = tort_symbol_make("new");
  tort__s(open) = tort_symbol_make("open");
  tort__s(popen) = tort_symbol_make("popen");
  tort__s(printf) = tort_symbol_make("printf");
  tort__s(read) = tort_symbol_make("read");
  tort__s(set) = tort_symbol_make("set");
  tort__s(set_delegate) = tort_symbol_make("set_delegate");
  tort__s(size) = tort_symbol_make("size");
  tort__s(true) = tort_symbol_make("true");
  tort__s(value) = tort_symbol_make("value");
  tort__s(write) = tort_symbol_make("write");

  return tort_(symbols);
}

