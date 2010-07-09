#include "tort/core.h"


/********************************************************************/


tort_v tort_symbol_make(const char *string)
{
  if ( string ) {
  tort_map_entry *e = _tort_map_get_entry_cstr(0, _tort->symbols, string);
  if ( e ) {
    // fprintf(stderr, "\n old symbol = %s %p\n", tort_symbol_data(e->value), (void *) e->value);
    return e->value;
  } else {
    tort_v key, value;
    key = tort_string_new_cstr(string);
    value = tort_allocate(0, 0, sizeof(tort_symbol), _tort->_mt_symbol);
    tort_ref(tort_symbol, value)->name = key;
    _tort_map_add(0, _tort->symbols, key, value);
    // fprintf(stderr, "\n new symbol = %s %p\n", tort_symbol_data(value), (void *) value);
    return value;
  } 
  } else {
    tort_v value;
    value = tort_allocate(0, 0, sizeof(tort_symbol), _tort->_mt_symbol);
    tort_ref(tort_symbol, value)->name = tort_nil;
    return value;
  }
}


void tort_runtime_initialize_symbol()
{

  _tort->_s_DOT = tort_symbol_make("DOT");
  _tort->_s___create = tort_symbol_make("__create");
  _tort->_s___finalize = tort_symbol_make("__finalize");
  _tort->_s___register_finalizer = tort_symbol_make("__register_finalizer");
  _tort->_s___write = tort_symbol_make("__write");
  _tort->_s__inspect = tort_symbol_make("_inspect");
  _tort->_s_alloc_size = tort_symbol_make("alloc_size");
  _tort->_s_apply = tort_symbol_make("apply");
  _tort->_s_backtrace = tort_symbol_make("backtrace");
  _tort->_s_backtrace_size = tort_symbol_make("backtrace_size");
  _tort->_s_clone = tort_symbol_make("clone");
  _tort->_s_close = tort_symbol_make("close");
  _tort->_s_create = tort_symbol_make("create");
  _tort->_s_each = tort_symbol_make("each");
  _tort->_s_eof = tort_symbol_make("eof");
  _tort->_s_error = tort_symbol_make("error");
  _tort->_s_false = tort_symbol_make("false");
  _tort->_s_format = tort_symbol_make("format");
  _tort->_s_get = tort_symbol_make("get");
  _tort->_s_get_key = tort_symbol_make("get_key");
  _tort->_s_lisp_read = tort_symbol_make("lisp_read");
  _tort->_s_lisp_write = tort_symbol_make("lisp_write");
  _tort->_s_list_TO_vector = tort_symbol_make("list_TO_vector");
  _tort->_s_lookup = tort_symbol_make("lookup");
  _tort->_s_map = tort_symbol_make("map");
  _tort->_s_new = tort_symbol_make("new");
  _tort->_s_open = tort_symbol_make("open");
  _tort->_s_popen = tort_symbol_make("popen");
  _tort->_s_printf = tort_symbol_make("printf");
  _tort->_s_read = tort_symbol_make("read");
  _tort->_s_set = tort_symbol_make("set");
  _tort->_s_set_cdrE = tort_symbol_make("set_cdrE");
  _tort->_s_size = tort_symbol_make("size");
  _tort->_s_true = tort_symbol_make("true");
  _tort->_s_value = tort_symbol_make("value");
  _tort->_s_write = tort_symbol_make("write");

}
