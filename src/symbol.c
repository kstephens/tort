/* -*- c -*- */
#include "tort/core.h"

/********************************************************************/

const char *_tort_symbol_data(tort_v sym) 
{ 
  return tort_string_data(tort_ref(tort_symbol, sym)->name);
}

tort_symbol* _tort_m_symbol___create(tort_thread_param tort_v name)
{
  tort_symbol *value = tort_allocate(tort__mt(symbol), sizeof(tort_symbol));
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
#define tort_d_s(N) tort__s(N) = tort_symbol_make(#N);
#include "tort/d_s.h"
  return tort_(symbols);
}

