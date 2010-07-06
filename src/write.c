#include "tort/tort.h"

#include <stdio.h>

#define tort_write_decl(name) tort_val name (tort_val message, tort_val rcvr, tort_val io)

#define IO (io ? io : tort_stdout)

#define printf(fmt, args...) tort_send(tort__s(printf), IO, fmt, ##args)

tort_write_decl(_tort_object_write)
{
  printf("!object @%p", (void *) rcvr);
  return tort_nil;
}


tort_write_decl(_tort_object_format_lisp)
{
  printf("(make <object> @%p)", (void *) rcvr);
  return tort_nil;
}


tort_write_decl(_tort_tagged_write)
{
  printf("%ld", (long) tort_tagged_data(rcvr));
  return tort_nil;
}


tort_write_decl(_tort_string_write)
{
  printf("\"%s\"", (char *) tort_string_data(rcvr));
  return tort_nil;
}


tort_write_decl(_tort_vector_write)
{
  printf("!vector { ");
  tort_vector_loop(rcvr, obj) {
    if ( obj_i > 0 ) printf(", ");
    tort_write(IO, obj);
  } tort_vector_loop_end(rcvr);
  printf(" }");
  return tort_nil;
}


tort_write_decl(_tort_vector_format_lisp)
{
  printf("#(");
  tort_vector_loop(rcvr, obj) {
    if ( obj_i > 0 ) printf(" ");
    tort_send(tort__s(format_lisp), obj, IO);
  } tort_vector_loop_end(rcvr);
  printf(")");
  return tort_nil;
}


tort_write_decl(_tort_symbol_write)
{
  if ( tort_ref(tort_symbol, rcvr)->name != tort_nil ) {
    printf("%s", (char *) tort_symbol_data(rcvr));
  } else {
    printf("!symbol @%p", (void*) rcvr);
  }
  return tort_nil;
}


tort_write_decl(_tort_symbol_format_lisp)
{
  if ( tort_ref(tort_symbol, rcvr)->name != tort_nil ) {
    printf("%s", (char *) tort_symbol_data(rcvr));
  } else {
    printf("(make <symbol> @%p)", (void*) rcvr);
  }
  return tort_nil;
}


tort_write_decl(_tort_nil_write)
{
  printf("nil");
  return tort_nil;
}


tort_write_decl(_tort_method_write)
{
  tort_val meth_name = tort_ref(tort_method, rcvr)->name;
  const char *meth_cstr = meth_name ? tort_symbol_data(meth_name) : "#<unknown>";
  printf("!method %s @%p", meth_cstr, (void *) tort_h_applyf(rcvr));
  return tort_nil;
}


tort_write_decl(_tort_message_write)
{
  tort_message *msg = tort_ref(tort_message, rcvr);
  printf("!message { ");
  tort_write(IO, msg->selector);
  printf(" ");
  tort_write(IO, msg->receiver);
  printf(" ");
  tort_write(IO, msg->method);
  printf("}");
 
  return tort_nil;
}


tort_write_decl(_tort_map_write)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry **x = map->entry, *entry;
  size_t entry_i = 0;

  printf("!map { ");
  while ( (entry = *(x ++)) ) {
    if ( entry_i > 0 ) printf(", ");
    tort_write(IO, entry->key);
    printf(" => ");
    tort_write(IO, entry->value);
    entry_i ++;
  }
  printf(" }");
  return tort_nil;
}


tort_write_decl(_tort_map_format_lisp)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry **x = map->entry, *entry;
  size_t entry_i = 0;

  printf("(make <map> ");
  while ( (entry = *(x ++)) ) {
    if ( entry_i > 0 ) printf(" ");
    tort_send(tort__s(format_lisp), entry->key, IO);
    printf(" ");
    tort_send(tort__s(format_lisp), entry->value, IO);
    entry_i ++;
  }
  printf(")");
  return tort_nil;
}


#undef printf
#undef IO

void tort_runtime_initialize_write()
{
  _tort->_s_write  = tort_symbol_make("write");

  tort_add_method(_tort->_mt_object, "write", _tort_object_write);
  tort_add_method(_tort->_mt_tagged, "write", _tort_tagged_write);
  tort_add_method(_tort->_mt_string, "write", _tort_string_write);
  tort_add_method(_tort->_mt_vector, "write", _tort_vector_write);
  tort_add_method(_tort->_mt_symbol, "write", _tort_symbol_write);
  tort_add_method(_tort->_mt_method, "write", _tort_method_write);
  tort_add_method(_tort->_mt_message, "write", _tort_message_write);
  tort_add_method(_tort->_mt_nil,    "write", _tort_nil_write);
  tort_add_method(_tort->_mt_map,    "write", _tort_map_write);

  _tort->_s_format_lisp  = tort_symbol_make("format_lisp");
  tort_add_method(_tort->_mt_object, "format_lisp", _tort_object_format_lisp);
  tort_add_method(_tort->_mt_tagged, "format_lisp", _tort_tagged_write);
  tort_add_method(_tort->_mt_string, "format_lisp", _tort_string_write);
  tort_add_method(_tort->_mt_vector, "format_lisp", _tort_vector_format_lisp);
  tort_add_method(_tort->_mt_symbol, "format_lisp", _tort_symbol_write);
  tort_add_method(_tort->_mt_method, "format_lisp", _tort_method_write);
  tort_add_method(_tort->_mt_message, "format_lisp", _tort_message_write);
  tort_add_method(_tort->_mt_nil,    "format_lisp", _tort_nil_write);
  tort_add_method(_tort->_mt_map,    "format_lisp", _tort_map_format_lisp);

}

