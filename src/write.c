#include "tort/core.h"


/********************************************************************/


#define tort_inspect_decl(name) static tort_v name (tort_thread_param tort_v rcvr, tort_v io)

#define IO (io ? io : tort_stdout)

#define printf(fmt, args...) tort_send(tort__s(printf), IO, fmt, ##args)


/********************************************************************/


tort_inspect_decl(_tort_object_write)
{
  const char *str = tort_object_name_(rcvr);
  if ( str ) {
    printf("%s", str);
  } else {
    printf("@object @%p", (void *) rcvr);
  }

  return tort_nil;
}


tort_inspect_decl(_tort_object_lisp_write)
{
  printf("(make <object> @%p)", (void *) rcvr);
  return tort_nil;
}


tort_inspect_decl(_tort_tagged_write)
{
  printf("%ld", (long) tort_tagged_data(rcvr));
  return tort_nil;
}


tort_inspect_decl(_tort_string_write)
{
  printf("\"%s\"", (char *) tort_string_data(rcvr));
  return tort_nil;
}


tort_inspect_decl(_tort_vector_write)
{
  printf("@vector { ");
  tort_vector_loop(rcvr, obj) {
    if ( obj_i > 0 ) printf(", ");
    tort_inspect(IO, obj);
  } tort_vector_loop_end(rcvr);
  printf(" }");
  return tort_nil;
}


tort_inspect_decl(_tort_vector_lisp_write)
{
  printf("#(");
  tort_vector_loop(rcvr, obj) {
    if ( obj_i > 0 ) printf(" ");
    tort_send(tort__s(lisp_write), obj, IO);
  } tort_vector_loop_end(rcvr);
  printf(")");
  return tort_nil;
}


tort_inspect_decl(_tort_symbol_write)
{
  if ( tort_ref(tort_symbol, rcvr)->name != tort_nil ) {
    printf("%s", (char *) tort_symbol_data(rcvr));
  } else {
    printf("@symbol @%p", (void*) rcvr);
  }
  return tort_nil;
}


tort_inspect_decl(_tort_symbol_lisp_write)
{
  if ( tort_ref(tort_symbol, rcvr)->name != tort_nil ) {
    printf("%s", (char *) tort_symbol_data(rcvr));
  } else {
    printf("(make <symbol> @%p)", (void*) rcvr);
  }
  return tort_nil;
}


tort_inspect_decl(_tort_nil_write)
{
  printf("nil");
  return tort_nil;
}


tort_inspect_decl(_tort_boolean_write)
{
  printf(rcvr == tort_false ? "false" : "true");
  return tort_nil;
}


tort_inspect_decl(_tort_boolean_lisp_write)
{
  printf(rcvr == tort_false ? "#f" : "#t");
  return tort_nil;
}


tort_inspect_decl(_tort_method_write)
{
  tort_v meth_name = tort_ref(tort_method, rcvr)->name;
  const char *meth_cstr = meth_name ? tort_symbol_data(meth_name) : "#<unknown>";
  printf("@method %s @%p", meth_cstr, (void *) tort_h_applyf(rcvr));
  return tort_nil;
}


tort_inspect_decl(_tort_message_write)
{
  tort_message *msg = tort_ref(tort_message, rcvr);
  printf("@message { ");
  tort_inspect(IO, msg->selector);
  printf(" ");
  tort_inspect(IO, msg->receiver);
  printf(" ");
  tort_inspect(IO, msg->method);
  printf("}");
 
  return tort_nil;
}


tort_inspect_decl(_tort_map_write)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry **x = map->entry, *entry;
  size_t entry_i = 0;

  const char *str = tort_object_name_(rcvr);
  if ( str ) {
    printf("%s", str);
  } else {
    printf("@map { ");
    while ( (entry = *(x ++)) ) {
      if ( entry_i > 0 ) printf(", ");
      tort_inspect(IO, entry->key);
      printf(" => ");
      tort_inspect(IO, entry->value);
      entry_i ++;
    }
    printf(" }");
  }
  return tort_nil;
}


tort_inspect_decl(_tort_map_lisp_write)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry **x = map->entry, *entry;
  size_t entry_i = 0;

  printf("(make <map> ");
  while ( (entry = *(x ++)) ) {
    if ( entry_i > 0 ) printf(" ");
    tort_send(tort__s(lisp_write), entry->key, IO);
    printf(" ");
    tort_send(tort__s(lisp_write), entry->value, IO);
    entry_i ++;
  }
  printf(")");
  return tort_nil;
}


tort_inspect_decl(_tort_eos_lisp_write)
{
  printf("#e");
  return tort_nil;
}


#undef printf
#undef IO


/********************************************************************/


void tort_runtime_initialize_write()
{
  _tort->_s_write  = tort_symbol_make("write");
  _tort->_s__inspect  = tort_symbol_make("_inspect");

  tort_add_method(_tort->_mt_object, "_inspect", _tort_object_write);
  tort_add_method(_tort->_mt_tagged, "_inspect", _tort_tagged_write);
  tort_add_method(_tort->_mt_string, "_inspect", _tort_string_write);
  tort_add_method(_tort->_mt_vector, "_inspect", _tort_vector_write);
  tort_add_method(_tort->_mt_symbol, "_inspect", _tort_symbol_write);
  tort_add_method(_tort->_mt_method, "_inspect", _tort_method_write);
  tort_add_method(_tort->_mt_message, "_inspect", _tort_message_write);
  tort_add_method(_tort->_mt_nil,    "_inspect", _tort_nil_write);
  tort_add_method(_tort->_mt_map,    "_inspect", _tort_map_write);
  tort_add_method(_tort->_mt_boolean, "_inspect", _tort_boolean_write);

  _tort->_s_lisp_write  = tort_symbol_make("lisp_write");

  tort_add_method(_tort->_mt_object, "lisp_write", _tort_object_lisp_write);
  tort_add_method(_tort->_mt_tagged, "lisp_write", _tort_tagged_write);
  tort_add_method(_tort->_mt_string, "lisp_write", _tort_string_write);
  tort_add_method(_tort->_mt_vector, "lisp_write", _tort_vector_lisp_write);
  tort_add_method(_tort->_mt_symbol, "lisp_write", _tort_symbol_lisp_write);
  tort_add_method(_tort->_mt_method, "lisp_write", _tort_method_write);
  tort_add_method(_tort->_mt_message, "lisp_write", _tort_message_write);
  tort_add_method(_tort->_mt_nil,    "lisp_write", _tort_nil_write);
  tort_add_method(_tort->_mt_map,    "lisp_write", _tort_map_lisp_write);
  tort_add_method(_tort->_mt_boolean, "lisp_write", _tort_boolean_lisp_write);
  tort_add_method(_tort->_mt_eos,    "lisp_write", _tort_eos_lisp_write);
}

