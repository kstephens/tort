#include "tort/core.h"


/********************************************************************/


#define IO (io ? io : tort_stdout)

#define printf(fmt, args...) tort_send(tort__s(printf), IO, fmt, ##args)


/********************************************************************/


tort_v _tort_object__inspect(tort_thread_param tort_v rcvr, tort_v io)
{
  const char *str = tort_object_name_(rcvr);
  if ( str ) {
    printf("%s", str);
  } else {
    printf("@object @%p", (void *) rcvr);
  }

  return tort_nil;
}


tort_v _tort_object_lisp_write(tort_thread_param tort_v rcvr, tort_v io)
{
  printf("(make <object> @%p)", (void *) rcvr);
  return tort_nil;
}


tort_v _tort_tagged__inspect(tort_thread_param tort_v rcvr, tort_v io)
{
  printf("%ld", (long) tort_tagged_data(rcvr));
  return tort_nil;
}


tort_v _tort_string__inspect(tort_thread_param tort_v rcvr, tort_v io)
{
  printf("\"%s\"", (char *) tort_string_data(rcvr));
  return tort_nil;
}


tort_v _tort_vector__inspect(tort_thread_param tort_v rcvr, tort_v io)
{
  printf("@vector { ");
  tort_vector_loop(rcvr, obj) {
    if ( obj_i > 0 ) printf(", ");
    tort_inspect(IO, obj);
  } tort_vector_loop_end(rcvr);
  printf(" }");
  return tort_nil;
}


tort_v _tort_vector_lisp_write(tort_thread_param tort_v rcvr, tort_v io)
{
  printf("#(");
  tort_vector_loop(rcvr, obj) {
    if ( obj_i > 0 ) printf(" ");
    tort_send(tort__s(lisp_write), obj, IO);
  } tort_vector_loop_end(rcvr);
  printf(")");
  return tort_nil;
}


tort_v _tort_symbol__inspect(tort_thread_param tort_v rcvr, tort_v io)
{
  if ( tort_ref(tort_symbol, rcvr)->name != tort_nil ) {
    printf("%s", (char *) tort_symbol_data(rcvr));
  } else {
    printf("@symbol @%p", (void*) rcvr);
  }
  return tort_nil;
}


tort_v _tort_symbol_lisp_write(tort_thread_param tort_v rcvr, tort_v io)
{
  if ( tort_ref(tort_symbol, rcvr)->name != tort_nil ) {
    printf("%s", (char *) tort_symbol_data(rcvr));
  } else {
    printf("(make <symbol> @%p)", (void*) rcvr);
  }
  return tort_nil;
}


tort_v _tort_nil__inspect(tort_thread_param tort_v rcvr, tort_v io)
{
  printf("nil");
  return tort_nil;
}


tort_v _tort_boolean__inspect(tort_thread_param tort_v rcvr, tort_v io)
{
  printf(rcvr == tort_false ? "false" : "true");
  return tort_nil;
}


tort_v _tort_boolean_lisp_write(tort_thread_param tort_v rcvr, tort_v io)
{
  printf(rcvr == tort_false ? "#f" : "#t");
  return tort_nil;
}


tort_v _tort_method__inspect(tort_thread_param tort_v rcvr, tort_v io)
{
  tort_v meth_name = tort_ref(tort_method, rcvr)->name;
  const char *meth_cstr = meth_name ? tort_symbol_data(meth_name) : "#<unknown>";
  printf("@method %s @%p", meth_cstr, (void *) tort_h_applyf(rcvr));
  return tort_nil;
}


tort_v _tort_message__inspect(tort_thread_param tort_v rcvr, tort_v io)
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


tort_v _tort_map__inspect(tort_thread_param tort_v rcvr, tort_v io)
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


tort_v _tort_map_lisp_write(tort_thread_param tort_v rcvr, tort_v io)
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


tort_v _tort_eos_lisp_write(tort_thread_param tort_v rcvr, tort_v io)
{
  printf("#e");
  return tort_nil;
}


#undef printf
#undef IO


/********************************************************************/


void tort_runtime_initialize_write()
{
  /* Reused methods. */
  tort_add_method(_tort->_mt_tagged, "lisp_write", _tort_tagged__inspect);
  tort_add_method(_tort->_mt_string, "lisp_write", _tort_string__inspect);
  tort_add_method(_tort->_mt_method, "lisp_write", _tort_method__inspect);
  tort_add_method(_tort->_mt_message, "lisp_write", _tort_message__inspect);
  tort_add_method(_tort->_mt_nil,    "lisp_write", _tort_nil__inspect);
}

