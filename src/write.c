#include "tort/tort.h"

#include <stdio.h>

#define tort_write_decl(name) tort_val name (tort_val message, tort_val rcvr, tort_val io)

#define IO (io ? io : tort_stdout)

#define printf(fmt, args...) tort_send(tort_s(printf), IO, fmt, ##args)

tort_write_decl(_tort_object_write)
{
  printf("!object @%p ", (void *) rcvr);
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
  size_t i = 0;

  printf("!vector { ");
  while ( i < tort_vector_size(rcvr) ) {
    tort_write(tort_vector_data(rcvr)[i], IO);
    printf(", ");
  }
  printf(" } ");
  return tort_nil;
}


tort_write_decl(_tort_symbol_write)
{
  printf("%s", (char *) tort_symbol_data(rcvr));
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
  printf("!method %s @%p ", meth_cstr, (void *) tort_h_applyf(rcvr));
  return tort_nil;
}


tort_write_decl(_tort_message_write)
{
  tort_message *msg = tort_ref(tort_message, rcvr);
  printf("!message { ");
  tort_write(msg->selector, IO);
  printf(" ");
  tort_write(msg->receiver, IO);
  printf(" ");
  tort_write(msg->method, IO);
  printf(" } ");
 
  return tort_nil;
}


tort_write_decl(_tort_map_write)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry **x = map->entry, *entry;

  printf("!map { ");
  while ( (entry = *(x ++)) ) {
    tort_write(entry->key, IO);
    printf(" => ");
    tort_write(entry->value, IO);
    printf(", ");
  }
  printf(" } ");
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
}

