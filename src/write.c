#include "tort/tort.h"

#include <stdio.h>


tort_apply_decl(_tort_object_write)
{
  printf("!object %p ", (void *) rcvr);
  return tort_nil;
}

tort_apply_decl(_tort_string_write)
{
  printf("\"%s\"", (char *) tort_string_data(rcvr));
  return tort_nil;
}


tort_apply_decl(_tort_symbol_write)
{
  printf("%s", (char *) tort_symbol_data(rcvr));
  return tort_nil;
}

tort_apply_decl(_tort_nil_write)
{
  printf("nil");
  return tort_nil;
}


tort_apply_decl(_tort_method_write)
{
  printf("!method %p ", (void *) tort_applyf(rcvr));
  return tort_nil;
}


tort_apply_decl(_tort_message_write)
{
  tort_message *msg = tort_ref(tort_message, rcvr);
  printf("!message { ");
  tort_send(_tort->_s_write, msg->selector);
  printf(" ");
  tort_send(_tort->_s_write, msg->receiver);
  printf(" ");
  tort_send(_tort->_s_write, msg->method);
  printf(" } ");
 
  return tort_nil;
}


tort_apply_decl(_tort_map_write)
{
  tort_map *map = tort_ref(tort_map, rcvr);
  tort_map_entry **x = map->entry, *entry;

  printf("!map { ");
  while ( entry = *(x ++) ) {
    tort_send(_tort->_s_write, entry->key);
    printf(" => ");
    tort_send(_tort->_s_write, entry->value);
    printf(", ");
  }
  printf(" } ");
  return tort_nil;
}


void tort_runtime_initialize_write()
{
  tort_add_method(_tort->_mt_object, "write", _tort_object_write);
  tort_add_method(_tort->_mt_string, "write", _tort_string_write);
  tort_add_method(_tort->_mt_symbol, "write", _tort_symbol_write);
  tort_add_method(_tort->_mt_method, "write", _tort_method_write);
  tort_add_method(_tort->_mt_message, "write", _tort_message_write);
  tort_add_method(_tort->_mt_nil,    "write", _tort_nil_write);
  tort_add_method(_tort->_mt_map,    "write", _tort_map_write);
}

