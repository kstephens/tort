#ifndef _tort_INTERNAL_H
#define _tort_INTERNAL_H

#include "tort/tort.h"

extern tort_val _tort_object___message(tort_val _tort_message, tort_val rcvr)
;
extern tort_val _tort_message_backtrace(tort_val _tort_message, tort_val rcvr)
;
extern tort_val _tort_debugger_start(tort_val _tort_message, tort_val rcvr)
;
extern tort_val _tort_object_name(tort_val _tort_message, tort_val rcvr)
;
extern tort_val _tort_fatal(const char *format, va_list vap)
;
extern tort_val _tort_error(const char *format, va_list vap)
;
extern tort_val _tort_io_create(tort_val message, tort_val rcvr, FILE *fp)
;
extern tort_val _tort_io_open(tort_val message, tort_val rcvr, tort_val name, tort_val mode)
;
extern tort_val _tort_io_popen(tort_val message, tort_val rcvr, tort_val name, tort_val mode)
;
extern tort_val _tort_io_close(tort_val message, tort_val rcvr)
;
extern tort_val _tort_io_write(tort_val message, tort_val rcvr, tort_val buf)
;
extern tort_val _tort_io_flush(tort_val message, tort_val rcvr)
;
extern tort_val _tort_io_printf(tort_val message, tort_val rcvr, const char *fmt, ...)
;
extern tort_val _tort_io_read(tort_val message, tort_val rcvr, tort_val buf)
;
extern tort_val _tort_io_eof(tort_val message, tort_val rcvr)
;
extern tort_val _tort_io_error(tort_val message, tort_val rcvr)
;
extern tort_val _tort_list_size(tort_val _tort_message, tort_val rcvr) 
;
extern tort_val _tort_list_lisp_write(tort_val _tort_message, tort_val rcvr, tort_val io) 
;
extern tort_val _tort_string_to_number(tort_val s, int radix)
;
extern tort_val _tort_object_clone (tort_val _tort_message, tort_val rcvr)
;
extern tort_val _tort_object_identity (tort_val _tort_message, tort_val rcvr)
;
extern tort_val _tort_map_initialize(tort_val _tort_message, tort_val rcvr)
;
extern tort_val _tort_map_add(tort_val _tort_message, tort_val rcvr, tort_val key, tort_val value)
;
extern tort_val _tort_map_get(tort_val _tort_message, tort_val rcvr, tort_val key)
;
extern tort_val _tort_map_get_key(tort_val _tort_message, tort_val rcvr, tort_val value)
;
extern tort_val _tort_map_get_string(tort_val _tort_message, tort_val rcvr, tort_val key)
;
extern tort_val _tort_map_set(tort_val _tort_message, tort_val rcvr, tort_val key, tort_val value)
;
extern tort_val _tort_map_delete(tort_val _tort_message, tort_val rcvr, tort_val key)
;
extern tort_val _tort_map_size(tort_val _tort_message, tort_val rcvr)
;
extern tort_val _tort_map_clone(tort_val _tort_message, tort_val rcvr)
;
extern tort_val _tort_vector_new(tort_val _tort_message, tort_val rcvr, tort_val _size)
;
extern tort_val _tort_vector_clone (tort_val _tort_message, tort_val rcvr)
;
extern tort_val _tort_vector_get (tort_val _tort_message, tort_val rcvr, tort_val _i)
;
extern tort_val _tort_vector_set (tort_val _tort_message, tort_val rcvr, tort_val _i, tort_val _v)
;
extern tort_val _tort_vector_size (tort_val _tort_message, tort_val rcvr)
;
extern tort_val _tort_vector_alloc_size (tort_val _tort_message, tort_val rcvr)
;
extern tort_val _tort_vector_each (tort_val _tort_message, tort_val rcvr, tort_val block)
;
extern tort_val _tort_vector_map (tort_val _tort_message, tort_val rcvr, tort_val block)
;
extern tort_val _tort_string_new(tort_val _tort_message, tort_val rcvr, tort_val size)
;
extern tort_val _tort_string_clone (tort_val _tort_message, tort_val rcvr)
;
extern tort_val _tort_string_get (tort_val _tort_message, tort_val rcvr, tort_val _i)
;
extern tort_val _tort_string_set (tort_val _tort_message, tort_val rcvr, tort_val _i, tort_val _v)
;

#endif
