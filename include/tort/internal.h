#ifndef _tort_INTERNAL_H
#define _tort_INTERNAL_H

#include "tort/tort.h"

extern tort_v _tort_object___message(tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_message_backtrace(tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_debugger_start(tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_object_name(tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_fatal(const char *format, va_list vap)
;
extern tort_v _tort_error(const char *format, va_list vap)
;
extern tort_v _tort_object___register_finalizer(tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_io_create(tort_v _tort_message, tort_v rcvr, FILE *fp)
;
extern tort_v _tort_io_open(tort_v _tort_message, tort_v rcvr, tort_v name, tort_v mode)
;
extern tort_v _tort_io_popen(tort_v _tort_message, tort_v rcvr, tort_v name, tort_v mode)
;
extern tort_v _tort_io_close(tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_io___write(tort_v _tort_message, tort_v rcvr, tort_v str)
;
extern tort_v _tort_io_flush(tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_io_printf(tort_v _tort_message, tort_v rcvr, const char *fmt, ...)
;
extern tort_v _tort_io_read(tort_v _tort_message, tort_v rcvr, tort_v buf)
;
extern tort_v _tort_io_eof(tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_io_error(tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_io___finalize(tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_list_size(tort_v _tort_message, tort_v rcvr) 
;
extern tort_v _tort_list_lisp_write(tort_v _tort_message, tort_v rcvr, tort_v io) 
;
extern tort_v _tort_string_to_number(tort_v s, int radix)
;
extern tort_v _tort_object_clone (tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_object_identity (tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_map_initialize(tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_map_add(tort_v _tort_message, tort_v rcvr, tort_v key, tort_v value)
;
extern tort_v _tort_map_get(tort_v _tort_message, tort_v rcvr, tort_v key)
;
extern tort_v _tort_map_get_key(tort_v _tort_message, tort_v rcvr, tort_v value)
;
extern tort_v _tort_map_get_string(tort_v _tort_message, tort_v rcvr, tort_v key)
;
extern tort_v _tort_map_set(tort_v _tort_message, tort_v rcvr, tort_v key, tort_v value)
;
extern tort_v _tort_map_delete(tort_v _tort_message, tort_v rcvr, tort_v key)
;
extern tort_v _tort_map_size(tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_map_clone(tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_vector_new(tort_v _tort_message, tort_v rcvr, tort_v _size)
;
extern tort_v _tort_vector_clone (tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_vector_get (tort_v _tort_message, tort_v rcvr, tort_v _i)
;
extern tort_v _tort_vector_set (tort_v _tort_message, tort_v rcvr, tort_v _i, tort_v _v)
;
extern tort_v _tort_vector_size (tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_vector_alloc_size (tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_vector_each (tort_v _tort_message, tort_v rcvr, tort_v block)
;
extern tort_v _tort_vector_map (tort_v _tort_message, tort_v rcvr, tort_v block)
;
extern tort_v _tort_string_new(tort_v _tort_message, tort_v rcvr, tort_v size)
;
extern tort_v _tort_string_clone (tort_v _tort_message, tort_v rcvr)
;
extern tort_v _tort_string_get (tort_v _tort_message, tort_v rcvr, tort_v _i)
;
extern tort_v _tort_string_set (tort_v _tort_message, tort_v rcvr, tort_v _i, tort_v _v)
;

#endif
