#ifndef _tort_INTERNAL_H
#define _tort_INTERNAL_H

#include "tort/tort.h"


/* map.c */
tort_map_entry *_tort_map_get_entry(tort_thread_param tort_v rcvr, tort_v key);

tort_map_entry *_tort_map_get_entry_string(tort_thread_param tort_v rcvr, tort_v key);

tort_map_entry *_tort_map_get_entry_string(tort_thread_param tort_v rcvr, tort_v key);

tort_map_entry *_tort_map_get_entry_cstr(tort_thread_param tort_v rcvr, const char *key);

extern tort_v _tort_object___message(tort_thread_param tort_v rcvr)
;
extern tort_v _tort_message_backtrace(tort_thread_param tort_v rcvr)
;
extern tort_v _tort_debugger_start(tort_thread_param tort_v rcvr)
;
extern tort_v _tort_object_name(tort_thread_param tort_v rcvr)
;
extern tort_v _tort_fatal(const char *format, va_list vap)
;
extern tort_v _tort_error(const char *format, va_list vap)
;
extern tort_v _tort_object___register_finalizer(tort_thread_param tort_v rcvr)
;
extern tort_v _tort_io___create(tort_thread_param tort_v rcvr, FILE *fp)
;
extern tort_v _tort_io_open(tort_thread_param tort_v rcvr, tort_v name, tort_v mode)
;
extern tort_v _tort_io_popen(tort_thread_param tort_v rcvr, tort_v name, tort_v mode)
;
extern tort_v _tort_io_close(tort_thread_param tort_v rcvr)
;
extern tort_v _tort_io___write(tort_thread_param tort_v rcvr, tort_v str)
;
extern tort_v _tort_io_flush(tort_thread_param tort_v rcvr)
;
extern tort_v _tort_io_printf(tort_thread_param tort_v rcvr, const char *fmt, ...)
;
extern tort_v _tort_io_read(tort_thread_param tort_v rcvr, tort_v buf)
;
extern tort_v _tort_io_eof(tort_thread_param tort_v rcvr)
;
extern tort_v _tort_io_error(tort_thread_param tort_v rcvr)
;
extern tort_v _tort_io___finalize(tort_thread_param tort_v rcvr)
;
extern tort_v _tort_list_size(tort_thread_param tort_v rcvr) 
;
extern tort_v _tort_list_lisp_write(tort_thread_param tort_v rcvr, tort_v io) 
;
extern tort_v _tort_string_to_number(tort_v s, int radix)
;
extern tort_v _tort_map_initialize(tort_thread_param tort_v rcvr)
;
extern tort_v _tort_map_add(tort_thread_param tort_v rcvr, tort_v key, tort_v value)
;
extern tort_v _tort_map_get(tort_thread_param tort_v rcvr, tort_v key)
;
extern tort_v _tort_map_get_key(tort_thread_param tort_v rcvr, tort_v value)
;
extern tort_v _tort_map_get_string(tort_thread_param tort_v rcvr, tort_v key)
;
extern tort_v _tort_map_set(tort_thread_param tort_v rcvr, tort_v key, tort_v value)
;
extern tort_v _tort_map_delete(tort_thread_param tort_v rcvr, tort_v key)
;
extern tort_v _tort_map_size(tort_thread_param tort_v rcvr)
;
extern tort_v _tort_map_clone(tort_thread_param tort_v rcvr)
;
extern tort_v _tort_string_new(tort_thread_param tort_v rcvr, tort_v size)
;
extern tort_v _tort_string_clone (tort_thread_param tort_v rcvr)
;
extern tort_v _tort_string_get (tort_thread_param tort_v rcvr, tort_v _i)
;
extern tort_v _tort_string_set (tort_thread_param tort_v rcvr, tort_v _i, tort_v _v)
;
extern tort_v _tort_object_clone (tort_thread_param tort_v rcvr)
;
extern tort_v _tort_object_identity (tort_thread_param tort_v rcvr)
;
extern tort_v _tort_mtable_add_method (tort_thread_param tort_v map, tort_v sym, tort_v func)
;
extern tort_v _tort_vector_new(tort_thread_param tort_v rcvr, tort_v _size)
;
extern tort_v _tort_vector_clone (tort_thread_param tort_v rcvr)
;
extern tort_v _tort_vector_get (tort_thread_param tort_v rcvr, tort_v _i)
;
extern tort_v _tort_vector_set (tort_thread_param tort_v rcvr, tort_v _i, tort_v _v)
;
extern tort_v _tort_vector_size (tort_thread_param tort_v rcvr)
;
extern tort_v _tort_vector_alloc_size (tort_thread_param tort_v rcvr)
;
extern tort_v _tort_vector_each (tort_thread_param tort_v rcvr, tort_v block)
;
extern tort_v _tort_vector_map (tort_thread_param tort_v rcvr, tort_v block)
;

#endif
