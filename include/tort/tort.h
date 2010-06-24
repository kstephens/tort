#ifndef _tort_tort_h
#define _tort_tort_h

/*
 * tort - tiny object run-time
 */

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>


typedef unsigned long tort_val;

#define tort_ref(T, X)      ((struct T *)(X))
#define tort_ref_box(PTR)   ((tort_val)(PTR))

#define tort_error_decl(X)  void X (const char *format, va_list vap)

#define tort_lookup_decl(X) tort_val X (tort_val message, tort_val rcvr, ...)
#define tort_apply_decl(X)  tort_val X (tort_val message, tort_val rcvr, ...)

typedef
struct tort_header {
  tort_apply_decl((*applyf));
  tort_lookup_decl((*lookupf));
  tort_val mtable;
} tort_header;

#define tort_h(X) ((struct tort_header*)tort_ref(tort_object, X))[-1]
#define tort_applyf(X) tort_h(X).applyf
#define tort_lookupf(X) tort_h(X).lookupf
#define tort_mtable(X) tort_h(X).mtable

typedef
struct tort_object {
  tort_val *slots;
  size_t nslots;
} tort_object;

typedef 
struct tort_map_entry {
  tort_val key;
  tort_val value;
} tort_map_entry;

typedef
struct tort_map {
  tort_map_entry **entry;
  size_t entry_n;
} tort_map;

typedef
struct tort_string {
  char *data;
  size_t size;
} tort_string;

#define tort_string_data(X) tort_ref(tort_string, X)->data
#define tort_string_size(X) tort_ref(tort_string, X)->size

tort_val tort_string_new_cstr(const char *str);

typedef
struct tort_symbol {
  tort_val name;
} tort_symbol;

#define tort_symbol_data(X) tort_string_data(tort_ref(tort_symbol, X)->name)

typedef
struct tort_method {
  tort_val name;
} tort_method;

typedef
struct tort_message {
  tort_val selector;
  tort_val receiver;
  tort_val method;
} tort_message;

typedef struct _tort_message {
  struct tort_header _hdr;
  struct tort_message _msg;
} _tort_message;

typedef
struct tort_io {
  FILE *fp;
} tort_io;

typedef
struct tort_runtime {
  tort_val nil;
  tort_val symbols;
  tort_error_decl((*error));
  tort_error_decl((*fatal));
  tort_val _mt_map;
  tort_val _mt_object;
  tort_val _mt_string;
  tort_val _mt_symbol;
  tort_val _mt_method;
  tort_val _mt_message;
  tort_val _mt_nil;
  tort_val _mt_io;
  tort_val _s_new;
  tort_val _s_lookup;
  tort_val _s_apply;
  tort_val _s_get;
  tort_val _s_set;
  tort_val _s_value;
  /* io */
  tort_val _s_create;
  tort_val _s_open;
  tort_val _s_close;
  tort_val _s_write;
  tort_val _s_printf;
  /* io */
  tort_val _io_stdin;
  tort_val _io_stdout;
  tort_val _io_stderr;
} tort_runtime;


#define tort_send(SEL, RCVR, ARGS...)					\
  ({									\
    _tort_message __msg = {						\
      { _tort_message_applyf, _tort_message_lookupf, _tort->_mt_message }, \
      { (SEL), (RCVR), tort_nil }					\
    };									\
    tort_val __msg_val = tort_ref_box(&__msg._msg);			\
    tort_lookupf(__msg_val)(0, __msg_val);				\
    tort_applyf(__msg._msg.method)(__msg_val, __msg._msg.receiver , ## ARGS); \
   })
 
extern tort_runtime *_tort;
extern tort_val _tort_message_apply;
extern tort_val _tort_message_lookup;
extern tort_val _tort_message_mtable;
extern tort_val _tort_nil;

#define tort_nil _tort->nil

void *tort_malloc(size_t size);
void *tort_realloc(void *ptr, size_t size);

tort_val _tort_map_initialize(tort_val message, tort_val rcvr);
tort_val _tort_map_set(tort_val message, tort_val rcvr, tort_val key, tort_val value);
tort_map_entry *_tort_map_get_entry(tort_val message, tort_val rcvr, tort_val key);
tort_val _tort_map_get(tort_val message, tort_val rcvr, tort_val key);

tort_lookup_decl(_tort_object_lookupf);
tort_apply_decl(_tort_object_applyf);

tort_lookup_decl(_tort_message_lookupf);
tort_apply_decl(_tort_message_applyf);

tort_val tort_allocate (tort_val message, tort_val rcvr, size_t size, tort_val meth_table);

tort_val tort_symbol_make (const char *string);
tort_val tort_method_make (tort_apply_decl((*applyf)));

tort_val tort_add_method(tort_val rcvr, const char *name, void *applyf);

tort_val tort_runtime_create ();


#endif

