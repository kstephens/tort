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

#define tort_taggedQ(X)     ((X) & 1)
#define tort_tagged_box(V)  ((((long) (V)) << 1) | 1)
#define tort_tagged_data(X) (((long) (X)) >> 1)

#define tort_i(V) tort_tagged_box(V)
#define tort_I(X) tort_tagged_data(X)

#define tort_error_decl(X)  void X (const char *format, va_list vap)

#define tort_lookup_decl(X) tort_val X (tort_val _tort_message, tort_val rcvr, ...)
#define tort_apply_decl(X)  tort_val X (tort_val _tort_message, tort_val rcvr, ...)

#ifndef TORT_ALLOC_DEBUG
#define TORT_ALLOC_DEBUG 0
#endif

typedef
struct tort_header {
#if TORT_ALLOC_DEBUG
  const char *alloc_file;
  int alloc_line;
  unsigned long alloc_id;
#endif
  tort_apply_decl((*applyf));
  tort_lookup_decl((*lookupf));
  tort_val mtable;
} tort_header;

static tort_header tort_h_ref_ (tort_val X)
{
  return ((struct tort_header*)tort_ref(tort_object, X))[-1];
}

#define tort_h_ref(X)    ((struct tort_header*)tort_ref(tort_object, X))[-1]
#define tort_h_tagged(X) _tort->_tagged_header
#define tort_h(X)        ( tort_taggedQ(X) ? tort_h_tagged(X) : tort_h_ref(X) )

#define tort_applyf(X) tort_h(X).applyf
#define tort_lookupf(X) tort_h(X).lookupf
#define tort_mtable(X) tort_h(X).mtable

typedef
struct tort_object {
  tort_val *slots;
  size_t nslots;
  tort_val cmp;
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
  size_t alloc_size;
} tort_string;

#define tort_string_data(X) tort_ref(tort_string, X)->data
#define tort_string_size(X) tort_ref(tort_string, X)->size
#define tort_string_alloc_size(X) tort_ref(tort_string, X)->alloc_size

tort_val tort_string_new_cstr(const char *str);

typedef
struct tort_vector {
  tort_val *data;
  size_t size;
  size_t alloc_size;
} tort_vector;

#define tort_vector_data(X) tort_ref(tort_vector, X)->data
#define tort_vector_size(X) tort_ref(tort_vector, X)->size
#define tort_vector_alloc_size(X) tort_ref(tort_vector, X)->alloc_size

tort_val tort_vector_new(const tort_val *d, size_t s);

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
  tort_val previous_message;
} tort_message;

typedef struct _tort_message_data {
  struct tort_header _hdr;
  struct tort_message _msg;
} _tort_message_data;

typedef
struct tort_io {
  FILE *fp;
  tort_val mode;
  int popen;
} tort_io;

typedef
struct tort_runtime {
  tort_val nil;
  tort_val string_null, vector_null;
  tort_val symbols;

  tort_val message;

  tort_val _in_error;
  tort_error_decl((*error));
  tort_error_decl((*fatal));

  tort_header _tagged_header;

  tort_val _mt_map;
  tort_val _mt_object;
  tort_val _mt_string;
  tort_val _mt_vector;
  tort_val _mt_symbol;
  tort_val _mt_method;
  tort_val _mt_message;
  tort_val _mt_nil;
  tort_val _mt_tagged;
  tort_val _mt_io;

  tort_val _s_new;
  tort_val _s_clone;
  tort_val _s_lookup;
  tort_val _s_apply;
  tort_val _s_get;
  tort_val _s_get_key;
  tort_val _s_set;
  tort_val _s_value;
  tort_val _s_true;
  tort_val _s_false;
  tort_val _s_size;
  tort_val _s_alloc_size;

  /* io */
  tort_val _s_create;
  tort_val _s_open;
  tort_val _s_popen;
  tort_val _s_close;
  tort_val _s_read;
  tort_val _s_write;
  tort_val _s_printf;
  tort_val _s_eof;
  tort_val _s_error;

  /* io */
  tort_val _io_stdin;
  tort_val _io_stdout;
  tort_val _io_stderr;

  tort_val _initialized;
} tort_runtime;


#define tort_stdin  (_tort->_io_stdin)
#define tort_stdout (_tort->_io_stdout)
#define tort_stderr (_tort->_io_stderr)
#define tort_write(obj, io) tort_send(tort_s(write), obj, io)

#define tort_send(SEL, RCVR, ARGS...)					\
  ({									\
    _tort_message_data __msg = {					\
      { _tort_message_applyf, _tort_message_lookupf, _tort->_mt_message }, \
      { (SEL), (RCVR), tort_nil, _tort_message }			\
    };									\
    tort_val __msg_val = tort_ref_box(&__msg._msg);			\
    _tort_message = __msg_val;						\
    tort_lookupf(__msg_val)(0, __msg_val);				\
    tort_applyf(__msg._msg.method)(__msg_val, __msg._msg.receiver , ## ARGS); \
   })
 
extern tort_runtime *_tort;
extern tort_val _tort_message; /* catch for top-level messages. */

#define tort_nil _tort->nil
#define tort_string_null _tort->string_null
#define tort_vector_null _tort->vector_null

#define tort_true tort_s(true)
#define tort_false tort_s(false)

void *tort_malloc(size_t size);
void *tort_realloc(void *ptr, size_t size);

tort_val _tort_map_initialize(tort_val _tort_message, tort_val rcvr);
tort_val _tort_map_set(tort_val _tort_message, tort_val rcvr, tort_val key, tort_val value);
tort_map_entry *_tort_map_get_entry(tort_val _tort_message, tort_val rcvr, tort_val key);
tort_val _tort_map_get(tort_val _tort_message, tort_val rcvr, tort_val key);
tort_val tort_map_create();

tort_lookup_decl(_tort_object_lookupf);
tort_apply_decl(_tort_object_applyf);

tort_lookup_decl(_tort_message_lookupf);
tort_apply_decl(_tort_message_applyf);

#if TORT_ALLOC_DEBUG
tort_val _tort_allocate (const char *alloc_file, int alloc_line, tort_val _tort_message, tort_val rcvr, size_t size, tort_val meth_table);
#define tort_allocate(_1, _2, _3, _4) _tort_allocate(__FILE__, __LINE__, _1, _2, _3, _4)
#else
tort_val _tort_allocate (tort_val _tort_message, tort_val rcvr, size_t size, tort_val meth_table);
#define tort_allocate(_1, _2, _3, _4) _tort_allocate(_1, _2, _3, _4)
#endif

tort_val tort_symbol_make (const char *string);
#define tort_s(X) tort_symbol_make(#X)

tort_val tort_method_make (tort_apply_decl((*applyf)));

tort_val tort_add_method(tort_val map, const char *name, void *applyf);

tort_val tort_runtime_create ();


#endif

