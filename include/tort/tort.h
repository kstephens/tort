#ifndef _tort_tort_h
#define _tort_tort_h

/*
 * tort - tiny object run-time
 */

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h> /* memset() */


typedef void* tort_v;

#define tort_ref(T, X)      ((struct T *)(X))
#define tort_ref_box(PTR)   ((tort_v)(PTR))

#define tort_taggedQ(X)     ((long)(X) & 1)
#define tort_tagged_box(V)  ((tort_v) ((((long) (V)) << 1) | 1))
#define tort_tagged_data(X) (((long) (X)) >> 1)

#define tort_i(V) tort_tagged_box(V)
#define tort_I(X) tort_tagged_data(X)

#define tort_error_decl(X)  tort_v X (const char *format, va_list vap)

#define tort_lookup_decl(X) tort_v X (tort_v _tort_message, tort_v rcvr, ...)
#define tort_apply_decl(X)  tort_v X (tort_v _tort_message, tort_v rcvr, ...)

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
  size_t alloc_size; /** allocated size, not including this header */
  tort_lookup_decl((*lookupf));
  tort_apply_decl((*applyf));
  tort_v mtable; /** The object's method table. */
} tort_header;

#define tort_h_ref(X)    (((struct tort_header*) tort_ref(tort_object, X)) - 1)
#define tort_h_tagged(X) &_tort->_tagged_header
#define tort_h(X)        ( tort_taggedQ(X) ? tort_h_tagged(X) : tort_h_ref(X) )

#define tort_h_applyf(X) tort_h(X)->applyf
#define tort_h_lookupf(X) tort_h(X)->lookupf
#define tort_h_mtable(X) tort_h(X)->mtable

typedef
struct tort_object {
  tort_v *slots;
  size_t nslots;
  tort_v cmp;
} tort_object;

typedef 
struct tort_map_entry {
  tort_v key;
  tort_v value;
} tort_map_entry;

typedef
struct tort_map {
  tort_map_entry **entry;
  size_t entry_n;
} tort_map;

typedef
struct tort_mtable {
  tort_map _map;
  tort_v delegate;
} tort_mtable;

typedef
struct tort_string { /* Same layout as tort_vector. */
  char *data;
  size_t size;
  size_t alloc_size;
} tort_string;

#define tort_string_data(X) tort_ref(tort_string, X)->data
#define tort_string_size(X) tort_ref(tort_string, X)->size
#define tort_string_alloc_size(X) tort_ref(tort_string, X)->alloc_size

tort_v tort_string_new(const char *d, size_t s);
tort_v tort_string_new_cstr(const char *str);

typedef
struct tort_vector { /* Same layout as tort_string. */
  tort_v *data;
  size_t size;
  size_t alloc_size;
} tort_vector;

#define tort_vector_data(X) tort_ref(tort_vector, X)->data
#define tort_vector_size(X) tort_ref(tort_vector, X)->size
#define tort_vector_alloc_size(X) tort_ref(tort_vector, X)->alloc_size

#define tort_vector_loop(X, V) \
  do {									\
  size_t V##_i;								\
  for ( V##_i = 0; V##_i < tort_vector_size(X); ++ V##_i ) {		\
  tort_v V = tort_vector_data(X)[V##_i];

#define tort_vector_loop_end(X)			\
  }						\
    } while ( 0 )

tort_v tort_vector_new(const tort_v *d, size_t s);

typedef
struct tort_symbol {
  tort_v name;
} tort_symbol;

#define tort_symbol_data(X) tort_string_data(tort_ref(tort_symbol, X)->name)

typedef
struct tort_method {
  tort_v name;
  tort_v data;
} tort_method;

typedef
struct tort_message {
  tort_v selector;
  tort_v receiver;
  tort_v method;
  tort_v previous_message;
} tort_message;

typedef struct _tort_message_data {
  struct tort_header _hdr;
  struct tort_message _msg;
} _tort_message_data;

typedef
struct tort_io {
  FILE *fp;
  tort_v name;
  tort_v mode;
  int flags;
} tort_io;

typedef
struct tort_runtime {
  tort_v nil;
  tort_v string_null, vector_null;
  tort_v b_true, b_false;
  tort_v symbols;
  tort_v root;

  tort_v message;

  tort_v _in_error;
  tort_error_decl((*error));
  tort_error_decl((*fatal));

  tort_header _tagged_header;

  int _argc;
  char **_argv;
  char **_env;

  tort_v _mt_object;
  tort_v _mt_string;
  tort_v _mt_vector;
  tort_v _mt_map;
  tort_v _mt_mtable;
  tort_v _mt_symbol;
  tort_v _mt_method;
  tort_v _mt_message;
  tort_v _mt_nil;
  tort_v _mt_tagged;
  tort_v _mt_io;
  tort_v _mt_boolean;
  tort_v _mt_eos;
  tort_v _mt_block;
  tort_v _mt_pair;

  tort_v _s_new;
  tort_v _s_clone;
  tort_v _s_lookup;
  tort_v _s_apply;
  tort_v _s_get;
  tort_v _s_get_key;
  tort_v _s_set;
  tort_v _s_value;
  tort_v _s_true;
  tort_v _s_false;
  tort_v _s_size;
  tort_v _s_alloc_size;
  tort_v _s_map;
  tort_v _s_each;

  /* io */
  tort_v _s_create;
  tort_v _s___create;
  tort_v _s_open;
  tort_v _s_popen;
  tort_v _s_close;
  tort_v _s_read;
  tort_v _s___write;
  tort_v _s_write;
  tort_v _s_printf;
  tort_v _s_eof;
  tort_v _s_error;
  tort_v _s__inspect;

  tort_v _s_backtrace;
  tort_v _s_backtrace_size;

  tort_v _s_format;

  /* gc */
  tort_v _s___finalize;
  tort_v _s___register_finalizer;

  /* lisp */
  tort_v _s_lisp_write;
  tort_v _s_lisp_read;
  tort_v _s_set_cdrE;
  tort_v _s_list_TO_vector;
  tort_v _s_DOT;

  /* io */
  tort_v _io_stdin;
  tort_v _io_stdout;
  tort_v _io_stderr;
  tort_v _io_eos;

  /* symtab */
  tort_v _symtab;

  tort_v _initialized;
} tort_runtime;


#define tort_stdin  (_tort->_io_stdin)
#define tort_stdout (_tort->_io_stdout)
#define tort_stderr (_tort->_io_stderr)
#define tort_eos    (_tort->_io_eos)

#define tort_write(io, str) tort_send(tort__s(__write), io, str)
#define tort_inspect(io, obj) tort_send(tort__s(_inspect), obj, io)
#define tort_printf(io, fmt, args...) tort_send(tort__s(printf), io, fmt, ## args)
#define tort_flush(io) tort_send(tort_s(flush), io)

#define _tort_send_RCVR(RCVR, ARGS...)(RCVR)
#define _tort_send_ARGS(RCVR, ARGS...)ARGS
#define _tort_send_RCVR_ARGS(RCVR, EATEN, ARGS...)RCVR, ##ARGS
#define _tort_send(SEL, RCVR_AND_ARGS...)				\
  ({									\
    _tort_message_data __tort_msg = {					\
      { sizeof(tort_message), _tort_object_lookupf, _tort_object_applyf, _tort->_mt_message }, \
      { (SEL), _tort_send_RCVR(RCVR_AND_ARGS), tort_nil, _tort_message } \
    };									\
    tort_v __tort_msg_val = tort_ref_box(&__tort_msg._msg);			\
    tort_h_lookupf(__tort_msg._msg.receiver)(__tort_msg_val, __tort_msg._msg.receiver); \
    __tort_msg_val = tort_h_applyf(__tort_msg._msg.method)(__tort_msg_val, _tort_send_RCVR_ARGS(__tort_msg._msg.receiver, RCVR_AND_ARGS)); \
    memset(&__tort_msg, 0, sizeof(__tort_msg)); \
    __tort_msg_val; \
  })
#define tort_send(SEL, RCVR_AND_ARGS...)_tort_send(SEL, RCVR_AND_ARGS)

extern tort_runtime *_tort;
extern tort_v _tort_message; /* catch for top-level messages. */

#define tort_nil _tort->nil
#define tort_string_null _tort->string_null
#define tort_vector_null _tort->vector_null

#define tort_true  _tort->b_true
#define tort_false _tort->b_false

void *tort_malloc(size_t size);
void *tort_realloc(void *ptr, size_t size);

tort_map_entry *_tort_map_get_entry(tort_v _tort_message, tort_v rcvr, tort_v key);
tort_v tort_map_create();

tort_v tort_mtable_create();

tort_lookup_decl(_tort_object_lookupf);
tort_apply_decl(_tort_object_applyf);

#if TORT_ALLOC_DEBUG
tort_v _tort_allocate (const char *alloc_file, int alloc_line, tort_v _tort_message, tort_v rcvr, size_t size, tort_v meth_table);
#define tort_allocate(_1, _2, _3, _4) _tort_allocate(__FILE__, __LINE__, _1, _2, _3, _4)
#else
tort_v _tort_allocate (tort_v _tort_message, tort_v rcvr, size_t size, tort_v meth_table);
#define tort_allocate(_1, _2, _3, _4) _tort_allocate(_1, _2, _3, _4)
#endif

tort_v tort_symbol_make (const char *string);

#define tort_s(X) tort_symbol_make(#X)
#define tort__s(X) _tort->_s_##X

#define tort__mt(X) _tort->_mt_##X

tort_v tort_object_make ();

tort_v tort_method_make (tort_apply_decl((*applyf)));

tort_v tort_add_method(tort_v map, const char *name, void *applyf);

tort_v tort_runtime_create_ (int *argcp, char ***argvp, char ***envp);
#define tort_runtime_create() tort_runtime_create_(&argc, &argv, &environ)

tort_v tort_fatal (const char *format, ...);
tort_v tort_error (const char *format, ...);
tort_v tort_error_message(const char *format, ...);

const char *tort_object_name_(tort_v val);
const char *tort_object_name(tort_v val);

extern int _tort_gc_mode;
void tort_gc_collect();
void tort_gc_dump_stats();
void tort_gc_invoke_finalizers();

#endif

