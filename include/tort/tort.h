#ifndef _tort_tort_h
#define _tort_tort_h 1

/*
 * tort - tiny object run-time
 */

#include <stdlib.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h> /* memset() */


#include "tort/config.h"

typedef void* tort_v;

#define tort_ref(T, X)      ((struct T *)(X))
#define tort_ref_box(PTR)   ((tort_v)(PTR))

#define tort_taggedQ(X)     ((size_t)(X) & 1)
#define tort_tagged_box(V)  ((tort_v) ((((ssize_t) (V)) << 1) | 1))
#define tort_tagged_data(X) (((ssize_t) (X)) >> 1)

#define tort_i(V) tort_tagged_box(V)
#define tort_I(X) tort_tagged_data(X)

#define tort_error_decl(X)  tort_v X (const char *format, va_list vap)

typedef struct tort_symbol tort_symbol;

typedef
struct tort_message {
  tort_symbol *selector;
  tort_v receiver;
  tort_v method;
  tort_v previous_message;
  tort_v fiber;
} tort_message;

#define tort_thread_param tort_message *_tort_message,
#define tort_thread_arg          _tort_message,

#define tort_lookup_decl(X) tort_v X (tort_thread_param tort_v rcvr, ...)
#define tort_apply_decl(X)  tort_v X (tort_thread_param tort_v rcvr, ...)

typedef
struct tort_header {
  size_t alloc_size; /** allocated size, not including this header */
  tort_lookup_decl((*lookupf));
  tort_apply_decl((*applyf));
#if TORT_ALLOC_DEBUG
  const char *alloc_file;
  int alloc_line;
  size_t alloc_id;
#endif
  tort_v mtable; /** The object's method table. */
} tort_header;

#define tort_h_nil(X)    &tort_(nil_header)
#define tort_h_tagged(X) &tort_(tagged_header)
#define tort_h_ref(X)    (((struct tort_header*) tort_ref(tort_object, X)) - 1)

#if TORT_NIL_IS_ZERO
#define tort_nil ((tort_v) 0)
#define tort_nilQ(X) ((X) == 0)
#define tort_h(X)        ( tort_nilQ(X) ? tort_h_nil(X) : tort_taggedQ(X) ? tort_h_tagged(X) : tort_h_ref(X) )
#else
#define tort_nil tort_(nil)
#define tort_nilQ(X) ((X) == tort_nil)
#define tort_h(X)        ( tort_taggedQ(X) ? tort_h_tagged(X) : tort_h_ref(X) )
#endif

#define tort_h_applyf(X)  tort_h(X)->applyf
#define tort_h_lookupf(X) tort_h(X)->lookupf
#define tort_h_mtable(X)  tort_h(X)->mtable

typedef
struct tort_object {
  tort_v *slots;
  size_t nslots;
  tort_v cmp;
} tort_object;

typedef 
struct tort_vector_base { /* Same layout as tort_vector, tort_string. */
  void *data;
  size_t size;
  size_t alloc_size;
  size_t element_size;
} tort_vector_base;

#define tort_vector_base_data(X) tort_ref(tort_vector_base, X)->data
#define tort_vector_base_size(X) tort_ref(tort_vector_base, X)->size
#define tort_vector_base_alloc_size(X) tort_ref(tort_vector_base, X)->alloc_size
#define tort_vector_base_element_size(X) tort_ref(tort_vector_base, X)->element_size

tort_v tort_vector_base_new(tort_v mtable, const void *d, size_t s, size_t element_size);

typedef
struct tort_vector { 
  tort_v *data;
  size_t size;
  size_t alloc_size;
  size_t element_size; /* sizeof(tort_v) */
} tort_vector;

#define tort_vector_data(X) ((tort_v*)tort_vector_base_data(X))
#define tort_vector_size(X) tort_vector_base_size(X)
#define tort_vector_alloc_size(X) tort_vector_base_alloc_size(X)

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
struct tort_map_entry {
  tort_v key;
  tort_v value;
} tort_map_entry;

typedef
struct tort_map { /* Same layout as tort_vector_base. */
  tort_vector_base _vector_base;
} tort_map;

#define tort_map_data(X) ((tort_map_entry**)tort_vector_base_data(X))
#define tort_map_size(X) tort_vector_base_size(X)

#define tort_map_EACH(m, me)					\
  {								\
  tort_map_entry **me##p = tort_map_data(m), *me;		\
  while ( (me = *(me##p ++)) ) {
#define tort_map_EACH_END() }}

typedef
struct tort_string { /* Same layout as tort_vector_base. */
  char *data;
  size_t size;
  size_t alloc_size;
  size_t element_size; /* sizeof(char) */
} tort_string;

#define tort_string_data(X) ((char*)tort_vector_base_data(X))
#define tort_string_size(X) tort_vector_base_size(X)
#define tort_string_alloc_size(X) tort_vector_base_alloc_size(X)

tort_v tort_string_new(const char *d, size_t s);
tort_v tort_string_new_cstr(const char *str);

typedef
struct tort_mtable {
  tort_map _map;
  tort_v delegate;
} tort_mtable;

struct tort_symbol {
  tort_string *name;
  tort_v version;
};

static inline 
const char *tort_symbol_data(tort_v sym) 
{ 
  return tort_string_data(tort_ref(tort_symbol, sym)->name);
}

tort_v tort_symbol_make(const char *string);
const char *tort_symbol_encode(const char *in);

typedef
struct tort_method {
  tort_v name;
  tort_v data;
} tort_method;

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
#if ! TORT_NIL_IS_ZERO
  tort_v nil;
#endif
  tort_v string_null, vector_null;
  tort_v b_true, b_false;
  tort_v symbols;
  tort_v root;

  tort_v message;

  tort_v _in_error;
  tort_error_decl((*error));
  tort_error_decl((*fatal));

  tort_header nil_header;
  tort_header tagged_header;

  int _argc;
  char **_argv;
  char **_env;

#define tort_d_mt(N) tort_v _mt_##N;
#include "tort/d_mt.h"

#define tort_d_s(N) tort_v _s_##N;
#include "tort/d_s.h"

  /* io */
  tort_v _io_stdin;
  tort_v _io_stdout;
  tort_v _io_stderr;
  tort_v _io_eos;

  /* globals */
  tort_v m_mtable; /* map of symbol to mtable. */

  tort_v _initialized;
} tort_runtime;

typedef struct _tort_runtime_data {
  struct tort_header _hdr;
  struct tort_runtime _runtime;
} _tort_runtime_data;

extern tort_runtime *_tort;
#if TORT_MULTIPLICITY
#define tort_(X) _tort->X
#else
extern _tort_runtime_data __tort;
#define tort_(X) __tort._runtime.X
#endif

#define tort_stdin  tort_(_io_stdin)
#define tort_stdout tort_(_io_stdout)
#define tort_stderr tort_(_io_stderr)
#define tort_eos    tort_(_io_eos)

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
      { sizeof(tort_message),						\
	_tort_object_lookupf,						\
	_tort_object_applyf,						\
	tort__mt(message) },						\
      { (SEL),								\
	_tort_send_RCVR(RCVR_AND_ARGS),					\
	tort_nil,							\
	_tort_message,							\
	_tort_message ? _tort_message->fiber : _tort_fiber		\
      }									\
    };									\
    tort_h_lookupf(__tort_msg._msg.receiver)(&__tort_msg._msg, __tort_msg._msg.receiver); \
    tort_h_applyf(__tort_msg._msg.method)(&__tort_msg._msg, _tort_send_RCVR_ARGS(__tort_msg._msg.receiver, RCVR_AND_ARGS)); \
  })
#define tort_send(SEL, RCVR_AND_ARGS...)_tort_send(SEL, RCVR_AND_ARGS)

/* catch for top-level messages: DO NOT MODIFY! */
extern tort_message *_tort_message; 
extern tort_v _tort_fiber;

#define tort_string_null tort_(string_null)
#define tort_vector_null tort_(vector_null)

#define tort_true  tort_(b_true)
#define tort_false tort_(b_false)

void *tort_malloc(size_t size);
void  tort_free(void *ptr);
void *tort_realloc(void *ptr, size_t size);

tort_v tort_map_create();

tort_v tort_mtable_create();

tort_lookup_decl(_tort_object_lookupf);
tort_apply_decl(_tort_object_applyf);

#if TORT_ALLOC_DEBUG
tort_v _tort_allocate (tort_thread_param tort_v meth_table, size_t size, const char *alloc_file, int alloc_line);
#define tort_allocate(_1, _2) _tort_allocate(tort_thread_arg _1, _2, __FILE__, __LINE__)
#else
tort_v _tort_allocate (tort_thread_param tort_v meth_table, size_t size);
#define tort_allocate(_1, _2) _tort_allocate(tort_thread_arg _1, _2)
#endif

#if TORT_MULTIPLICITY
#define tort_s(X) tort_symbol_make(tort_symbol_encode(#X))
#define tort_mt(X) tort_mtable_get(#X)
#else
#define tort_s(X)  ({ static tort_v _s_##X;  _s_##X ?  _s_##X :  (_s_##X  = tort_symbol_make(tort_symbol_encode(#X))); })
#define tort_mt(X) ({ static tort_v _mt_##X; _mt_##X ? _mt_##X : (_mt_##X = tort_mtable_get(#X)); })
#endif
#define tort__s(X) tort_(_s_##X)
#define tort__mt(X) tort_(_mt_##X)

tort_v tort_object_make ();

tort_v tort_mtable_get (const char *string);
tort_v tort_mtable_set (const char *string, tort_v mtable);
tort_v tort_mtable_make (const char *string, tort_v parent);

tort_v tort_method_make (tort_apply_decl((*applyf)));

tort_v tort_add_method(tort_v map, const char *name, void *applyf);
tort_v tort_add_class_method(tort_v map, const char *name, void *applyf);

tort_v tort_runtime_create_ (int *argcp, char ***argvp, char ***envp);
#define tort_runtime_create() tort_runtime_create_(&argc, &argv, &environ)

tort_v tort_fatal (const char *format, ...);
tort_v tort_error (const char *format, ...);
tort_v tort_error_message(const char *format, ...);

void tort_debug_stop_at();
const char *tort_object_name_(tort_v val);
const char *tort_object_name(tort_v val);

extern int _tort_gc_mode;
void tort_gc_collect();
void tort_gc_dump_stats();
void tort_gc_invoke_finalizers();

#endif

