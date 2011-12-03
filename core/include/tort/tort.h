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

#include "tort/box.h"
#include "tort/config.h"

typedef void* tort_v;
typedef ssize_t tort_vi;

#define tort_ref(T, X)      ((struct T *)(X))
#define tort_ref_box(PTR)   ((tort_v)(PTR))

#define tort_TAG_MASK ((1 << TORT_TAG_BITS) - 1)
#define tort_tag(X) ((short) tort_taggedQ(X))
#define tort_taggedQ(X)     ((size_t)(X) & tort_TAG_MASK)
#define tort_tagged_box(V,TAG)  ((tort_v) ((((ssize_t) (V)) << TORT_TAG_BITS) | TAG))
#define tort_tagged_data(X) (((ssize_t) (X)) >> TORT_TAG_BITS)
#define tort_tagged_udata(X) (((size_t) (X)) >> TORT_TAG_BITS)

/* Fixnums: Small integers */
#define tort_i(V) tort_tagged_box(V,tort_tag_fixnum)
#define tort_I(X) tort_tagged_data(X)

/* Locatives: */
#ifdef tort_tag_locative
extern tort_v tort_locative_new(tort_v *);
//#define tort_l(P) tort_locative_new((void*) P)
#define tort_l(P) _tort_l(P)
#define _tort_l(P) ((tort_v )(((size_t) (P)) ^ tort_tag_locative))
#define tort_L(X) ((tort_v*)(((size_t) (X)) - tort_tag_locative))
#endif

/* Characters: */
#ifndef tort_c
#define tort_c(V) tort_i(V)
#define tort_C(X) tort_I(X)
#endif

typedef struct tort_symbol tort_symbol;
typedef struct tort_mtable tort_mtable;
typedef struct tort_message tort_message;
typedef struct tort_method tort_method;

#define tort_thread_param tort_message *_tort_message,
#define tort_tp tort_thread_param
#define tort_thread_arg                 _tort_message,
#define tort_ta tort_thread_arg

#define tort_lookup_decl(X) tort_message* X (tort_tp tort_mtable *mtable, tort_message *message)
#define tort_apply_decl(X)  tort_v X (tort_tp tort_v rcvr, ...)

extern tort_apply_decl(_tort_m_object___method_not_found);
extern tort_apply_decl(_tort_m_object___cannot_apply);

#define tort_GETTER(MT,T,N) \
  tort_v _tort_M_##MT##___offset_##N ( tort_tp tort_mtable *mtable ) { return tort_i(&((struct tort_##MT*) 0)->N); } \
  tort_v _tort_m_##MT##__##N ( tort_tp struct tort_##MT *rcvr ) { return tort_box_(T,rcvr->N); }
#define tort_SETTER(MT,T,N) \
  tort_v _tort_m_##MT##__##N##SET ( tort_tp struct tort_##MT *rcvr, tort_v val ) { rcvr->N = tort_unbox_(T,val); return rcvr; }
#define tort_ACCESSOR(MT,T,N) \
  tort_GETTER(MT,T,N); \
  tort_SETTER(MT,T,N)

typedef
struct tort_header {
#if TORT_ALLOC_DEBUG
  const char *alloc_file;
  int alloc_line;
  size_t alloc_id;
#endif
  tort_apply_decl((*applyf)); /** How to apply this object as a method. */
  tort_mtable *mtable; /** The object's method table. */
} tort_header;
#define tort_H tort_header _h[0]

#define tort_h_struct(X) typedef struct X##_ { tort_header _h; X _; } X##_

#define tort_h_ref(X)    (((struct tort_header*) (X))-1)
#if TORT_TAG_BITS == 1
#define tort_h_tagged(X) (tort_taggedQ(X) ? &tort_(tagged_header[1]) : tort_h_ref(X))
#else
#define tort_h_tagged(X) (tort_taggedQ(X) ? &tort_(tagged_header[tort_tag(X)]) : tort_h_ref(X))
#endif

typedef
struct tort_object { tort_H;
} tort_object;

#if TORT_NIL_IS_ZERO
#define tort_h_nil(X)    &tort_(nil_header)
#define tort_nil         ((tort_v) 0)
#define tort_nilQ(X)     ((X) == 0)
#define tort_h(X)        ( tort_nilQ(X) ? tort_h_nil(X) : tort_h_tagged(X) )
#else
#define tort_nil         tort_(nil)
#define tort_nilQ(X)     ((X) == tort_nil)
#define tort_h(X)        tort_h_tagged(X)
#endif

#define tort_h_mtable(X)  tort_h(X)->mtable

typedef struct tort_caller_info { tort_H;
  const char   *file; 
  int           line;
  tort_v        data;
} tort_caller_info;
tort_h_struct(tort_caller_info);

struct tort_message { tort_H;
  tort_symbol  *selector;
  tort_object  *receiver;
  tort_message *previous_message;
  tort_mtable  *mtable; /* the mtable to start searching from, the mtable where the method was found. */
  tort_method  *method; /* the found method. */
  tort_v        argc;   /* number of arguments, including receiver.  >= 0 if specified by caller. */
  tort_v        fiber;  /* the sending fiber */
#if TORT_MESSAGE_FILE_LINE
  tort_caller_info *caller_info;
#endif
};
tort_h_struct(tort_message);

typedef
struct tort_ptr { tort_H; /* basic ptr. */
  void *data;
} tort_ptr;
#define tort_P(X) tort_ref(tort_ptr, X)->data
void * tort_ptr_data(tort_v x);
tort_v tort_ptr_new(void *ptr);

typedef 
struct tort_vector_base { tort_H; /* Same layout as tort_vector, tort_string. */
  void *data;
  size_t size;
  size_t alloc_size;
  size_t element_size;
} tort_vector_base;
tort_v tort_vector_base_new(tort_v mtable, const void *d, size_t s, size_t element_size);
#define tort_vector_base_data(X) tort_ref(tort_vector_base, X)->data
#define tort_vector_base_size(X) tort_ref(tort_vector_base, X)->size
#define tort_vector_base_alloc_size(X) tort_ref(tort_vector_base, X)->alloc_size
#define tort_vector_base_element_size(X) tort_ref(tort_vector_base, X)->element_size

typedef
struct tort_vector { tort_H; /* Same layout as tort_vector_base. */
  tort_v *data;
  size_t size;
  size_t alloc_size;
  size_t element_size; /* sizeof(tort_v) */
} tort_vector;
tort_v tort_vector_new(const tort_v *d, size_t s);
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

typedef 
struct tort_pair { tort_H;
  tort_v first, second;
} tort_pair;

typedef
struct tort_map { tort_H; /* Same layout as tort_vector_base. */
  tort_vector_base _vector_base;
  tort_v equality;
} tort_map;
tort_v tort_map_new();
#define tort_map_data(X) ((tort_pair**)tort_vector_base_data(X))
#define tort_map_size(X) tort_vector_base_size(X)
#define tort_map_EACH(m, me)					\
  {								\
  tort_pair **me##p = tort_map_data(m), *me;			\
  while ( (me = *(me##p ++)) ) {
#define tort_map_EACH_END() }}

typedef
struct tort_string { tort_H; /* Same layout as tort_vector_base. */
  char *data;
  size_t size;
  size_t alloc_size;
  size_t element_size; /* sizeof(char) */
} tort_string;

#define tort_string_charP(X) ((char*)tort_vector_base_data(X))
#define tort_string_data(X) tort_string_charP(X)
#define tort_string_size(X) tort_vector_base_size(X)
#define tort_string_alloc_size(X) tort_vector_base_alloc_size(X)

tort_v tort_string_new(const char *d, size_t s);
tort_v tort_string_new_cstr(const char *str);

struct tort_mtable { tort_H;
  tort_map _map; /* same layout as tort_map. */
  tort_mtable* delegate;
  size_t instance_size; /** allocated object size, not including this header */
  void *gc_data;
  tort_v gc_mark_method;
  tort_v gc_free_method;
  tort_v data;
};

struct tort_symbol { tort_H;
  tort_string *name;
  tort_v version;
#if TORT_ANON_SYMBOL_MTABLE
  tort_map *mtable_method_map;
#endif
};

static inline 
const char *tort_symbol_charP(tort_v sym) 
{ 
  return tort_ref(tort_symbol, sym)->name ? 
    tort_string_charP(tort_ref(tort_symbol, sym)->name) : 
    "";
}
#define tort_symbol_data(X) tort_symbol_charP(X)

tort_symbol* tort_symbol_new(const char *string);
const char *tort_symbol_encode(const char *in);
tort_symbol* tort_symbol_new_encode(const char *string);

struct tort_method { tort_H;
  tort_v data;
  tort_v name;
};
tort_h_struct(tort_method);

typedef
struct tort_io { tort_H;
  FILE *fp;
  tort_v name;
  tort_v mode;
  int flags;
  tort_v data;
} tort_io;

typedef
struct tort_runtime { tort_H;
  tort_header nil_header; /** Header for tort_nil object. */
  tort_header tagged_header[1 << TORT_TAG_BITS]; /** Header for tagged object (integers). */

#if ! TORT_NIL_IS_ZERO
  tort_v nil;
#endif
  tort_v b_true, b_false;
  tort_map *symbols; /** Symbol table map: string=>symbol. */
  tort_v root; /** Root namespace. */

  tort_v message; /* Current message: not thread/fiber-safe. */
  tort_caller_info *unknown_caller_info;
  tort_v _m_method_not_found; /** method called if method cannot be found. */
  tort_v _m_cannot_apply;     /** method called if method cannot be applied. */

  tort_v _in_error;
#define tort_error_decl(X)  tort_v X (tort_tp const char *format, va_list *vapp)
  tort_error_decl((*error));
  tort_error_decl((*fatal));
  tort_v error_catch;
  tort_v top_catch;

  int _argc; /** Process argument count from main(). */
  char **_argv;/** Process argument vector from main(). */
  char **_env;/** Process environment from main(). */
  void *stack_bottom;

#define tort_d_mt(N) tort_mtable *_mt_##N;
#include "tort/d_mt.h"

#define tort_d_s(N) tort_symbol *_s_##N;
#include "tort/d_s.h"

  /* io */
  tort_v _io_stdin;
  tort_v _io_stdout;
  tort_v _io_stderr;
  tort_v _io_eos;
  tort_v _printf_dispatch;

  /* globals */
  tort_v m_mtable; /* map of symbol to mtable. */
  tort_v dl_maps;  /* map of loaded dynamic library filenames to maps of DL symbols. */

  tort_v _initialized;
} tort_runtime;

tort_h_struct(tort_runtime);

extern tort_runtime *_tort;
#if TORT_MULTIPLICITY
#define tort_(X) _tort->X
#else
extern tort_runtime_ __tort;
#define tort_(X) __tort._.X
#endif

#define tort_stdin  tort_(_io_stdin)
#define tort_stdout tort_(_io_stdout)
#define tort_stderr tort_(_io_stderr)
#define tort_eos    tort_(_io_eos)

#define tort_write(io, str) tort_send(tort__s(__write), io, str)
#define tort_inspect(io, obj) tort_send(tort__s(_inspect), obj, io)
#define tort_printf(io, fmt, args...) tort_send(tort__s(__printfs), io, fmt, ## args)
#define tort_printfv(io, fmt, vapp) tort_send(tort__s(__printfsv), io, fmt, vapp)
#define tort_flush(io) tort_send(tort_s(flush), io)
#define tort_sprintf(STR, FMT, ARGS...) tort_send(tort__s(__printfs), (tort_v) (STR), (FMT), ##ARGS)

tort_message* _tort_lookup (tort_tp tort_v rcvr, tort_message *message);
tort_lookup_decl(_tort_m_mtable__lookup);
extern int _tort_lookup_trace;

#define _tort_send_RCVR(RCVR, ARGS...)(RCVR)
#define _tort_send_ARGS(RCVR, ARGS...)ARGS
#define _tort_send_RCVR_ARGS(RCVR, EATEN, ARGS...)RCVR, ##ARGS
#if TORT_MESSAGE_FILE_LINE
#define _tort_send_msg_file_line(MSG) {					\
    static tort_caller_info_ __tort_caller_info = { { }, { { }, __FILE__, __LINE__ } }; \
    (MSG)->caller_info = &__tort_caller_info._;				\
  }
#else
#define _tort_send_msg_file_line(MSG) 
#endif

#define _tort_send_msg_init(SEL, RCVR_AND_ARGS...)			\
    tort_message_ __tort_msg = {					\
      { },								\
      { { }, (SEL),							\
	(tort_v) _tort_send_RCVR(RCVR_AND_ARGS),			\
	_tort_message,							\
        0,								\
      }									\
    };									\
    _tort_send_msg_file_line(&__tort_msg._)

/* ARGC = 1 (RCVR) + number of args, or -1 if unknown. */
#define _tort_sendn(SEL, ARGC, RCVR_AND_ARGS...)			\
  ({									\
    _tort_send_msg_init(SEL, RCVR_AND_ARGS);				\
    __tort_msg._.argc = tort_i(ARGC);					\
    _tort_lookup(_tort_message, __tort_msg._.receiver, &__tort_msg._)-> \
      method->_h[-1].applyf(&__tort_msg._, _tort_send_RCVR_ARGS(__tort_msg._.receiver, RCVR_AND_ARGS)); \
  })
#define tort_send(SEL, RCVR_AND_ARGS...)_tort_sendn(SEL, -1, RCVR_AND_ARGS)
#define tort_sendn(SEL, ARGC, RCVR_AND_ARGS...)_tort_sendn(SEL, ARGC, RCVR_AND_ARGS)

/* Tail call, reuse current message object. */
#define _tort_sendnt(SEL, ARGC, RCVR_AND_ARGS...)			\
  do {									\
    _tort_message->selector = (tort_v) (SEL);				\
    _tort_message->receiver = (tort_v) _tort_send_RCVR(RCVR_AND_ARGS);	\
    _tort_message->mtable = 0;						\
    _tort_message->argc = tort_i(ARGC);					\
    _tort_send_msg_file_line(_tort_message);				\
    return								\
      _tort_lookup(_tort_message, _tort_message->receiver, _tort_message)-> \
      method->_h[-1].applyf(_tort_message, _tort_send_RCVR_ARGS(_tort_message->receiver, RCVR_AND_ARGS)); \
  } while ( 0 )
#define return_tort_send(SEL, RCVR_AND_ARGS...)_tort_sendnt(SEL, -1, RCVR_AND_ARGS)
#define return_tort_sendn(SEL, ARGC, RCVR_AND_ARGS...)_tort_sendnt(SEL, ARGC, RCVR_AND_ARGS)

/* catch for top-level messages: DO NOT MODIFY THESE VARS! */
extern tort_message *_tort_message; 
extern tort_v _tort_fiber;

#define tort_true  tort_(b_true)
#if TORT_FALSE_IS_NIL
#define tort_false tort_nil
#else
#define tort_false tort_(b_false)
#endif

void *tort_object_alloc(tort_mtable *mtable, size_t size);
void *tort_malloc(size_t size);
void *tort_malloc_atomic(size_t size);
void  tort_free(void *ptr);
void  tort_free_atomic(void *ptr);
void *tort_realloc(void *ptr, size_t size);
void *tort_realloc_atomic(void *ptr, size_t size);
void  tort_gc_collect();
void  tort_gc_mark(tort_v referrer, tort_v referred);
#define tort_gc_add_root(referencep) (void)0 // TODO
#define tort_gc_remove_root(referencep) (void)0
void tort_gc_mark_range(tort_v referrer, void *b, void *e);
void tort_gc_add_root_callback(void (*func)(void *data), void *data);

#if TORT_ALLOC_DEBUG
tort_v _tort_allocate (tort_tp tort_v meth_table, size_t size, const char *alloc_file, int alloc_line);
#define tort_allocate(_1, _2) _tort_allocate(tort_ta _1, _2, __FILE__, __LINE__)
#else
tort_v _tort_allocate (tort_tp tort_v meth_table, size_t size);
#define tort_allocate(_1, _2) _tort_allocate(tort_ta _1, _2)
#endif

#if TORT_MULTIPLICITY
#define tort_s(X) tort_symbol_new_encode(#X)
#define tort_mt(X) tort_mtable_get(#X)
#else
#define tort_s(X)  ({ static tort_v _s_##X;  _s_##X ?  _s_##X :  (_s_##X  = tort_symbol_new_encode(#X)); })
#define tort_mt(X) ({ static tort_v _mt_##X; _mt_##X ? _mt_##X : (_mt_##X = tort_mtable_get(#X)); })
#endif
#define tort__s(X) tort_(_s_##X)
#define tort__mt(X) tort_(_mt_##X)

tort_v tort_object_new ();

tort_mtable* tort_mtable_new(tort_v delegate);
tort_mtable* tort_mtable_new_class(tort_v delegate);
tort_mtable* tort_mtable_get(const char *string);
tort_mtable* tort_mtable_create_class(const char *string, tort_v parent);

tort_method* tort_method_new (void *applyf, tort_v data);
tort_method* tort_offset_getter_new (tort_v offset);
tort_method* tort_offset_setter_new (tort_v offset);
tort_method* tort_constant_getter_new (tort_v value);

tort_v tort_add_method(tort_v map, const char *name, void *applyf);
tort_v tort_add_class_method(tort_v map, const char *name, void *applyf);

tort_v tort_runtime_create_ (int *argcp, char ***argvp, char ***envp);
#define tort_runtime_create() tort_runtime_create_(&argc, &argv, &environ)

tort_v tort_fatal (tort_tp const char *format, ...);
tort_v tort_error (tort_tp const char *format, ...);
tort_v tort_error_message(const char *format, ...);

void tort_debug_stop_at();
const char *tort_mtable_name_(tort_v val);
const char *tort_object_name_(tort_v val);
const char *tort_object_name(tort_v val);

extern const char *_tort_gc_mode;
void tort_gc_collect();
void tort_gc_dump_stats();
void tort_gc_invoke_finalizers();

typedef struct tort_dynlib { tort_H;
  tort_map _map; /* same layout as map. */
  tort_v name;
  tort_v path;
  tort_v error;
  tort_v base_sym;
  tort_v base_ptr;
} tort_dynlib;

void *tort_dlopen(const char *file, char *file_buffer);
void *tort_dlsym(void *handle, const char *symbol);
void tort_dlclose(void *handle);

#endif

