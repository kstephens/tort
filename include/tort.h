#ifndef _tort_tort_h
#define _tort_tort_h

#include <stdarg.h>

typedef unsigned long tort_val;

#define tort_ref(t, x)       ((struct t *)(x))
#define tort_ref_box(t, x) ((tort_val)(ptr))

#define tort_error_decl(X)  void X (const char *format, va_list vap)

#define tort_lookup_decl(X) tort_val X (tort_val message, tort_val rcvr, ...)
#define tort_apply_decl(X)  tort_val X (tort_val message, tort_val rcvr, ...)

struct tort_header {
  tort_apply_func(*applyf);
  tort_lookup_func(*lookupf);
  tort_val mtable;
};

#define tort_h(x) ((struct tort_header*)tort_ref(x))[-1]
#define tort_apply_func(x) tort_h(x).applyf
#define tort_lookup_func(x) tort_h(x).lookupf
#define tort_mtable(x) tort_h(x).mtable

typedef 
struct tort_map_entry {
  tort_val key;
  tort_val value;
} tort_map_entry;

typedef
struct tort_map {
  struct tort_map_entry **entry;
  size_t entry_n;
} tort_map;

typedef
struct tort_string {
  char *data;
  size_t size;
} tort_string;

#define tort_string_data(x) tort_ref_unbox(tort_string, x)->data
#define tort_string_size(x) tort_ref_unbox(tort_string, x)->size

tort_val tort_string_new_cstr(const char *str);

typedef
struct tort_symbol {
  tort_val name;
} tort_symbol;

typedef
struct tort_method {
  tort_val name;
} tort_method;

typedef
struct tort_message {
  tort_val selector;
  tort_val reciever;
  tort_val method;
} tort_messagae;

struct _tort_message {
  struct tort_header _hdr;
  struct tort_message _msg;
};

typedef
struct tort_runtime {
  struct tort_map _symbols;
  tort_error_decl((*error));
  tort_error_decl((*fatal));
  tort_val _mt_map;
  tort_val _mt_object;
  tort_val _mt_string;
  tort_val _mt_symbol;
  tort_val _mt_method;
  tort_val _s_new;
  tort_val _s_lookup;
  tort_val _s_apply;
} tort_runtime;


#define tort_send(sel, rcvr, ...)					\
  ({									\
    _tort_message __msg = {						\
      { _tort_message_apply, _tort_message_lookup, _tort_message_mtable }, 
      { (sel), (rcvr), tort_nil }						\
    };									\
    tort_val __msg_val = tort_ref_box(&__msg._msg);				\
    (tort_lookup_func(__msg))(__msg_val);				\
    (tort_apply_func(__msg.method)(__msg_val, __msg,rcvr, ...);		\
  })

extern tort_runtime *_tort_runtime;
extern tort_val _tort_message_apply;
extern tort_val _tort_message_lookup;
extern tort_val _tort_message_mtable;
extern tort_val tort_nil;

void *tort_malloc(size_t size);
void *tort_realloc(void *ptr, size_t size);

void _tort_map_initialize(tort_map *map);
void _tort_map_set(tort_map *map, tort_val key, tort_val value);
tort_map_entry *_tort_map_get_entry(tort_map *map, tort_val key);
tort_val _tort_map_get(tort_map *map, tort_val key);

tort_lookup_decl(_tort_object_lookupf);
tort_apply_decl(_tort_object_applyf);

void * tort_allocate (size_t size, tort_ref meth_table);

tort_val tort_symbol (const char *string);
tort_val tort_method_make (tort_apply_decl((*applyf)));
tort_val tort_runtime_create ();


#endif

