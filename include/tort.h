
typedef unsigned long tort_val;

#define tort_ref(x) ((tort_val*)(x))
#define tort_lookup_decl(X) tort_val X (tort_val message, tort_val rcvr, ...)
#define tort_apply_decl(X) tort_val X (tort_val message, tort_val rcvr, ...)

struct tort_header {
  tort_apply_func(*applyf);
  tort_lookup_func(*lookupf);
  tort_val mtable;
};

#define tort_h(x) ((struct tort_header*)tort_ref(x))[-1]
#define tort_apply_func(x) tort_h(x).applyf
#define tort_lookup_func(x) tort_h(x).lookupf
#define tort_mtable(x) tort_h(x).mtable

struct tort_map_entry {
  tort_val key;
  tort_val value;
};

struct tort_map {
  struct tort_map_entry **entry;
  size_t entry_n;
};

struct tort_string {
  char *data;
  size_t size;
};

struct tort_selector {
  tort_val name;
};

struct tort_method {
  tort_val name;
};

struct tort_message {
  tort_val selector;
  tort_val reciever;
  tort_val method;
};

struct _tort_message {
  struct tort_header _hdr;
  struct tort_message _msg;
};

#define tort_call(tort_val sel, tort_rcvr rcvr, ...)			\
  ({									\
    _tort_message __msg = {						\
      { _tort_message_apply, _tort_message_lookup, _tort_message_mtable }, 
      { (sel), (rcvr), tort_nil }						\
    };									\
    tort_val __msg_val = tort_ref_box(&__msg._msg);				\
    (tort_lookup_func(__msg))(__msg_val);				\
    (tort_apply_func(__msg.method)(__msg_val, __msg,rcvr, ...);		\
  })

extern tort_val _tort_message_apply;
extern tort_val _tort_message_lookup;
extern tort_val _tort_message_mtable;
extern tort_val tort_nil;

void *tort_malloc(size_t size)
{
  memclr(malloc(size), size);
}

void _tort_map_initialize(tort_map *map)
{
  map->entry_n = 0;
  tort_malloc(sizeof(map->entry[0]) * (map->entry_n + 1));
}

void _tort_map_set(tort_map *map, tort_val key, tort_val value)
{
  tort_map_entry *e = _tort_map_get_entry(map, key);
  if ( ! e ) {
    e = malloc(sizeof(*e));
    e->key = key;
    map->entry_n ++;
    map->entry = realloc(map->entry, sizeof(map->entry[0]) * (map->entry_n + 1));
    map->entry[map->entry_n] = e;
    map->entry[map->entry_n + 1] = 0;
  } 
  e->value = value;
}

tort_map_entry *_tort_map_get_entry(tort_map *map, tort_val key) {
  tort_map_entry **x = map->entries, *entry;

  while ( entry = *(x ++) ) {
    if ( entry->key == msg.selector ) {
      return entry;
    }
  }

  return 0;
}

tort_val _tort_map_get(tort_map *map, tort_val key) {
  tort_map_entry *e = _tort_map_get_entry(map, key);
  return e ? e->value : _tort_nil;
}

tort_lookup_decl(_tort_object_lookupf) {
  msg.method = _tort_map_get(tort_ref(tort_mtable(msg.receiver)), msg.selector);
  return msg;
}

tort_apply_decl(_tort_object_applyf) {
  tort_error("cannot apply this object");
  return tort_nil;
}


void * tort_allocate(size_t size, tort_ref meth_table)
{
  tort_val val;
  void *ptr;
  size += sizeof(tort_header);
  ptr = tort_malloc(size);
  ptr += sizeof(tort_header);
  val = tort_val_box(ptr);

  tort_applyf(val)  = _tort_object_applyf;
  tort_lookupf(val) = _tort_object_lookupf;
  tort_mtable(val)  = meth_table;

  return ptr;
}

tort_val tort_symbol(const char *string)
{
  
}

tort_val tort_method_make(tort_apply_decl((*applyf)))
{
  struct tort_method *meth = tort_allocate(sizeof(struct tort_method), _tort_method_mtable);
  meth->name = 0;
  val = tort_ref_box(meth);
  tort_applyf(val) = applyf;
  return val;
}

void tort_system_create()
{
  _tort_map_mtable = tort_allocate(sizeof(struct tort_map), 0);
  _tort_obj_mtable = tort_allocate(sizeof(struct tort_map), 0);
  _tort_string_mtable   = tort_allocate(sizeof(struct tort_map), 0);
  _tort_selector_mtable = tort_allocate(sizeof(struct tort_map), 0);
  _tort_method_mtable = tort_allocate(sizeof(struct tort_map), 0);
  
  tort_s_new = tort_symbol("new");
  tort_s_lookup = tort_symbol("lookup");
  tort_s_apply = tort_symbol("apply");

  tort_map_set(_tort_object_mtable, tort_s_lookup, tort_method_make(_tort_object_lookupf));
  tort_map_set(_tort_object_mtable, tort_s_apply, tort_method_make(_tort_object_applyf));

}


