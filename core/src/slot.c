#include "tort/core.h"

tort_SLOT(slot,tort_v,mtable);
tort_SLOT(slot,tort_v,name);
tort_SLOT(slot,tort_v,type);
tort_SLOT(slot,tort_v,offset);
tort_SLOT(slot,tort_v,size);

tort_v _tort_M_slot__new(tort_tp tort_mtable *x, tort_v mtable, tort_v name, tort_v type, tort_v offset, tort_v size)
{
  tort_slot *self = tort_send(tort__s(allocate), mtable, tort_i(sizeof(*self)));
  self->mtable = mtable;
  self->name = name;
  self->type = type;
  self->offset = offset;
  self->size = size;
  return_tort_send(tort__s(initialize), self);
}

tort_v _tort_m_slot__initialize(tort_tp tort_slot *slot)
{
  const char *name = tort_symbol_charP(slot->name);
  char *name_buf = malloc(strlen(name) + 2);
  slot->getter = slot->name;
  slot->setter = tort_symbol_new(strcat(strcpy(name_buf, name), "="));
  slot->locater = tort_nil;
  if ( slot->type == tort__s(tort_v) ) {
    slot->locater = tort_symbol_new(strcat(strcpy(name_buf, name), "&"));
  }
  return slot;
}

tort_v _tort_m_slot__attach(tort_tp tort_slot *slot)
{
  tort_v slots = tort_send(tort__s(slots), slot->mtable);
  tort_send(tort__s(set), slots, slot->name, slot);
  if ( slot->type == tort__s(tort_v) ) {
    tort_send(tort__s(add_method), slot->mtable, slot->getter,  tort_offset_getter_new(slot->offset));
    tort_send(tort__s(add_method), slot->mtable, slot->setter,  tort_offset_setter_new(slot->offset));
    if ( slot->locater != tort_nil )
      tort_send(tort__s(add_method), slot->mtable, slot->locater, tort_offset_locater_new(slot->offset));
  }
  return slot;
}

tort_slot* tort_slot_prepare(tort_slot *slot)
{
  tort_h_mtable(slot) = tort__mt(slot);
  slot = tort_send(tort__s(clone), slot);
  slot->mtable = tort_mtable_get((const char *) slot->mtable);
  slot->name = tort_symbol_new((const char*) slot->name);
  slot->type = tort_symbol_new((const char*) slot->type);
  tort_send(tort__s(initialize), slot);
  return slot;
}

#ifndef tort_d_slot
#define tort_d_slot(MT,T,N) extern tort_slot_ _tort_slot_##MT##__##N;
#include "tort/d_slot.h"
#endif
static tort_slot_ *slots[] = {
#ifndef tort_d_slot
#define tort_d_slot(MT,T,N) &_tort_slot_##MT##__##N,
#include "tort/d_slot.h"
#endif
  0
};

tort_v tort_runtime_initialize_slot()
{
  int i;
  for ( i = 0; slots[i]; ++ i ) {
    tort_slot *slot = tort_slot_prepare(&slots[i]->_);
    fprintf(stderr, "  slot %p\n", slot);
    fprintf(stderr, "  slot %s.%s %s +%d %d\n", 
	    tort_object_name(slot->mtable), 
	    tort_object_name(slot->name), 
	    tort_object_name(slot->type),
	    (int) tort_I(slot->offset),
	    (int) tort_I(slot->size));
    tort_send(tort__s(attach), slot);
  }
  return 0;
}

