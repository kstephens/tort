/* -*- c -*- */
/* Sources:
src/debug.c
src/dynlib.c
src/eq.c
src/error.c
src/fixnum.c
src/gc.c
src/init.c
src/io.c
src/literal.c
src/locative.c
src/lookup.c
src/map.c
src/message.c
src/method.c
src/mtable.c
src/object.c
src/pair.c
src/printf.c
src/ptr.c
src/slot.c
src/string.c
src/symbol.c
src/symbol_encoding.c
src/tagged.c
src/value.c
src/vector.c
src/word.c
src/write.c
*/
tort_d_slot(caller_info,tort_v,data)
tort_d_slot(io,tort_v,data)
tort_d_slot(io,tort_v,mode)
tort_d_slot(io,tort_v,name)
tort_d_slot(map,tort_v,equality)
tort_d_slot(message,tort_v,argc)
tort_d_slot(message,tort_v,caller_info)
tort_d_slot(message,tort_v,fiber)
tort_d_slot(message,tort_v,method)
tort_d_slot(message,tort_v,mtable)
tort_d_slot(message,tort_v,previous_message)
tort_d_slot(message,tort_v,receiver)
tort_d_slot(message,tort_v,selector)
tort_d_slot(method,tort_v,data)
tort_d_slot(method,tort_v,name)
tort_d_slot(mtable,tort_v,data)
tort_d_slot(mtable,tort_v,gc_free_method)
tort_d_slot(mtable,tort_v,gc_mark_method)
tort_d_slot(pair,tort_v,first)
tort_d_slot(pair,tort_v,second)
tort_d_slot(slot,tort_v,mtable)
tort_d_slot(slot,tort_v,name)
tort_d_slot(slot,tort_v,offset)
tort_d_slot(slot,tort_v,size)
tort_d_slot(slot,tort_v,type)
tort_d_slot(symbol,tort_v,mtable_method_map)
tort_d_slot(symbol,tort_v,name)
tort_d_slot(symbol,tort_v,version)
/* -*- c -*- */
#undef tort_d_slot
