/* -*- c -*- */
/* Sources:
include/tort/box.h
include/tort/config.h
include/tort/core.h
include/tort/d_m.h
include/tort/d_mt.h
include/tort/d_s.h
include/tort/fixnum.h
include/tort/init.h
include/tort/internal.h
include/tort/ops.h
include/tort/tort.h
src/debug.c
src/dynlib.c
src/eq.c
src/error.c
src/fixnum.c
src/gc.c
src/init.c
src/io.c
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
src/string.c
src/symbol.c
src/symbol_encoding.c
src/tagged.c
src/vector.c
src/write.c
*/
tort_d_m(tort__mt(boolean), tort__s(_inspect), _tort_m_boolean___inspect)
tort_d_m(tort__mt(caller_info), tort__s(data), _tort_m_caller_info__data)
tort_d_m(tort__mt(caller_info), tort__s(dataSET), _tort_m_caller_info__dataSET)
tort_d_m(tort__mt(caller_info), tort__s(file), _tort_m_caller_info__file)
tort_d_m(tort__mt(caller_info), tort__s(fileSET), _tort_m_caller_info__fileSET)
tort_d_m(tort__mt(caller_info), tort__s(line), _tort_m_caller_info__line)
tort_d_m(tort__mt(caller_info), tort__s(lineSET), _tort_m_caller_info__lineSET)
tort_d_m(tort__mt(dynlib), tort__s(_load_symtab), _tort_m_dynlib___load_symtab)
tort_d_m(tort__mt(dynlib), tort__s(dlopen), _tort_m_dynlib__dlopen)
tort_d_m(tort__mt(dynlib), tort__s(load), _tort_m_dynlib__load)
tort_d_m(tort__mt(fixnum), tort__s(ADD), _tort_m_fixnum__ADD)
tort_d_m(tort__mt(fixnum), tort__s(AND), _tort_m_fixnum__AND)
tort_d_m(tort__mt(fixnum), tort__s(DIV), _tort_m_fixnum__DIV)
tort_d_m(tort__mt(fixnum), tort__s(EQ), _tort_m_fixnum__EQ)
tort_d_m(tort__mt(fixnum), tort__s(GE), _tort_m_fixnum__GE)
tort_d_m(tort__mt(fixnum), tort__s(GT), _tort_m_fixnum__GT)
tort_d_m(tort__mt(fixnum), tort__s(INV), _tort_m_fixnum__INV)
tort_d_m(tort__mt(fixnum), tort__s(LAND), _tort_m_fixnum__LAND)
tort_d_m(tort__mt(fixnum), tort__s(LE), _tort_m_fixnum__LE)
tort_d_m(tort__mt(fixnum), tort__s(LOR), _tort_m_fixnum__LOR)
tort_d_m(tort__mt(fixnum), tort__s(LSH), _tort_m_fixnum__LSH)
tort_d_m(tort__mt(fixnum), tort__s(LT), _tort_m_fixnum__LT)
tort_d_m(tort__mt(fixnum), tort__s(MOD), _tort_m_fixnum__MOD)
tort_d_m(tort__mt(fixnum), tort__s(MUL), _tort_m_fixnum__MUL)
tort_d_m(tort__mt(fixnum), tort__s(NE), _tort_m_fixnum__NE)
tort_d_m(tort__mt(fixnum), tort__s(NEG), _tort_m_fixnum__NEG)
tort_d_m(tort__mt(fixnum), tort__s(NOT), _tort_m_fixnum__NOT)
tort_d_m(tort__mt(fixnum), tort__s(OR), _tort_m_fixnum__OR)
tort_d_m(tort__mt(fixnum), tort__s(RSH), _tort_m_fixnum__RSH)
tort_d_m(tort__mt(fixnum), tort__s(SUB), _tort_m_fixnum__SUB)
tort_d_m(tort__mt(fixnum), tort__s(XOR), _tort_m_fixnum__XOR)
tort_d_m(tort__mt(fixnum), tort__s(_inspect), _tort_m_fixnum___inspect)
tort_d_m(tort__mt(fixnum), tort__s(_to_string), _tort_m_fixnum___to_string)
tort_d_m(tort__mt(io), tort__s(__finalize), _tort_m_io____finalize)
tort_d_m(tort__mt(io), tort__s(__write), _tort_m_io____write)
tort_d_m(tort__mt(io), tort__s(_write), _tort_m_io___write)
tort_d_m(tort__mt(io), tort__s(close), _tort_m_io__close)
tort_d_m(tort__mt(io), tort__s(eof), _tort_m_io__eof)
tort_d_m(tort__mt(io), tort__s(error), _tort_m_io__error)
tort_d_m(tort__mt(io), tort__s(flush), _tort_m_io__flush)
tort_d_m(tort__mt(io), tort__s(open), _tort_m_io__open)
tort_d_m(tort__mt(io), tort__s(openQ), _tort_m_io__openQ)
tort_d_m(tort__mt(io), tort__s(popen), _tort_m_io__popen)
tort_d_m(tort__mt(io), tort__s(printf_as_string), _tort_m_io__printf_as_string)
tort_d_m(tort__mt(io), tort__s(read), _tort_m_io__read)
tort_d_m(tort__mt(locative), tort__s(_applyf), _tort_m_locative___applyf)
tort_d_m(tort__mt(locative), tort__s(_ptr_data), _tort_m_locative___ptr_data)
tort_d_m(tort__mt(locative), tort__s(_ptr_object), _tort_m_locative___ptr_object)
tort_d_m(tort__mt(locative), tort__s(_to_string), _tort_m_locative___to_string)
tort_d_m(tort__mt(locative), tort__s(value), _tort_m_locative__value)
tort_d_m(tort__mt(locative), tort__s(valueSET), _tort_m_locative__valueSET)
tort_d_m(tort__mt(map), tort__s(_gc_mark), _tort_m_map___gc_mark)
tort_d_m(tort__mt(map), tort__s(_inspect), _tort_m_map___inspect)
tort_d_m(tort__mt(map), tort__s(add), _tort_m_map__add)
tort_d_m(tort__mt(map), tort__s(clone), _tort_m_map__clone)
tort_d_m(tort__mt(map), tort__s(delete), _tort_m_map__delete)
tort_d_m(tort__mt(map), tort__s(emit), _tort_m_map__emit)
tort_d_m(tort__mt(map), tort__s(equalQ), _tort_m_map__equalQ)
tort_d_m(tort__mt(map), tort__s(equality), _tort_m_map__equality)
tort_d_m(tort__mt(map), tort__s(equalitySET), _tort_m_map__equalitySET)
tort_d_m(tort__mt(map), tort__s(get), _tort_m_map__get)
tort_d_m(tort__mt(map), tort__s(get_entry), _tort_m_map__get_entry)
tort_d_m(tort__mt(map), tort__s(get_entry_by_value), _tort_m_map__get_entry_by_value)
tort_d_m(tort__mt(map), tort__s(get_entry_cstr), _tort_m_map__get_entry_cstr)
tort_d_m(tort__mt(map), tort__s(get_entry_string), _tort_m_map__get_entry_string)
tort_d_m(tort__mt(map), tort__s(get_key), _tort_m_map__get_key)
tort_d_m(tort__mt(map), tort__s(get_string), _tort_m_map__get_string)
tort_d_m(tort__mt(map), tort__s(initialize), _tort_m_map__initialize)
tort_d_m(tort__mt(map), tort__s(set), _tort_m_map__set)
tort_d_m(tort__mt(message), tort__s(_inspect), _tort_m_message___inspect)
tort_d_m(tort__mt(message), tort__s(argc), _tort_m_message__argc)
tort_d_m(tort__mt(message), tort__s(argcSET), _tort_m_message__argcSET)
tort_d_m(tort__mt(message), tort__s(backtrace), _tort_m_message__backtrace)
tort_d_m(tort__mt(message), tort__s(caller_info), _tort_m_message__caller_info)
tort_d_m(tort__mt(message), tort__s(caller_infoSET), _tort_m_message__caller_infoSET)
tort_d_m(tort__mt(message), tort__s(fiber), _tort_m_message__fiber)
tort_d_m(tort__mt(message), tort__s(fiberSET), _tort_m_message__fiberSET)
tort_d_m(tort__mt(message), tort__s(initialize), _tort_m_message__initialize)
tort_d_m(tort__mt(message), tort__s(method), _tort_m_message__method)
tort_d_m(tort__mt(message), tort__s(methodSET), _tort_m_message__methodSET)
tort_d_m(tort__mt(message), tort__s(mtable), _tort_m_message__mtable)
tort_d_m(tort__mt(message), tort__s(mtableSET), _tort_m_message__mtableSET)
tort_d_m(tort__mt(message), tort__s(previous_message), _tort_m_message__previous_message)
tort_d_m(tort__mt(message), tort__s(previous_messageSET), _tort_m_message__previous_messageSET)
tort_d_m(tort__mt(message), tort__s(receiver), _tort_m_message__receiver)
tort_d_m(tort__mt(message), tort__s(receiverSET), _tort_m_message__receiverSET)
tort_d_m(tort__mt(message), tort__s(selector), _tort_m_message__selector)
tort_d_m(tort__mt(message), tort__s(selectorSET), _tort_m_message__selectorSET)
tort_d_m(tort__mt(method), tort__s(_inspect), _tort_m_method___inspect)
tort_d_m(tort__mt(method), tort__s(data), _tort_m_method__data)
tort_d_m(tort__mt(method), tort__s(dataSET), _tort_m_method__dataSET)
tort_d_m(tort__mt(method), tort__s(name), _tort_m_method__name)
tort_d_m(tort__mt(method), tort__s(nameSET), _tort_m_method__nameSET)
tort_d_m(tort__mt(mtable), tort__s(_delegate_changed), _tort_m_mtable___delegate_changed)
tort_d_m(tort__mt(mtable), tort__s(_method_changed), _tort_m_mtable___method_changed)
tort_d_m(tort__mt(mtable), tort__s(add_method), _tort_m_mtable__add_method)
tort_d_m(tort__mt(mtable), tort__s(alias_method), _tort_m_mtable__alias_method)
tort_d_m(tort__mt(mtable), tort__s(data), _tort_m_mtable__data)
tort_d_m(tort__mt(mtable), tort__s(dataSET), _tort_m_mtable__dataSET)
tort_d_m(tort__mt(mtable), tort__s(delegate), _tort_m_mtable__delegate)
tort_d_m(tort__mt(mtable), tort__s(delegateSET), _tort_m_mtable__delegateSET)
tort_d_m(tort__mt(mtable), tort__s(delegates), _tort_m_mtable__delegates)
tort_d_m(tort__mt(mtable), tort__s(gc_data), _tort_m_mtable__gc_data)
tort_d_m(tort__mt(mtable), tort__s(gc_dataSET), _tort_m_mtable__gc_dataSET)
tort_d_m(tort__mt(mtable), tort__s(gc_free_method), _tort_m_mtable__gc_free_method)
tort_d_m(tort__mt(mtable), tort__s(gc_free_methodSET), _tort_m_mtable__gc_free_methodSET)
tort_d_m(tort__mt(mtable), tort__s(gc_mark_method), _tort_m_mtable__gc_mark_method)
tort_d_m(tort__mt(mtable), tort__s(gc_mark_methodSET), _tort_m_mtable__gc_mark_methodSET)
tort_d_m(tort__mt(mtable), tort__s(initialize), _tort_m_mtable__initialize)
tort_d_m(tort__mt(mtable), tort__s(instance_size), _tort_m_mtable__instance_size)
tort_d_m(tort__mt(mtable), tort__s(instance_sizeSET), _tort_m_mtable__instance_sizeSET)
tort_d_m(tort__mt(mtable), tort__s(lookup), _tort_m_mtable__lookup)
tort_d_m(tort__mt(mtable), tort__s(remove_method), _tort_m_mtable__remove_method)
tort_d_m(tort__mt(nil), tort__s(__printfsv), _tort_m_nil____printfsv)
tort_d_m(tort__mt(nil), tort__s(_inspect), _tort_m_nil___inspect)
tort_d_m(tort__mt(nil), tort__s(_object_ptr), _tort_m_nil___object_ptr)
tort_d_m(tort__mt(object), tort__s(__debugger), _tort_m_object____debugger)
tort_d_m(tort__mt(object), tort__s(__message), _tort_m_object____message)
tort_d_m(tort__mt(object), tort__s(__printfs), _tort_m_object____printfs)
tort_d_m(tort__mt(object), tort__s(__printfsv), _tort_m_object____printfsv)
tort_d_m(tort__mt(object), tort__s(__register_finalizer), _tort_m_object____register_finalizer)
tort_d_m(tort__mt(object), tort__s(_applyf), _tort_m_object___applyf)
tort_d_m(tort__mt(object), tort__s(_applyfSET), _tort_m_object___applyfSET)
tort_d_m(tort__mt(object), tort__s(_cannot_apply), _tort_m_object___cannot_apply)
tort_d_m(tort__mt(object), tort__s(_gc_free), _tort_m_object___gc_free)
tort_d_m(tort__mt(object), tort__s(_gc_mark), _tort_m_object___gc_mark)
tort_d_m(tort__mt(object), tort__s(_inspect), _tort_m_object___inspect)
tort_d_m(tort__mt(object), tort__s(_method_not_found), _tort_m_object___method_not_found)
tort_d_m(tort__mt(object), tort__s(_mtable), _tort_m_object___mtable)
tort_d_m(tort__mt(object), tort__s(_mtableSET), _tort_m_object___mtableSET)
tort_d_m(tort__mt(object), tort__s(_name), _tort_m_object___name)
tort_d_m(tort__mt(object), tort__s(_object_ptr), _tort_m_object___object_ptr)
tort_d_m(tort__mt(object), tort__s(_printfv), _tort_m_object___printfv)
tort_d_m(tort__mt(object), tort__s(_set_slot_at), _tort_m_object___set_slot_at)
tort_d_m(tort__mt(object), tort__s(_slot_at), _tort_m_object___slot_at)
tort_d_m(tort__mt(object), tort__s(_slot_locative_at), _tort_m_object___slot_locative_at)
tort_d_m(tort__mt(object), tort__s(_tag), _tort_m_object___tag)
tort_d_m(tort__mt(object), tort__s(_to_string), _tort_m_object___to_string)
tort_d_m(tort__mt(object), tort__s(clone), _tort_m_object__clone)
tort_d_m(tort__mt(object), tort__s(eqQ), _tort_m_object__eqQ)
tort_d_m(tort__mt(object), tort__s(equalQ), _tort_m_object__equalQ)
tort_d_m(tort__mt(object), tort__s(identity), _tort_m_object__identity)
tort_d_m(tort__mt(object), tort__s(initialize), _tort_m_object__initialize)
tort_d_m(tort__mt(object), tort__s(not), _tort_m_object__not)
tort_d_m(tort__mt(pair), tort__s(_inspect), _tort_m_pair___inspect)
tort_d_m(tort__mt(pair), tort__s(equalQ), _tort_m_pair__equalQ)
tort_d_m(tort__mt(pair), tort__s(first), _tort_m_pair__first)
tort_d_m(tort__mt(pair), tort__s(firstSET), _tort_m_pair__firstSET)
tort_d_m(tort__mt(pair), tort__s(second), _tort_m_pair__second)
tort_d_m(tort__mt(pair), tort__s(secondSET), _tort_m_pair__secondSET)
tort_d_m(tort__mt(ptr), tort__s(EQ), _tort_m_ptr__EQ)
tort_d_m(tort__mt(ptr), tort__s(GE), _tort_m_ptr__GE)
tort_d_m(tort__mt(ptr), tort__s(GT), _tort_m_ptr__GT)
tort_d_m(tort__mt(ptr), tort__s(LAND), _tort_m_ptr__LAND)
tort_d_m(tort__mt(ptr), tort__s(LE), _tort_m_ptr__LE)
tort_d_m(tort__mt(ptr), tort__s(LOR), _tort_m_ptr__LOR)
tort_d_m(tort__mt(ptr), tort__s(LT), _tort_m_ptr__LT)
tort_d_m(tort__mt(ptr), tort__s(NE), _tort_m_ptr__NE)
tort_d_m(tort__mt(ptr), tort__s(NOT), _tort_m_ptr__NOT)
tort_d_m(tort__mt(ptr), tort__s(_ccall), _tort_m_ptr___ccall)
tort_d_m(tort__mt(ptr), tort__s(_ccallv), _tort_m_ptr___ccallv)
tort_d_m(tort__mt(ptr), tort__s(_inspect), _tort_m_ptr___inspect)
tort_d_m(tort__mt(ptr), tort__s(_ptr_data), _tort_m_ptr___ptr_data)
tort_d_m(tort__mt(ptr), tort__s(_ptr_object), _tort_m_ptr___ptr_object)
tort_d_m(tort__mt(ptr), tort__s(_to_string), _tort_m_ptr___to_string)
tort_d_m(tort__mt(ptr), tort__s(equalQ), _tort_m_ptr__equalQ)
tort_d_m(tort__mt(string), tort__s(__printfsv), _tort_m_string____printfsv)
tort_d_m(tort__mt(string), tort__s(__write), _tort_m_string____write)
tort_d_m(tort__mt(string), tort__s(_dlopen), _tort_m_string___dlopen)
tort_d_m(tort__mt(string), tort__s(_error), _tort_m_string___error)
tort_d_m(tort__mt(string), tort__s(_fatal), _tort_m_string___fatal)
tort_d_m(tort__mt(string), tort__s(_inspect), _tort_m_string___inspect)
tort_d_m(tort__mt(string), tort__s(_to_string), _tort_m_string___to_string)
tort_d_m(tort__mt(string), tort__s(_write), _tort_m_string___write)
tort_d_m(tort__mt(string), tort__s(close), _tort_m_string__close)
tort_d_m(tort__mt(string), tort__s(escape), _tort_m_string__escape)
tort_d_m(tort__mt(string), tort__s(flush), _tort_m_string__flush)
tort_d_m(tort__mt(string), tort__s(get), _tort_m_string__get)
tort_d_m(tort__mt(string), tort__s(set), _tort_m_string__set)
tort_d_m(tort__mt(string), tort__s(unescapeE), _tort_m_string__unescapeE)
tort_d_m(tort__mt(symbol), tort__s(_inspect), _tort_m_symbol___inspect)
tort_d_m(tort__mt(symbol), tort__s(_to_string), _tort_m_symbol___to_string)
tort_d_m(tort__mt(symbol), tort__s(_version_change), _tort_m_symbol___version_change)
tort_d_m(tort__mt(symbol), tort__s(mtable_method_map), _tort_m_symbol__mtable_method_map)
tort_d_m(tort__mt(symbol), tort__s(name), _tort_m_symbol__name)
tort_d_m(tort__mt(symbol), tort__s(version), _tort_m_symbol__version)
tort_d_m(tort__mt(tagged), tort__s(_object_ptr), _tort_m_tagged___object_ptr)
tort_d_m(tort__mt(vector), tort__s(_gc_mark), _tort_m_vector___gc_mark)
tort_d_m(tort__mt(vector), tort__s(_inspect), _tort_m_vector___inspect)
tort_d_m(tort__mt(vector), tort__s(add), _tort_m_vector__add)
tort_d_m(tort__mt(vector), tort__s(each), _tort_m_vector__each)
tort_d_m(tort__mt(vector), tort__s(equalQ), _tort_m_vector__equalQ)
tort_d_m(tort__mt(vector), tort__s(get), _tort_m_vector__get)
tort_d_m(tort__mt(vector), tort__s(map), _tort_m_vector__map)
tort_d_m(tort__mt(vector), tort__s(set), _tort_m_vector__set)
tort_d_m(tort__mt(vector_base), tort__s(_add), _tort_m_vector_base___add)
tort_d_m(tort__mt(vector_base), tort__s(_append), _tort_m_vector_base___append)
tort_d_m(tort__mt(vector_base), tort__s(_data), _tort_m_vector_base___data)
tort_d_m(tort__mt(vector_base), tort__s(_delete_n), _tort_m_vector_base___delete_n)
tort_d_m(tort__mt(vector_base), tort__s(_gc_free), _tort_m_vector_base___gc_free)
tort_d_m(tort__mt(vector_base), tort__s(_initialize), _tort_m_vector_base___initialize)
tort_d_m(tort__mt(vector_base), tort__s(_ref), _tort_m_vector_base___ref)
tort_d_m(tort__mt(vector_base), tort__s(alloc_size), _tort_m_vector_base__alloc_size)
tort_d_m(tort__mt(vector_base), tort__s(append), _tort_m_vector_base__append)
tort_d_m(tort__mt(vector_base), tort__s(clone), _tort_m_vector_base__clone)
tort_d_m(tort__mt(vector_base), tort__s(element_size), _tort_m_vector_base__element_size)
tort_d_m(tort__mt(vector_base), tort__s(emptyE), _tort_m_vector_base__emptyE)
tort_d_m(tort__mt(vector_base), tort__s(equalQ), _tort_m_vector_base__equalQ)
tort_d_m(tort__mt(vector_base), tort__s(resize), _tort_m_vector_base__resize)
tort_d_m(tort__mt(vector_base), tort__s(size), _tort_m_vector_base__size)
tort_d_m(tort_h_ref(tort__mt(caller_info))->mtable, tort__s(_offset_data), _tort_M_caller_info___offset_data)
tort_d_m(tort_h_ref(tort__mt(caller_info))->mtable, tort__s(_offset_file), _tort_M_caller_info___offset_file)
tort_d_m(tort_h_ref(tort__mt(caller_info))->mtable, tort__s(_offset_line), _tort_M_caller_info___offset_line)
tort_d_m(tort_h_ref(tort__mt(dynlib))->mtable, tort__s(new), _tort_M_dynlib__new)
tort_d_m(tort_h_ref(tort__mt(fixnum))->mtable, tort__s(_ops), _tort_M_fixnum___ops)
tort_d_m(tort_h_ref(tort__mt(gc))->mtable, tort__s(gc_collect), _tort_M_gc__gc_collect)
tort_d_m(tort_h_ref(tort__mt(gc))->mtable, tort__s(gc_stats), _tort_M_gc__gc_stats)
tort_d_m(tort_h_ref(tort__mt(gc))->mtable, tort__s(mark), _tort_M_gc__mark)
tort_d_m(tort_h_ref(tort__mt(io))->mtable, tort__s(__create), _tort_M_io____create)
tort_d_m(tort_h_ref(tort__mt(io))->mtable, tort__s(__stat), _tort_M_io____stat)
tort_d_m(tort_h_ref(tort__mt(io))->mtable, tort__s(create), _tort_M_io__create)
tort_d_m(tort_h_ref(tort__mt(map))->mtable, tort__s(_offset_equality), _tort_M_map___offset_equality)
tort_d_m(tort_h_ref(tort__mt(map))->mtable, tort__s(new), _tort_M_map__new)
tort_d_m(tort_h_ref(tort__mt(message))->mtable, tort__s(_offset_argc), _tort_M_message___offset_argc)
tort_d_m(tort_h_ref(tort__mt(message))->mtable, tort__s(_offset_caller_info), _tort_M_message___offset_caller_info)
tort_d_m(tort_h_ref(tort__mt(message))->mtable, tort__s(_offset_fiber), _tort_M_message___offset_fiber)
tort_d_m(tort_h_ref(tort__mt(message))->mtable, tort__s(_offset_method), _tort_M_message___offset_method)
tort_d_m(tort_h_ref(tort__mt(message))->mtable, tort__s(_offset_mtable), _tort_M_message___offset_mtable)
tort_d_m(tort_h_ref(tort__mt(message))->mtable, tort__s(_offset_previous_message), _tort_M_message___offset_previous_message)
tort_d_m(tort_h_ref(tort__mt(message))->mtable, tort__s(_offset_receiver), _tort_M_message___offset_receiver)
tort_d_m(tort_h_ref(tort__mt(message))->mtable, tort__s(_offset_selector), _tort_M_message___offset_selector)
tort_d_m(tort_h_ref(tort__mt(message))->mtable, tort__s(new), _tort_M_message__new)
tort_d_m(tort_h_ref(tort__mt(method))->mtable, tort__s(_offset_data), _tort_M_method___offset_data)
tort_d_m(tort_h_ref(tort__mt(method))->mtable, tort__s(_offset_name), _tort_M_method___offset_name)
tort_d_m(tort_h_ref(tort__mt(mtable))->mtable, tort__s(_offset_data), _tort_M_mtable___offset_data)
tort_d_m(tort_h_ref(tort__mt(mtable))->mtable, tort__s(_offset_gc_data), _tort_M_mtable___offset_gc_data)
tort_d_m(tort_h_ref(tort__mt(mtable))->mtable, tort__s(_offset_gc_free_method), _tort_M_mtable___offset_gc_free_method)
tort_d_m(tort_h_ref(tort__mt(mtable))->mtable, tort__s(_offset_gc_mark_method), _tort_M_mtable___offset_gc_mark_method)
tort_d_m(tort_h_ref(tort__mt(mtable))->mtable, tort__s(_offset_instance_size), _tort_M_mtable___offset_instance_size)
tort_d_m(tort_h_ref(tort__mt(mtable))->mtable, tort__s(new_class), _tort_M_mtable__new_class)
tort_d_m(tort_h_ref(tort__mt(mtable))->mtable, tort__s(new_mtable), _tort_M_mtable__new_mtable)
tort_d_m(tort_h_ref(tort__mt(object))->mtable, tort__s(_allocate), _tort_M_object___allocate)
tort_d_m(tort_h_ref(tort__mt(object))->mtable, tort__s(allocate), _tort_M_object__allocate)
tort_d_m(tort_h_ref(tort__mt(object))->mtable, tort__s(new), _tort_M_object__new)
tort_d_m(tort_h_ref(tort__mt(pair))->mtable, tort__s(_offset_first), _tort_M_pair___offset_first)
tort_d_m(tort_h_ref(tort__mt(pair))->mtable, tort__s(_offset_second), _tort_M_pair___offset_second)
tort_d_m(tort_h_ref(tort__mt(pair))->mtable, tort__s(new), _tort_M_pair__new)
tort_d_m(tort_h_ref(tort__mt(string))->mtable, tort__s(_new), _tort_M_string___new)
tort_d_m(tort_h_ref(tort__mt(string))->mtable, tort__s(new), _tort_M_string__new)
tort_d_m(tort_h_ref(tort__mt(symbol))->mtable, tort__s(_create), _tort_M_symbol___create)
tort_d_m(tort_h_ref(tort__mt(symbol))->mtable, tort__s(_offset_mtable_method_map), _tort_M_symbol___offset_mtable_method_map)
tort_d_m(tort_h_ref(tort__mt(symbol))->mtable, tort__s(_offset_name), _tort_M_symbol___offset_name)
tort_d_m(tort_h_ref(tort__mt(symbol))->mtable, tort__s(_offset_version), _tort_M_symbol___offset_version)
tort_d_m(tort_h_ref(tort__mt(symbol))->mtable, tort__s(new), _tort_M_symbol__new)
tort_d_m(tort_h_ref(tort__mt(vector))->mtable, tort__s(_new), _tort_M_vector___new)
tort_d_m(tort_h_ref(tort__mt(vector))->mtable, tort__s(new), _tort_M_vector__new)
tort_d_m(tort_h_ref(tort__mt(vector_base))->mtable, tort__s(_new), _tort_M_vector_base___new)
tort_d_m(tort_h_ref(tort__mt(vector_base))->mtable, tort__s(_offset_alloc_size), _tort_M_vector_base___offset_alloc_size)
tort_d_m(tort_h_ref(tort__mt(vector_base))->mtable, tort__s(_offset_element_size), _tort_M_vector_base___offset_element_size)
tort_d_m(tort_h_ref(tort__mt(vector_base))->mtable, tort__s(_offset_size), _tort_M_vector_base___offset_size)
/* -*- c -*- */
#undef tort_d_m

