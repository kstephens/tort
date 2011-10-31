#ifndef _tort_BOX_H
#define _tort_BOX_H

#define tort_box_(T,V) tort_box_##T(V)
#define tort_unbox_(T,V) tort_unbox_##T(V)

#define tort_box_tort_v(V)((tort_v)(V))
#define tort_unbox_tort_v(V)((tort_v)(V))

#define tort_box_size_t(V)tort_i(V)
#define tort_unbox_size_t(V)tort_I(V)

#define tort_box_int(V)tort_i(V)
#define tort_unbox_int(V)tort_I(V)

#define tort_box_charP(V)tort_string_new_cstr(V)
#define tort_unbox_charP(V)tort_string_charP(V)

#define tort_box_voidP(V)tort_ptr_new(V)
#define tort_unbox_voidP(V)tort_ptr_data(V)

#endif
