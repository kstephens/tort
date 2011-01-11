#ifndef _tort_BOX_H
#define _tort_BOX_H

#define tort_box_(T,V) tort_box_##T(V)
#define tort_unbox_(T,V) tort_unbox_##T(V)

#define tort_box_tort_v(V)((tort_v)(V))
#define tort_unbox_tort_v(V)((tort_v)(V))

#define tort_box_size_t(V)tort_i(V)
#define tort_unbox_size_t(V)tort_I(V)

#endif
