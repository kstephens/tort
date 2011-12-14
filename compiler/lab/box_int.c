#include "tort/tort.h"

tort_v _tort_box_int(tort_vi i)
{
  return tort_i(i);
}

tort_vi _tort_unbox_int(tort_v o)
{
  return tort_I(o);
}

