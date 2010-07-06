#ifndef _tort_BLOCK_H
#define _tort_BLOCK_H

#include "tort/tort.h"

typedef struct tort_block {
  int opaque;
} tort_block;

typedef struct _tort_block {
  tort_header _h;
  tort_block _;
} _tort_block;

#define tort_block_(args...)			\
  ({						\
  _tort_block _blk = {				\
    { sizeof(tort_block),			\
      _tort_block_lookupf,			\
      _tort_object_applyf,			\
      _tort->_mt_block				\
    },						\
  };						\
  tort_val _blk_f (tort_val _blk_msg, tort_val _blk_obj, ##args)

#define tort_block_end()			\
  _blk._h.applyf = (void*) _blk_f;		\
  tort_ref_box(&_blk._);			\
  })

extern tort_apply_decl(_tort_block_lookupf);

#endif
