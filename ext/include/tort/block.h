#ifndef _tort_BLOCK_H
#define _tort_BLOCK_H

#include "tort/tort.h"

/**
   Blocks respond to #value by returning themselves as
   the method in the lookupf, if the selector is 
   #value.

   This forces the applyf of the block's #value to be the 
   method implementation of the block itself.

   This is done to avoid creating a new mtable for each block.

   The block is the receiver of the #value message.

   This uses the GCC nested function feature; see gcc -fnested-functions.
*/
typedef struct tort_block {
  tort_message *scope;
  void *data;
} tort_block;

typedef struct _tort_block {
  tort_header _h;
  tort_block _;
} _tort_block;

#define tort_block_(args...)					\
  ({								\
  _tort_block _blk = {						\
    { sizeof(tort_block),					\
      _tort_block_lookupf,					\
      _tort_object_applyf,					\
      tort__mt(block)						\
    },								\
    { _tort_message, 0 },					\
  };								\
  tort_v _blk_f (tort_thread_param tort_block *_blk_obj, ##args)

#define tort_block_end()				\
  _blk._h.applyf = (void*) _blk_f;			\
  tort_send(tort__s(clone), tort_ref_box(&_blk._));	\
  })

extern tort_apply_decl(_tort_block_lookupf);

tort_v tort_runtime_initialize_block();

#endif
