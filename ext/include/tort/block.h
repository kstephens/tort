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
typedef struct tort_block { tort_H;
  tort_message *scope;
  void *data;
} tort_block;

typedef struct tort_block_ {
  tort_header _h;
  tort_block _block;
} tort_block_;

#define tort_block_(BLK,PARAMS...)					\
  tort_block_ BLK##_ = {						\
    { sizeof(tort_block),						\
      _tort_block_lookupf,						\
      _tort_object_applyf,						\
      tort__mt(block)							\
    },									\
    { { }, _tort_message, 0 },						\
  };									\
  tort_block *BLK = &BLK##_._block;					\
  tort_v BLK##_f (tort_thread_param tort_block *_block, ##PARAMS)

#define tort_block_END(BLK)			\
  BLK##_._h.applyf = (void*) BLK##_f;

extern tort_apply_decl(_tort_block_lookupf);

tort_v tort_runtime_initialize_block();

#endif
