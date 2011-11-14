#ifndef _tort_BLOCK_H
#define _tort_BLOCK_H

#include "tort/tort.h"

/**
   Blocks respond to #value by returning themselves as
   the method in block mt #lookup, if the selector is 
   #value.

   This forces the applyf of the block's #value to be the 
   method implementation of the block itself.

   This is done to avoid creating a new mtable for each block.

   The block is the receiver of the #value message.

   This uses the GCC nested function feature; see gcc -fnested-functions.
*/
typedef struct tort_block { tort_H;
  tort_method _;
  tort_message *scope;
  void *data;
} tort_block;
tort_h_struct(tort_block);

#define tort_block_(BLK,PARAMS...)					\
  tort_block_ BLK##_ = { { 0 } };					\
  BLK##_._h.applyf = _tort_m_object___cannot_apply;			\
  BLK##_._h.mtable = tort__mt(block);					\
  tort_block *BLK = &BLK##_._;						\
  BLK->scope = _tort_message;						\
  tort_v BLK##_f (tort_thread_param tort_block *_block, ##PARAMS)

#define tort_block_END(BLK)			\
  BLK##_._h.applyf = (void*) BLK##_f;

tort_v tort_runtime_initialize_block();

#endif
