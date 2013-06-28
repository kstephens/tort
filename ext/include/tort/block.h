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
#ifdef TORT_CLANG_BLOCKS
  tort_v (^clang_block)(tort_tp struct tort_block *_block, ...);
#endif
} tort_block;
tort_h_struct(tort_block);

#define tort_block_(BLK,PARAMS...)					\
  tort_block_ BLK##_ = { { 0 } };					\
  BLK##_._h.applyf = _tort_m_object___cannot_apply;			\
  BLK##_._h.mtable = tort__mt(block);					\
  tort_block *BLK = &BLK##_._;						\
  BLK->scope = _tort_message;						\
  tort_block__(BLK,##PARAMS)

#ifdef TORT_CLANG_BLOCKS
#define tort_block_var __block
#define tort_block__(BLK,PARAMS...)                                     \
  BLK->clang_block = (void*) ^ tort_v (tort_tp tort_block *_block, ##PARAMS)
#define tort_block_END(BLK)                                             \
  ; BLK##_._h.applyf = (void*) BLK->clang_block;
#else
#define tort_block_var
#define tort_block__(BLK,PARAMS...)                                     \
  tort_v BLK##_f (tort_tp tort_block *_block, ##PARAMS)
#define tort_block_END(BLK)			                        \
  BLK##_._h.applyf = (void*) BLK##_f;
#endif

tort_v tort_runtime_initialize_block();

#endif
