#ifndef _tort_CONFIG_H
#define _tort_CONFIG_H

#ifndef TORT_TAG_BITS
#define TORT_TAG_BITS 2
#endif

#if TORT_TAG_BITS >= 1
#define tort_tag_fixnum 1
#endif
#if TORT_TAG_BITS >= 2
#define tort_tag_locative 3
#endif

#ifndef TORT_MULTIPLICITY
#define TORT_MULTIPLICITY 0 /* if true, allow multiple tort_runtime instances. */
#endif

#ifndef TORT_GLOBAL_MCACHE
#define TORT_GLOBAL_MCACHE 1 /* if true, use a global method cache. */
#endif

#ifndef TORT_GLOBAL_MCACHE_STATS
#define TORT_GLOBAL_MCACHE_STATS 1 /* if true, capture global method cache stats. */
#endif

#ifndef TORT_MCACHE_USE_SYMBOL_VERSION
#define TORT_MCACHE_USE_SYMBOL_VERSION 1 /* if true, use symbol versioning instead of flushing the entire global method caches when a new method is added. */
#endif

#ifndef TORT_ANON_SYMBOL_MTABLE
#define TORT_ANON_SYMBOL_MTABLE 1 /* if true, anon symbols use their own map for mtable -> method. */
#endif

#ifndef TORT_NIL_IS_ZERO
#define TORT_NIL_IS_ZERO 1 /* if true, use (tort_v) 0 as nil */
#endif

#ifndef TORT_FALSE_IS_NIL
#define TORT_FALSE_IS_NIL 0 /* if true, tort_false = tort_nil */
#endif

#ifndef TORT_MESSAGE_FILE_LINE
#define TORT_MESSAGE_FILE_LINE 1 /* if true, messages contain file, line info. */
#endif

#ifndef TORT_ALLOC_DEBUG
#define TORT_ALLOC_DEBUG 0
#endif

#ifndef TORT_LOOKUP_TRACE
#define TORT_LOOKUP_TRACE 1
#endif

#ifndef TORT_GC_STATS
#define TORT_GC_STATS 1
#endif

#endif
