#ifndef _tort_CONFIG_H
#define _tort_CONFIG_H

#ifndef TORT_MULTIPLICITY
#define TORT_MULTIPLICITY 0 /* if true, allow multiple tort_runtime instances. */
#endif

#ifndef TORT_GLOBAL_MCACHE
#define TORT_GLOBAL_MCACHE 1 /* if true, use a global method cache. */
#endif

#ifndef TORT_GLOBAL_MCACHE_STATS
#define TORT_GLOBAL_MCACHE_STATS 0 /* if true, capture global method cache stats. */
#endif

#ifndef TORT_MCACHE_USE_SYMBOL_VERSION
#define TORT_MCACHE_USE_SYMBOL_VERSION 1 /* if true, use symbol versioning instead of flusing the entire global method caches when a new method is added. */
#endif

#ifndef TORT_NIL_IS_ZERO
#define TORT_NIL_IS_ZERO 1 /* if true, use (tort_v) 0 as nil */
#endif

#ifndef TORT_ALLOC_DEBUG
#define TORT_ALLOC_DEBUG 0
#endif

#ifndef TORT_LOOKUP_TRACE
#define TORT_LOOKUP_TRACE 0
#endif

#endif
