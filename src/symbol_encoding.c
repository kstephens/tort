#include "tort/core.h"

#include <string.h>

typedef struct tort_symbol_mapping {
  const char *pattern;
  const char *replace;
  int pattern_size;
} tort_symbol_mapping;

static tort_symbol_mapping mappings[] =
  {
    { "_TO_", "->" },
    { "DOT",  "." },
#define UOP(N,OP) { #N, #OP },
#define BOP(N,OP) { #N, #OP },
#define ROP(N,OP) { #N, #OP },
#include "tort/ops.h"
    { "D",    "-" },
    { "P",    "%" },
    { "E",    "!" },
    { "Q",    "?" },
    { 0, 0 },
  };

static
int sm_cmp(const void *a, const void *b)
{
  return ((tort_symbol_mapping*) b)->pattern_size - ((tort_symbol_mapping*) a)->pattern_size;
}

static
void sort_symbol_mappings()
{
  int n = 0;
  tort_symbol_mapping *sm = mappings;
  while ( sm->pattern ) {
    ++ sm;
    ++ n;
  }
  qsort(mappings, n, sizeof(mappings[0]), sm_cmp);
}

static
tort_symbol_mapping *find_mapping(tort_symbol_mapping *sm, const char *s)
{
  while ( sm->pattern ) {
    if ( ! sm->pattern_size ) {
      sm->pattern_size = strlen(sm->pattern);
    }
    if ( strncmp(s, sm->pattern, sm->pattern_size) == 0 ) {
      return sm;
    }
    ++ sm;
  }
  return 0;
}

const char *tort_symbol_encode(const char *in)
{
  const char *s = in;
  char *out = tort_malloc(sizeof(out[0]) * (strlen(in) + 1));
  char *t = out;
  int encoded = 0;
  static int sorted = 0;

  if ( ! sorted ) {
    ++ sorted;
    sort_symbol_mappings();
  }

  while ( *s ) {
    tort_symbol_mapping *sm = find_mapping(mappings, s);
    if ( sm ) {
      const char *r = sm->replace;
      encoded = 1;
      while ( *r ) {
	*(t ++) = *(r ++);
      }
      s += sm->pattern_size;
    } else {
      *(t ++) = *(s ++);
    }
  }
  *t = '\0';

  if ( encoded ) {
    return out;
  } else {
    tort_free(out);
    return in;
  }
}


