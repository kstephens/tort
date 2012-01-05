#include "tort/core.h"
#include <string.h>

typedef struct tort_symbol_mapping {
  const char *pattern;
  const char *replace;
  int pattern_size;
} tort_symbol_mapping;

static tort_symbol_mapping mappings[] =
  {
    { "CMP",  "<=>" },
    { "_TO_", "->" },
    { "DOT",  "." },
    { "SET",  "=" },
    { "LOC",  "&" },
#define UOP(N,OP) { #N, #OP },
#define LUP(N,OP) { #N, #OP },
#define BOP(N,OP) { #N, #OP },
#define LOP(N,OP) { #N, #OP },
#define ROP(N,OP) { #N, #OP },
#define UOP_NO_NEG 1
#include "tort/ops.h"
    { "NEG",  "@-" },
    { "D",    "-" },
    { "P",    "%" },
    { "E",    "!" },
    { "Q",    "?" },
    { "S",    "*" },
    { "A",    "&" },
    { "C",    ":" },
    { "T",    "~" },
    { "H",    "#" },
    { "L",    "$" },
    { "B",    "`" },
    { "F",    "'" },
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
    sm->pattern_size = strlen(sm->pattern);
    ++ sm;
    ++ n;
  }
  qsort(mappings, n, sizeof(mappings[0]), sm_cmp);
}

static
tort_symbol_mapping *find_mapping(tort_symbol_mapping *sm, const char *s)
{
  while ( sm->pattern ) {
    if ( ! strncmp(s, sm->pattern, sm->pattern_size) )
      return sm;
    ++ sm;
  }
  return 0;
}

static
const char *_tort_symbol_encode(const char *in)
{
  const char *s = in;
  char *out = tort_malloc_atomic(sizeof(out[0]) * (strlen(in) + 1));
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
      while ( *r )
	*(t ++) = *(r ++);
      s += sm->pattern_size;
      encoded = 1;
    } else {
      *(t ++) = *(s ++);
    }
  }
  *t = '\0';

  if ( encoded ) {
    // fprintf(stderr, "  symbol_encode %s => %s\n", in, out); 
    return out;
  } else {
    tort_free_atomic(out);
    return in;
  }
}

tort_v _tort_m_symbol_encoder__encode(tort_tp tort_mtable *mtable, tort_v str)
{
  const char *s = tort_string_data(str);
  const char *t = tort_symbol_encode(s);
  return s == t ? str : tort_string_new_cstr(t);
}

const char *tort_symbol_encode(const char *in)
{
  if ( ! in ) return in;
  if ( 0 && tort_(symbol_encoder) )
    return tort_string_data(tort_send(tort__s(encode), tort_(symbol_encoder), tort_string_new_cstr(in)));
  else
    return _tort_symbol_encode(in);
}

tort_symbol* tort_symbol_new_encode(const char *string)
{
  return tort_symbol_new(tort_symbol_encode(string));
}

tort_v tort_runtime_initialize_symbol_encoder()
{
  tort_(symbol_encoder) = tort_send(tort__s(new), tort__mt(symbol_encoder));
  tort_send(tort__s(set), tort_(root), tort_s(symbol_encoder), tort_(symbol_encoder));
  return 0;
}
