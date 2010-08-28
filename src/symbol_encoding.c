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
    { "ADD",  "+" },
    { "SUB",  "-" },
    { "MUL",  "*" },
    { "DIV",  "/" },
    { "NEG",  "-" },
    { "MOD",  "%" },
    { "LT",   "<" },
    { "GT",   ">" },
    { "LE",   "<=" },
    { "GE",   ">=" },
    { "EQ",   "==" },
    { "NE",   "!=" },
    { "P",    "%" },
    { "E",    "!" },
    { "Q",    "?" },
    { 0, 0 },
  };

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

const char *tort_encode_symbol(const char *in)
{
  const char *s = in;
  char *out = tort_malloc(sizeof(out[0]) * (strlen(in) + 1));
  char *t = out;

  while ( *s ) {
    tort_symbol_mapping *sm = find_mapping(mappings, s);
    if ( sm ) {
      const char *r = sm->replace;
      while ( *r ) {
	*(t ++) = *(r ++);
      }
      s += sm->pattern_size;
    } else {
      *(t ++) = *(s ++);
    }
  }
  *t = '\0';
  return out;
}


#include <stdio.h>

int main(int argc, char **argv)
{
#define T(X) \
  printf("%s => %s\n", #X, tort_encode_symbol(#X))
  T(_);
  T(asdfasd);
  T(string_TO_symbol);
  T(__fooE);
  T(PPfooQ);

  return 0;
}


