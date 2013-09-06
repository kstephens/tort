#include "tort/core.h"
#include <stdlib.h> /* strtoll */

tort_v _tort_m_string__get (tort_tp tort_string *rcvr, tort_v i)
{
  return tort_c(((unsigned char *)rcvr->data)[tort_I(i)]);
}

tort_v _tort_m_string__set (tort_tp tort_string *rcvr, tort_v i, tort_v v)
{
  ((unsigned char *)(rcvr->data))[tort_I(i)] = tort_C(v);
  return rcvr;
}

tort_v _tort_m_string__unescapeE(tort_tp tort_string *str)
{
  unsigned char *dst, *src = (void*) str->data, *src_end = (void*) (str->data + str->size);
  dst = src;
  while ( src < src_end ) {
    int c = *(src ++);
    if ( c == '\\' && src < src_end ) {
      int d = 0;
      switch ( (c = *(src ++)) ) {
      case 'a': c = '\a'; break;
      case 'b': c = '\b'; break;
      case 'e': c = '\e'; break;
      case 'n': c = '\n'; break;
      case 'r': c = '\r'; break;
      case 't': c = '\t'; break;
      case '0' ... '7':
	c -= '0'; 
	if ( src >= src_end ) break;
	c <<= 3; c += (*(src ++) - '0') & 7;
	if ( src >= src_end ) break;
	c <<= 3; c += (*(src ++) - '0') & 7;	
	break;
      case 'x': 
	d = c;
	d = (d >= 'a' ? d - 'a' + 10 : d >= 'A' ? d - 'A' + 10 : d - '0') & 15;
	c = d;
	if ( src >= src_end ) break;
	d = *(src ++);
	d = (d >= 'a' ? d - 'a' + 10 : d >= 'A' ? d - 'A' + 10 : d - '0') & 15;
	c <<= 4; c += d;
	break;
      }
    }
    *(dst ++) = c;
  }
  *dst = 0;
  str->size = (char*) dst - (char*) str->data;
  return str;
}

tort_v _tort_m_string__escape(tort_tp tort_string *str, tort_v more)
{
  tort_string *result = tort_send(tort__s(clone), str);
  unsigned char *dst, *src = (void*) str->data, *src_end = (void*) (str->data + str->size);
  tort_send(tort__s(resize), result, tort_i(str->size * 4)); /* 4 = '\x00' or '\000' */
  dst = (void*) result->data;
  while ( src < src_end ) {
    int c = *(src ++);
    if ( c == '\\' || (more != tort_nil && c == '"') ) {
      *(dst ++) = '\\';
    } else if ( ! (32 <= c || c < 127)  ) { 
      *(dst ++) = '\\'; 
      switch ( c ) {
      case '\a': c = 'a'; break;
      case '\b': c = 'b'; break;
      case '\e': c = 'e'; break;
      case '\n': c = 'n'; break;
      case '\r': c = 'r'; break;
      case '\t': c = 't'; break;
      default:
	*(dst ++) = '0' + ((c >> 6) & 0x7);
	*(dst ++) = '0' + ((c >> 3) & 0x7);
	c         = '0' + ((c >> 0) & 0x7);
      }
    }
    *(dst ++) = c;
  }
  *dst = 0;
  return_tort_send(tort__s(resize), result, tort_i((char*) dst - (char*) result->data));
}

tort_v _tort_m_string__to_number(tort_tp tort_string *s, tort_v _radix)
{
  int radix = tort_I(_radix);
  char *str = s->data, *endptr = str;
  long long x = strtoll(str, &endptr, radix);
  tort_v n = tort_i(x);
  return tort_I(n) == x && endptr != str ? n : tort_false;
}

tort_v tort_string_new(const char *ptr, size_t size)
{
  if ( tort_(_initialized) )
    return tort_send(tort__s(_new), tort__mt(string), ptr, size);
  else
    return _tort_M_u8vector___new(tort_ta tort__mt(string), ptr, size);
}

tort_v tort_string_new_cstr(const char *string)
{
  return tort_string_new(string, string ? strlen(string) : 0);
}
