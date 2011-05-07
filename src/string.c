#include "tort/core.h"

tort_v _tort_m_string__get (tort_thread_param tort_string *rcvr, tort_v _i)
{
  long i = tort_I(_i);
  return tort_i(rcvr->data[i]);
}

tort_v _tort_m_string__set (tort_thread_param tort_string *rcvr, tort_v _i, tort_v _v)
{
  long i = tort_I(_i);
  long v = tort_I(_v);
  rcvr->data[i] = v;
  return rcvr;
}

tort_v _tort_M_string___new(tort_tp tort_mtable *mtable, const char *string, size_t size)
{
  tort_string *v = tort_vector_base_new(mtable, string, size, sizeof(string[0]));
  if ( ! string ) {
    memset(v->data, 0, v->alloc_size);
  }
  return v;
}

tort_v _tort_M_string__new(tort_tp tort_mtable *mtable, tort_v size)
{
  return_tort_send(tort__s(_new), mtable, 0, tort_I(size));
}

tort_v _tort_m_string__escapeE(tort_tp tort_string *str)
{
  char *dst, *src = str->data, *src_end = str->data + str->size;
  dst = src;
  while ( src < src_end ) {
    if ( *(src ++) == '\\' && src < src_end ) {
      int c;
      switch ( (c = *(src ++)) ) {
      case 'a': c = '\a'; break;
      case 'b': c = '\b'; break;
      case 'e': c = '\e'; break;
      case 'n': c = '\n'; break;
      case 'r': c = '\r'; break;
      case 't': c = '\t'; break;
      case '0':
	c -= '0'; 
	c <<= 3; c += *(src ++) - '0';
	c <<= 3; c += *(src ++) - '0';	
      case 'x': {
	{ int x = c;
	  x = x >= 'a' ? x - 'a' + 10 : x >= 'A' ? x - 'A' + 10 : x - '0';
	  c = x;
	  x = *(src ++);
	  x = x >= 'a' ? x - 'a' + 10 : x >= 'A' ? x - 'A' + 10 : x - '0';
	  c <<= 4; c += x;
	}
      }
      }
      *(dst ++) = c;
    } else {
      dst ++;
    }
  }
  *dst = 0;
  str->size = dst - str->data;
  return str;
}

/********************************************************************/

tort_v tort_string_new(const char *ptr, size_t size)
{
  return _tort_M_string___new(tort_ta tort__mt(string), ptr, size);
}

tort_v tort_string_new_cstr(const char *string)
{
  return tort_string_new(string, strlen(string));
}


