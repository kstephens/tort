/*
** lispread.c - a generic lisp reader.
** Copyright 1998, 1999 Kurt A. Stephens http://www.acm.org/~stephensk
*/
/*
This lisp reader is very minimal, it does not implement the full Common Lisp syntax.  In general it is compliant with the Revised 5 Scheme Report.  

The ';', '(', ')', '#' and whitespace characters are token terminators.
Tokens that are not numbers are assumed to be symbols.
This reader does not decaseify symbols before calling STRING_2_SYMBOL.
';' comments are treated as whitespace.
Note: the "#!" comment allows lisp files to executable scripts on unix systems that support '#!/usr/local/bin/lisp'.

The following synactic structures can be read:

Comments      ;...\n, #!...\n
Quote         'x
Lists         (a b ...), (a b ... '.' c)
Vectors       #(a b ...)
Characters    #\b (#\space and #\newline are not IMPLEMENTED)
False         #f, #F
True          #t, #T
Unspecified   #u, #U (Opt.)
Numbers       #b0101002, #o1726m #d2349, #x0123456789abcedf, 1234, 1234.00, etc.
Strings       "...", "\"\\"
Symbols       asdf, +, etc.

To use lispread.c you must defined the following macros and #include "lispread.c"
to "glue" it to your code.

Macros declared "Opt." are optional.

Macro               Implementation
==========================================================================
VALUE               The C type for a lisp value.
READ_DECL           A C function definition for the lisp read function.
                    Within the body of READ_DECL, the "stream" variable must 
	            be bound to a VALUE of the input stream.
READ_DECL_END       Terminate the read C function definition.  Opt.
READ_CALL()         Call the lisp read function recursively.
RETURN(X)           Return a VALUE from the READ_DECL function.

MALLOC(s)           Allocate memory buffer from lisp.
REALLOC(p,s)        Reallocate a previously MALLOCed buffer from lisp.

PEEKC(stream)       Peek a C char or EOF from the stream
GETC(stream)        Read a C char or EOF from the stream

EOS                 The end-of-stream VALUE.
CONS(X,Y)           Return a new lisp CONS object.
IMMUTABLE_CONS(X)   Converts pair X to an immutable cons.
CAR(CONS)           Get the car field of a pair VALUE as in: (car CONS)
SET_CDR(CONS,V)     Set the cdr field of a pair VALUE as in: (set-cdr! CONS V)
SET(LOC,V)          Set a local variable as in (set! VARIABLE V).  Opt.  

MAKE_CHAR(I)        Create a lisp CHARACTER VALUE from a C integer.

LIST_2_VECTOR(X)    Convert list VALUE X into a VECTOR VALUE.
IMMUTABLE_VECTOR(X) Convert VECTOR VALUE X into an immutable VECTOR VALUE.

STRING(char*,int)   Create a new lisp STRING VALUE from a MALLOCed buffer.
IMMUTABLE_STRING(X) Convert STRING VALUE X to an immutable STRING VALUE.
ESCAPE_STRING(X)    Return a new STRING VALUE with escaped characters (\\, \") replaced.
STRING_2_NUMBER(X)  Convert string VALUE X into a NUMBER VALUE, or return F.
STRING_2_SYMBOL(X)  Convert string VALUE X into a SYMBOL VALUE.

SYMBOL(NAME)        Return a symbol VALUE for NAME with '_' replaced with '-'.
SYMBOL_DOT          The "." symbol.

CALL_MACRO_CHAR(X)  Call the macro character function for the C char X.  If the function returns F, continue scanning, otherwise return the CAR of the result.

EQ(X,Y)             Return non-zero C value if (eq? X Y).

NIL                 The empty list VALUE.
T                   The true VALUE. Opt.
F                   The false VALUE.
U                   The unspecified VALUE. Opt.

ERROR(format,...)   Raise an error using the printf() format.

*/

#ifdef READ_DECL

#include <ctype.h> /* isspace() */

#ifndef SET
#define SET(X,V) ((X) = (V))
#endif

#ifndef PEEKC
#define PEEKC(stream) \
  ({ int _pc = GETC(stream); if ( _pc != EOF ) UNGETC(stream, _pc); _pc; })
#endif

#ifndef READ_DEBUG_WHITESPACE
#define READ_DEBUG_WHITESPACE 0
#endif

static
int eat_whitespace_peekchar(VALUE stream)
{
  int c;

 more_whitespace:
  while ( (c = PEEKC(stream)) != EOF && isspace(c) ) {
#if READ_DEBUG_WHITESPACE
    fprintf(stderr, "  read: eat_whitespace_peekchar(): whitespace '%c'\n", (int) c);
    fflush(stderr);
#endif
    GETC(stream);
  }
  if ( c == ';' ) {
#if READ_DEBUG_WHITESPACE
    fprintf(stderr, "  read: eat_whitespace_peekchar(): comment start '%c'\n", (int) c);
    fflush(stderr);
#endif
    while ( (c = PEEKC(stream)) != EOF && c != '\n' ) {
#if READ_DEBUG_WHITESPACE
      fprintf(stderr, "  read: eat_whitespace_peekchar(): comment in '%c'\n", (int) c);
      fflush(stderr);
#endif
      GETC(stream);
    }
    goto more_whitespace;
  }

#if READ_DEBUG_WHITESPACE
  fprintf(stderr, "  read: eat_whitespace_peekchar(): done '%c'\n", (int) c);
  fflush(stderr);
#endif

  return(c);
}

#ifndef IMMUTABLE_CONS
#define IMMUTABLE_CONS(X) X
#endif

#ifndef IMMUTABLE_VECTOR
#define IMMUTABLE_VECTOR(X) X
#endif

#ifndef IMMUTABLE_STRING
#define IMMUTABLE_STRING(X) X
#endif

#ifndef RETURN
#define RETURN(X) return (X)
#endif

#ifndef ESCAPE_STRING
#define ESCAPE_STRING(X) X
#endif


READ_DECL
{
  int c;
  int radix = 10;

 try_again:
  c = eat_whitespace_peekchar(stream);
  if ( c == EOF )
    RETURN(EOS);

  GETC(stream);

  switch ( c ) {
    case '\'':
      RETURN(IMMUTABLE_CONS(CONS(SYMBOL(quote), IMMUTABLE_CONS(CONS(READ_CALL(), NIL)))));

    case '`':
      RETURN(IMMUTABLE_CONS(CONS(SYMBOL(quasiquote), IMMUTABLE_CONS(CONS(READ_CALL(), NIL)))));

    case ',':
      if ( PEEKC(stream) == '@' ) {
	GETC(stream);
	RETURN(IMMUTABLE_CONS(CONS(SYMBOL(unquote_splicing), IMMUTABLE_CONS(CONS(READ_CALL(), NIL)))));
      } else {
	RETURN(IMMUTABLE_CONS(CONS(SYMBOL(unquote), IMMUTABLE_CONS(CONS(READ_CALL(), NIL)))));
      }
      break;

    case '(': {
      VALUE l, lc;

      l = lc = NIL;
      while ( (c = eat_whitespace_peekchar(stream)) != EOF ) {
        VALUE x;
        
        if ( c == ')' ) {
	  GETC(stream);
          break;
        }
        
        SET(x, READ_CALL());
        
        if ( EQ(x, SYMBOL_DOT) ) {
          if ( EQ(lc, NIL) ) {
            RETURN(ERROR("expected something before '.' in list"));
          }

          SET_CDR(lc, READ_CALL());
	  (void) IMMUTABLE_CONS(lc);

          c = eat_whitespace_peekchar(stream);
          GETC(stream);
          if ( c != ')' ) {
            RETURN(ERROR("expected ')': found '%c'", c));
          }
          break;
        } else {
          VALUE y = CONS(x, NIL);
          if ( EQ(lc, NIL) ) {
            SET(l, y);
          } else {
            SET_CDR(lc, y);
	    (void) IMMUTABLE_CONS(lc);
          }
          SET(lc, y);
	  (void) IMMUTABLE_CONS(lc);
        }
      }
      (void) IMMUTABLE_CONS(l);
      RETURN(l);
      }

    case '#':
  hash_again:
      c = PEEKC(stream);
      switch ( c ) {
      case EOF:
	RETURN(ERROR("eos after '#'"));

	/* #! sh-bang comment till eof */
      case '!':
#if READ_DEBUG_WHITESPACE
	fprintf(stderr, "  read: #!\n");
	fflush(stderr);
#endif
	GETC(stream);
	while ( (c = PEEKC(stream)) != EOF && c != '\n' ) {
	  GETC(stream);
	}
    	goto try_again;

	/* s-expr comment ala chez scheme */
      case ';':
#if READ_DEBUG_WHITESPACE
	fprintf(stderr, "  read: #;\n");
	fflush(stderr);
#endif
	GETC(stream);
	GETC(stream);
	READ_CALL();
	goto try_again;

      case '(':
	RETURN(IMMUTABLE_VECTOR(LIST_2_VECTOR(READ_CALL())));
        
      case '\\':
	GETC(stream);
	c = GETC(stream);

	{
	  const char *charname = 0;

	  /* Handle #\space and #\newline */
	  if ( tolower(c) == 's' ) {
	    const char *s;

	    charname = " space";
	  match_charname:
	    
	    s = charname + 2;

	    do {
	      int c2;

	      if ( ! *s ) {
		/* fprintf(stderr, "READ: \#%s => \#%c\n", charname, c); */ 
		c = charname[0];
		break;
	      }
	      c2 = PEEKC(stream);
	      if ( c2 == EOF )
		break;
	      if ( tolower(c2) != *(s ++) ) {
		break;
	      }
	      GETC(stream);
	    } while ( 1 );

	  } else if ( tolower(c) == 'n' ) {
	    charname = "\nnewline";
	    goto match_charname;
	  }
	}

	if ( c == EOF ) {
	  RETURN(ERROR("eos after '#\\'"));
	}
	RETURN(MAKE_CHAR(c));

      case 'f': case 'F':
	GETC(stream);
	RETURN(F);

#ifdef T
      case 't': case 'T':
	GETC(stream);
	RETURN(T);
#endif
        
#ifdef U
      case 'u': case 'U':
	GETC(stream);
	RETURN(U);
#endif

      case 'e': case 'E':
      case 'i': case 'I':
        GETC(stream);
	goto hash_again;

      case 'b': case 'B':
	GETC(stream);
	radix = 2;
	goto read_number;
	
      case 'o': case 'O':
	GETC(stream);
	radix = 8;
	goto read_number;

      case 'd': case 'D':
	GETC(stream);
	radix = 10;
	goto read_number;
	
      case 'x': case 'X':
	GETC(stream);
	radix = 16;
	goto read_number;

      default:
#ifdef CALL_MACRO_CHAR
	{
	  VALUE x;

	  GETC(stream);
	  SET(x, CALL_MACRO_CHAR(c));
	  if ( EQ(x,F) ) {
	    goto try_again;
	  } else {
	    RETURN(CAR(x));
	  }
	}
#endif
	RETURN(ERROR("bad sequence: #%c", c));
      }
      break;

    case '"': {
      char *buf;
      size_t len;

      len = 0;
      buf = MALLOC(len + 1);
      while ( (c = GETC(stream)) != '"' ) {
        if ( c == EOF ) {
          RETURN(ERROR("EOS in string"));
        }
        buf = REALLOC(buf, len + 2);
        buf[len ++] = c;
        
        if ( c == '\\' ) {
          if ( (c = GETC(stream)) == EOF ) {
            RETURN(ERROR("EOS in string"));
          }
          buf = REALLOC(buf, len + 2);
          buf[len ++] = c;
        }
      }

      buf[len] = '\0';
      RETURN(IMMUTABLE_STRING(ESCAPE_STRING(STRING(buf, len))));
    }

    read_number:
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':

    case '~': case '!': case '@': case '$': case '%':
    case '&': case '*': case '_': case '+': case '-':
    case '=': case ':': case '<': case '>': case '^':
    case '.': case '?': case '/': case '|':

    case 'a': case 'b': case 'c': case 'd': case 'e':
    case 'f': case 'g': case 'h': case 'i': case 'j':
    case 'k': case 'l': case 'm': case 'n': case 'o':
    case 'p': case 'q': case 'r': case 's': case 't':
    case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':

    case 'A': case 'B': case 'C': case 'D': case 'E':
    case 'F': case 'G': case 'H': case 'I': case 'J':
    case 'K': case 'L': case 'M': case 'N': case 'O':
    case 'P': case 'Q': case 'R': case 'S': case 'T':
    case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
    {
      char *buf;
      size_t len;
      VALUE s, n;

      len = 1;
      buf = MALLOC(len + 1);
      buf[0] = c;

      while ( (c = PEEKC(stream)) != EOF && c != ';' && c != '(' && c != ')' && c != '#' && ! isspace(c) ) {
        GETC(stream);
        buf = REALLOC(buf, len + 2);
        buf[len ++] = c;
      }
      buf[len] = '\0';

      s = STRING(buf, len);
      n = STRING_2_NUMBER(s, radix);
      if ( EQ(n, F) ) {
	n = STRING_2_SYMBOL(s);
#ifdef NIL_SYMBOL
        if ( EQ(n, NIL_SYMBOL) ) {
	  n = NIL;
	}
#endif
      }
      RETURN(n);
    }
      break;

    default:
      RETURN(ERROR("unexpected character '%c'", c));
  }
}

#ifdef READ_DECL_END
READ_DECL_END
#endif

#endif
