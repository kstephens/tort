#include "tort/core.h"
#include <setjmp.h>

typedef struct tort_catch { tort_H;
  tort_apply_decl((*applyf));
  tort_v name;
  tort_v data;
  jmp_buf *jbp;
  tort_v applied;
  tort_v result;
  tort_pair *unwinds;
  struct tort_catch *previous_catch;
} tort_catch;
tort_h_struct(tort_catch);

#if 0
TORT_GETTER(catch,voidP,jbp);
TORT_ACCESSOR(catch,tort_v,applied);
TORT_ACCESSOR(catch,tort_v,result);
TORT_ACCESSOR(catch,tort_v,previous_catch);
#endif

static
int invalidate_catches_til(tort_catch *catch)
{
  if ( catch->applied != tort_false || ! catch->jbp ) {
    tort_error(tort_ta "catch %T already applied", catch);
    return 0;
  }
  catch->applied = tort_true;
  /* Invalidate between top_catch and the target catch. */
  while ( tort_(top_catch) != tort_nil ) { /* NOT THREAD-SAFE */
    tort_catch *c = tort_(top_catch);      /* NOT THREAD-SAFE */
    tort_(top_catch) = c->previous_catch;  /* NOT THREAD-SAFE */

    // Call unwinds.
    while ( c->unwinds != tort_nil ) {
      tort_v block = c->unwinds->first;
      c->unwinds = c->unwinds->second;
      tort_sendn(tort_s(value), 1, block);
    }

    // Stop now.
    if ( c == catch )
      break;

    // Do not allow longjmp to c.
    c->jbp = 0;
  }
  return 1;
}

tort_v _tort_m_catch__throw(tort_tp tort_catch *catch, tort_v value)
{
  catch->result = value;
  if ( invalidate_catches_til(catch) )
    longjmp(*catch->jbp, 1);
  return catch->result; /* NOTREACHED */
}

/* method interface */
static tort_v _catch___applyf(tort_tp tort_v value)
{
  tort_catch *catch = (void*) _tort_message->method;
  return _tort_m_catch__throw(tort_ta catch, value);
}

/* block interface. */
tort_v _tort_m_catch__value(tort_tp tort_catch *catch, tort_v value)
{
  return _tort_m_catch__throw(tort_ta catch, value);
}

tort_v _tort_m_catch__retry(tort_tp tort_catch *catch, tort_v value)
{
  catch->result = value;
  if ( invalidate_catches_til(catch) )
    longjmp(*catch->jbp, 2);
  return catch->result; /* NOTREACHED */
}

tort_v _tort_M_catch__top_catch(tort_tp tort_mtable *mtable)
{
  return tort_(top_catch);
}

tort_v _tort_M_catch__new(tort_tp tort_mtable *mtable)
{
  tort_catch *catch = tort_allocate(mtable, sizeof(*catch));
  catch->applyf = (void*) _catch___applyf;
  catch->name = tort_nil;
  catch->data = tort_nil;
  catch->jbp = 0;
  catch->applied = tort_false;
  catch->result = tort_nil;
  catch->unwinds = tort_nil;
  return catch;
}

tort_v _tort_m_catch__unwind_protect(tort_tp tort_catch *catch, tort_v block)
{
  catch->unwinds = tort_send(tort_s(new), tort__mt(pair), block, catch->unwinds);
  return catch;
}

tort_v _tort_M_catch__unwind_protect(tort_tp tort_mtable *mtable, tort_v block)
{
  if ( tort_(top_catch) == tort_nil ) {
    tort_(top_catch) = tort_send(tort_s(new), mtable);
  }
  tort_send(tort_s(unwind_protect), tort_(top_catch), block);
  return mtable;
}

tort_v _tort_m_catch__begin(tort_tp tort_catch *catch, tort_v block)
{
  tort_v result = tort_nil;
  int sj;
  jmp_buf jb;
 retry:
  switch ( (sj = setjmp(jb)) ) {
  case 0:
    catch->jbp = &jb;
    catch->applied = tort_false;
    catch->result = tort_nil;
    catch->previous_catch = tort_(top_catch); /* NOT THREAD-SAFE */
    tort_(top_catch) = catch; /* NOT THREAD-SAFE */
    result = tort_sendn(tort__s(value), 2, block, catch);
    break;
  case 1:
    catch->jbp = 0;
    result = catch->result;
    break;
  case 2:
    catch->jbp = 0;
    result = catch->result;
    goto retry;
  default:
    tort_fatal(tort_ta "invalid catch longjmp(%p, %d)", (void*) &jb, sj);
  }
  return result;
}

tort_v tort_runtime_initialize_catch()
{
  tort_mtable_make("catch", 0);
  return tort_true;
}

