#include "tort/fiber.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

static tort_fiber_t *fiber_a, *fiber_b;

static tort_fiber_func_DECL(a);
static tort_fiber_func_DECL(b);

static
tort_fiber_func_DECL(a)
{
  static int n = 10;
  fiber_a = _tort_fiber_ptr;
  fprintf(stderr, "fiber_a = @%p\n", fiber_a);
  while ( n -- ) {
    // fprintf(stderr, "a: fiber = %p\n", _tort_fiber);
    assert(_tort_fiber_ptr == fiber_a);
    fprintf(stderr, "a(%s) @%p @%p: n = %d\n", (char*) data, _tort_fiber_ptr, &_tort_fiber_ptr, n);
    if ( ! fiber_b ) {
      __tort_fiber_new(fiber_a, b, "from a()", (size_t) 0);
    } else {
      __tort_fiber_yield(fiber_a, fiber_b);
    }
  }
  return "a return";
}

static
tort_fiber_func_DECL(b)
{
  static int n = 10;
  fiber_b = _tort_fiber_ptr;
  fprintf(stderr, "fiber_b = @%p\n", fiber_b);
  while ( n -- ) {
    // fprintf(stderr, "b: fiber = @%p\n", _tort_fiber_ptr);
    assert(_tort_fiber_ptr == fiber_b);
    fprintf(stderr, "b(%s) @ @%p @%p: n = %d\n", (char*) data, _tort_fiber_ptr, &_tort_fiber_ptr, n);
    __tort_fiber_yield(fiber_b, fiber_a);
  }
  return "b return";
}


int main(int argc, char **argv)
{
  void *result;

  fprintf(stderr, "main() @%p\n", &argv);
  result = __tort_fiber_new(0, a, "from main()", (size_t) 0);
  fprintf(stderr, "  => %s\n", (char*) result);

  return 0;
}
