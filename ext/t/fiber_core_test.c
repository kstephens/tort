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
  int n = 10;

  assert(_tort_fiber_ptr->status == RUNNING);
  fprintf(stderr, "fiber_a = @%p\n", fiber_a);
  assert(fiber_a == _tort_fiber_ptr);

  while ( n -- ) {
    // fprintf(stderr, "a: fiber = %p\n", _tort_fiber);
    assert(_tort_fiber_ptr == fiber_a);
    assert(fiber_a->status == RUNNING);
    fprintf(stderr, "a(%s) @%p @%p: n = %d\n", (char*) data, _tort_fiber_ptr, &_tort_fiber_ptr, n);
    if ( ! fiber_b ) {
      fiber_b = malloc(sizeof(*fiber_b));
      memset(fiber_b, 0, sizeof(*fiber_b));
      __tort_fiber_init(fiber_b, (size_t) 0);
      fiber_b->name = "b";
      __tort_fiber_start(fiber_b, b, "from a()");
      __tort_fiber_yield(fiber_b);
      assert(fiber_b->status == PAUSED);
    } else {
      if ( fiber_b->status < EXITED ) {
        assert(fiber_b->status == PAUSED);
        __tort_fiber_yield(fiber_b);
        assert(fiber_b->status == PAUSED || fiber_b->status == EXITED);
      }
    }
  }
  fprintf(stderr, "a return\n");
  return "a return";
}

static
tort_fiber_func_DECL(b)
{
  int n = 5;

  fprintf(stderr, "fiber_b = @%p\n", fiber_b);
  assert(fiber_b == _tort_fiber_ptr);

  while ( n -- ) {
    // fprintf(stderr, "b: fiber = @%p\n", _tort_fiber_ptr);
    assert(_tort_fiber_ptr == fiber_b);
    assert(fiber_b->status == RUNNING);
    fprintf(stderr, "b(%s) @%p @%p: n = %d\n", (char*) data, _tort_fiber_ptr, &_tort_fiber_ptr, n);
    assert(fiber_a->status == PAUSED);
    __tort_fiber_yield(0);
  }
  fprintf(stderr, "b return\n");
  return "b return";
}

int main(int argc, char **argv)
{
  tort_fiber_t *fiber_main = __tort_fiber_main();

  fprintf(stderr, "main() @%p\n", &argv);

  fiber_a = malloc(sizeof(*fiber_a));
  memset(fiber_a, 0, sizeof(*fiber_a));
  assert(fiber_a->status == CREATED);
  __tort_fiber_init(fiber_a, (size_t) 0);
  fiber_a->name = "a";
  assert(fiber_a->status == INITIALIZED);

  __tort_fiber_start(fiber_a, a, "from main()");

  __tort_fiber_yield(fiber_a);

  fprintf(stderr, "back in main()\n");
  assert(fiber_a->status != RUNNING);
  assert(fiber_b->status != RUNNING);
  assert(fiber_main->status == RUNNING);

  return 0;
}

