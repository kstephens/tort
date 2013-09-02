#include <assert.h>
#include "allocator.hh"
#if _ASSEMBLY
#include "allocator.cc"
#endif

using namespace hemispace;

class Cons {
public:
  ref_ car, cdr;
  static ref_* scan(void *p)
  {
    Cons *o = (Cons*) p;
    o->car.scan();
    return &o->cdr;
  }
  static Class cls;
  void * operator new(size_t size)
  {
    return Allocator::instance.alloc(&Cons::cls);
  }
};

Class Cons::cls(sizeof(Cons), Cons::scan);

int main(int argc, char **argv)
{
  Allocator &a = Allocator::instance;
  a.roots_ = stackref_::scan_all;
  a.initial_size_ = 4096;
  a.init();

  stackref<Cons> x;
  for ( int i = 0; i < 4096; ++ i ) {
    x = new Cons();
    x->car = 0;
    x->cdr = x;
  }
  assert(x.ptr());
  assert(x->cdr.ptr() == x.ptr());

  fprintf(stderr, "\nOK\n");
  return 0;
}


