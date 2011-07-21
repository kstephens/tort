#include "allocator.hh"

namespace hemispace {
  Allocator Allocator::instance;
  stackref_* stackref_::top_ = 0;
}
