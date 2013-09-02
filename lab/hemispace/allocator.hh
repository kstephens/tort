#ifndef ALLOCATOR_HH
#define ALLOCATOR_HH

#include <string.h>
#include <stdio.h>
#include <iostream>
#include <assert.h>
#include <sys/mman.h> /* mmap(), munmap() */

namespace hemispace {
  class ref_;

  class Config {
  public:
    static const size_t word_size = sizeof(void*);
    static const int tag_mask = word_size - 1;
    static size_t tag_bits(void *ptr) { return (size_t) ptr & tag_mask; }
    static size_t align_to_word(size_t size) { return (size + tag_mask) & ~ (size_t) tag_mask; }
    static size_t align_to_page(size_t size)
    {
#ifndef PAGESIZE
#define PAGESIZE 4096UL
#endif
      return (size + PAGESIZE - 1) & ~(PAGESIZE - 1);
    }
  };

  class Class {
  public:
    size_t size_;
    ref_* (*scan_)(void *ptr);
    int flags_;
    Class(size_t size, ref_* (*scan)(void*), int flags = 0) :
      size_(size), scan_(scan), flags_(flags) { }
  };
  
  class ref_ {
  protected:
    void *p_;
  public:
    ref_()              : p_(0) { }
    ref_(void *p)       : p_(p) { }
    ref_(const ref_ &r) : p_(r.p_) { }
    
    ref_& operator =(void *p)       { p_ = p; return *this; }
    ref_& operator =(const ref_ &r) { p_ = r.p_; return *this; }

    void *ptr() const  { return p_; }
    void ptr_(void *p) { p_ = p; }

    void *forward_to() const  { return ((void**) p_)[-1]; }
    void forward_to_(void *p) { ((void**) p_)[-1] = p; }
    
    Class *cls() const    { return ((Class**) p_)[-1]; }
    void cls_(Class *cls) { ((Class**) p_)[-1] = cls; }

    void scan();
  };

  class Space {
  public:
    void *alloc_, *base_, *end_;
    size_t size_, object_n_;

    Space()  { }
    ~Space() { unmap(); }

    void flip(Space &other) 
    {
#define SWAP(T,X) { T tmp = X; X = other.X; other.X = tmp; }
      SWAP(void*, alloc_);
      SWAP(void*, base_);
      SWAP(void*, end_);
      SWAP(size_t, size_);
      SWAP(size_t, object_n_);
#undef SWAP
    }

    size_t allocated_size() const {
      return (char*) alloc_ - (char*) base_;
    }

    size_t align_size(size_t size) const { return Config::align_to_page(size); }

    void *sys_mmap(void *base, size_t size) const
    {
      void *addr = mmap(base, size, PROT_READ | PROT_WRITE,
			MAP_ANON | MAP_PRIVATE, -1, (off_t) 0);
      fprintf(stderr, "  @%p: mmap(@%p, %llu) => @%p\n", this, base, (unsigned long long) size, addr);
      return addr;
    }
    void sys_munmap(void *base, size_t size) const
    {
      fprintf(stderr, "  @%p: munmap(@%p, %llu)\n", this, base, (unsigned long long) size);
      assert(munmap(base, size) == 0);
    }

    void map(size_t size)
    {
      size_ = align_size(size);
      alloc_ = base_ = sys_mmap(0, size_);
	end_ = (char*) base_ + size_;
      object_n_ = 0;
    }

    void unmap()
    {
      if ( base_)
	sys_munmap(base_, size_);
      alloc_ = base_ = end_ = 0;
      size_ = object_n_ = 0;
    }

    void shrink(size_t size)
    {
      size_t new_size = align_size(size);
      assert((char*) base_ + new_size <= end_);
      if ( new_size < size_ ) {
	fprintf(stderr, "  shrinking [ %p, %p ) to [ %p, %p ) [%lu]\n",
		base_, end_, base_, (char*) base_ + new_size,
		(unsigned long) new_size);
	sys_munmap((char*) base_ + new_size, size_ - new_size);
	size_ = new_size;
	end_ = (char*) base_ + size_;
      }
    }

    void remap(size_t size)
    {
      size_t new_size = align_size(size);
      if ( base_ ) {
	if ( new_size < size_ ) {
	  sys_munmap((char*) base_ + new_size, size_ - new_size);
	} else {	  
	  sys_munmap(base_, size_);
	  base_ = sys_mmap(0, new_size);
 	}
      } else {
	base_ = sys_mmap(0, new_size);
      }

      fprintf(stderr, "  remap [ %p, %p ) [%lu]\n",
	      base_, (char*) base_ + new_size,
	      (unsigned long) new_size);

      size_ = new_size;
      alloc_ = base_;
      if ( ! base_ )
	size_ = 0;
      end_ = (char*) base_ + size_;
      object_n_ = 0;
    }

    int containsQ(void *ptr) const { return base_ <= ptr && ptr < alloc_; }

    void *alloc(Class *cls) 
    {
      size_t actual_size = cls->size_ + sizeof(cls);
      void *new_alloc = (char*) alloc_ + actual_size;
      void *result;

      if ( new_alloc <= end_ ) {
	++ object_n_;
	result = alloc_;
	* (Class**) result = cls;
	result = (char*) result + sizeof(cls);
	alloc_ = new_alloc;
	return result;
      } else {
	return 0;
      }
    }
  };
    
  class Allocator {
  public:
    Space from_, to_;
    size_t initial_size_;
    void (*roots_)();

    Allocator() { }

    void collect(size_t needed_size)
    {
      // Calculate new "to" space size.
      size_t new_size = from_.size_;
      new_size += from_.size_ * 0.10 + needed_size;

      fprintf(stderr, "\n  collect() : BEGIN\n");
      fprintf(stderr, "  collect() : before: object_n_ = %lu\n", (unsigned long) from_.object_n_);
      fprintf(stderr, "  collect() : before: new_size = %lu\n", (unsigned long) new_size);
      fprintf(stderr, "  collect() : before: from [ @%p, @%p )\n",
	      from_.base_, from_.alloc_);

      // Resize "to" Space.
      to_.remap(new_size);

      // Scan roots.
      roots_();

      // If "to" Space is much smaller, shrink it now.
      if ( to_.allocated_size() < from_.size_ / 2 ) {
	to_.shrink(from_.size_ / 2);
      }

      // Free old "from" Space.
      from_.unmap();

      // Flip "from" and "to".
      to_.flip(from_);

      fprintf(stderr, "  collect() : after: object_n_ = %lu\n", (unsigned long) from_.object_n_);
      fprintf(stderr, "  collect() : DONE\n\n");
    }

    void init()
    {
      from_.map(initial_size_);
      to_.map(initial_size_);
    }

    void *alloc(Class *cls)
    {
      void *ptr = from_.alloc(cls);
      if ( ! ptr ) {
	collect(cls->size_);
	ptr = from_.alloc(cls);
      }
      fprintf(stderr, ".");
      return ptr;
    }

    void *forwarding_to(ref_ *r)
    {
      void *ptr = r->forward_to();
      return to_.containsQ(ptr) ? ptr : 0;
    }

    void scan(ref_ *r)
    {
    again:
      fprintf(stderr, "    ref @%p => %p\n", r, r->ptr());
      /* If ref points into "from" Space, */
      if ( from_.containsQ(r->ptr()) ) {
	void *to_ptr;

	/* If there is a forwarding pointer in "from" Space to "to" Space, */
	if ( to_ptr = forwarding_to(r) ) {
	  fprintf(stderr, "      from %p forward to %p\n", r->ptr(), to_ptr);
	  /* Redirect ref to "to" Space. */
	  r->ptr_(to_ptr);
	} else {
	  Class *cls = r->cls();
	  /* Allocate an object copy in the "to" Space. */
	  to_ptr = to_.alloc(cls);
	  memcpy(to_ptr, r->ptr(), cls->size_);
	  
	  fprintf(stderr, "      from %p copy to %p\n", r->ptr(), to_ptr);

	  /* Leave a forwarding pointer in the "from" Space to the "to" Space. */
	  r->forward_to_(to_ptr);
	  
	  /* Point ref into "to" Space. */
	  r->ptr_(to_ptr);

	  /* Recursively scan object in "to" Space. */
	  r = cls->scan_(to_ptr);

	  /* Tail-recurse on ref returned from scan_. */
	  if ( r ) {
	    fprintf(stderr, "   again:\n");
	    goto again;
	  }
	}
      }
    }
    static Allocator instance;
  };

  inline void ref_::scan() {
    Allocator::instance.scan(this);
  }

  class stackref_ : public ref_ {
  private:
    stackref_ *prev_;

  public:
    static stackref_ *top_; // = 0;
    static void scan_all()
    {
      stackref_* sr = stackref_::top_;
      while ( sr ) {
	fprintf(stderr, "    stackref_ @%p:\n", sr);
	sr->scan();
	sr = sr->prev_;
      }
    }

    void init() { prev_ = stackref_::top_; stackref_::top_ = this; }

    stackref_()                   : ref_(0) { init(); }
    stackref_(void *p)            : ref_(p) { init(); }
    stackref_(const stackref_ &r) : ref_(r) { init(); }

    ~stackref_ () 
    {
      assert(stackref_::top_ == this);
      top_ = prev_;
    }
  };

  template <class T> 
  class ref : public ref_ {
  public:
    ref() : ref_() { }
    ref(T* p) : ref_((void*) p) { }
    ref(const ref &r) : ref_(r.p_) { }
    
    ref & operator = (T* p) { ptr_(p); return *this; }

    T* operator -> () const {
      return (T*) ptr();
    }
  };

  template <class T>
  class stackref : public stackref_ {
  public:
    stackref() : stackref_(0) { }
    stackref(T *p) : stackref_(p) { }
    stackref(const stackref &r) : stackref_(r) { }

    stackref & operator = (T* p) { ptr_(p); return *this; }

    T* operator -> () const {
      return (T*) ptr();
    }
  };

}

#endif
