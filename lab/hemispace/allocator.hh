#ifndef ALLOCATOR_HH
#define ALLOCATOR_HH

#include <string.h>
#include <stdio.h>
#include <iostream>
#include <sys/mman.h> /* mmap(), munmap() */

namespace hemispace {
  class ref_;

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
    ref_() : p_(0) { }
    ref_(void *p) : p_(p) { }
    ref_(const ref_ &r) : p_(r.p_) { }
    
    ref_& operator =(void *p) { p_ = p; return *this; }
    ref_& operator =(const ref_ &r) { p_ = r.p_; return *this; }

    void *ptr() const { return p_; }
    void ptr_(void *p) { p_ = p; }

    void *forward_to() const { return ((void**) p_)[-1]; }
    void forward_to_(void *p) { ((void**) p_)[-1] = p; }
    
    Class *cls() const { return ((Class**) p_)[-1]; }
    void cls_(Class *cls) { ((Class**) p_)[-1] = cls; }

    void scan();
  };

  class Space {
  public:
    void *alloc_, *base_, *end_;
    size_t size_, object_n_;

    Space(size_t size)
    {
      map(size);
    }
    ~Space()
    {
      unmap();
    }

    size_t align_size(size_t size)
    {
#ifndef PAGESIZE
#define PAGESIZE 4096
#endif
      return (size + PAGESIZE - 1) & ~(PAGESIZE - 1);
    }

    size_t allocated_size() const {
      return (char*) alloc_ - (char*) base_;
    }

    void map(size_t size)
    {
      size_ = align_size(size);
      alloc_ = base_ = mmap((void*) 0, 
			    size_, 
			    PROT_READ | PROT_WRITE,
			    MAP_ANON | MAP_PRIVATE,
			    -1, (off_t) 0);
      end_ = (char*) base_ + size_;
      object_n_ = 0;
    }

    void unmap()
    {
      if ( base_)
	munmap(base_, size_);
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
	munmap((char*) base_ + new_size, size_ - new_size);
	size_ = new_size;
	end_ = (char*) base_ + size_;
      }
    }

    void remap(size_t size)
    {
      size_t new_size = align_size(size);
      if ( base_ ) {
	if ( new_size < size_ ) {
	  munmap((char*) base_ + new_size, size_ - new_size);
	} else {	  
	  munmap(base_, size_);
	  base_ = mmap((void*) 0, 
		       new_size, 
		       PROT_READ | PROT_WRITE,
		       MAP_ANON | MAP_PRIVATE,
		       -1, (off_t) 0);
 	}
      } else {
	base_ = mmap((void*) 0, 
		     new_size, 
		     PROT_READ | PROT_WRITE,
		     MAP_ANON | MAP_PRIVATE,
		     -1, (off_t) 0);
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

    int containsQ(void *ptr) { return base_ <= ptr && ptr < alloc_; }

    void *alloc(Class *cls) {
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
    size_t initial_size_;
    Space *from_, *to_;
    void (*roots_)();

    Allocator() : from_(0), to_(0) { }

    void collect(size_t needed_size)
    {
      // Calculate new "to" space size.
      size_t new_size = from_->size_;
      new_size += from_->size_ * 0.10 + needed_size;

      fprintf(stderr, "\n  collect() : BEGIN\n");
      fprintf(stderr, "  collect() : before: object_n_ = %ld\n", (unsigned long) from_->object_n_);
      fprintf(stderr, "  collect() : before: new_size = %ld\n", (unsigned long) new_size);
      fprintf(stderr, "  collect() : before: from [ @%p, @%p )\n",
	      from_->base_, from_->alloc_);

      // Resize "to" Space.
      to_->remap(new_size);

      // Scan roots.
      roots_();

      // If "to" Space is much smaller, shrink it now.
      if ( to_->allocated_size() < from_->size_ / 2 ) {
	to_->shrink(from_->size_ / 2);
      }

      // Free old "from" Space.
      from_->unmap();

      // Flip "from" and "to".
      {
	Space *t = to_;
	to_ = from_;
	from_ = t;
      }

      fprintf(stderr, "  collect() : after: object_n_ = %ld\n", (unsigned long) from_->object_n_);
      fprintf(stderr, "  collect() : DONE\n\n");
    }

    void init()
    {
      from_ = new Space(initial_size_);
      to_   = new Space(initial_size_);
    }

    void *alloc(Class *cls)
    {
      void *ptr = from_->alloc(cls);
      if ( ! ptr ) {
	collect(cls->size_);
	ptr = from_->alloc(cls);
      }
      fprintf(stderr, ".");
      return ptr;
    }

    void *forwarding_to(ref_ *r)
    {
      void *ptr = r->forward_to();
      return to_->containsQ(ptr) ? ptr : 0;
    }

    void scan(ref_ *r)
    {
    again:
      fprintf(stderr, "    ref @%p => %p\n", r, r->ptr());
      /* If ref points into "from" Space, */
      if ( from_->containsQ(r->ptr()) ) {
	void *to_ptr;

	/* If there is a forwarding pointer in "from" Space to "to" Space, */
	if ( to_ptr = forwarding_to(r) ) {
	  fprintf(stderr, "      from %p forward to %p\n", r->ptr(), to_ptr);
	  /* Redirect ref to "to" Space. */
	  r->ptr_(to_ptr);
	} else {
	  Class *cls = r->cls();
	  /* Allocate a object copy in the "to" Space. */
	  to_ptr = to_->alloc(cls);
	  memcpy(to_ptr, r->ptr(), cls->size_);
	  
	  fprintf(stderr, "      from %p copy to %p\n", r->ptr(), to_ptr);

	  /* Leave a forwarding pointer in the "from" Space to the "to" Space. */
	  r->forward_to_((Class*) to_ptr);
	  
	  /* Point ref into "to" Space. */
	  r->ptr_(to_ptr);

	  /* Recursively scan object in "to" Space. */
	  r = cls->scan_(to_ptr);
	  if ( r ) {
	    fprintf(stderr, "   again:\n");
	    goto again;
	  }
	}
      }
    }
    static Allocator instance;
  };

  void ref_::scan() {
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
    stackref_() : ref_(0) { init(); }
    stackref_(void *p) : ref_(p) { init(); }
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
