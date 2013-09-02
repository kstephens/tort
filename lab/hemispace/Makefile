#CFLAGS+= -O3 # segfaults
#CFLAGS+= -O2 # segfaults
CFLAGS+= -g #
CXX=clang++

all : test-1.t

test-1.t: test-1.cc allocator.cc allocator.hh Makefile
	$(CXX) $(CFLAGS) test-1.cc allocator.cc -o $@

test-1.s: test-1.cc allocator.cc allocator.hh Makefile
	$(CXX) $(CFLAGS:-g=) -D_ASSEMBLY -S test-1.cc -o $@

allocator.s: allocator.cc allocator.hh Makefile
	$(CXX) $(CFLAGS) -S allocator.cc -o $@

test : all
	set -ex; for t in ./*.t ;\
	do \
	  time $$t ;\
	done

debug : test-1.t
	gdb --args test-1.t
