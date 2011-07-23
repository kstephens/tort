CFLAGS+= -O3 #
CFLAGS+= -g #

all : test-1.t

test-1.t: test-1.cc allocator.cc allocator.hh Makefile
	g++ $(CFLAGS) test-1.cc allocator.cc -o $@

test-1.s: test-1.cc allocator.cc allocator.hh Makefile
	g++ $(CFLAGS:-g=) -D_ASSEMBLY -S test-1.cc -o $@

allocator.s: allocator.cc allocator.hh Makefile
	g++ $(CFLAGS) -S allocator.cc -o $@

test : all
	set -ex; for t in ./*.t ;\
	do \
	  time $$t ;\
	done
