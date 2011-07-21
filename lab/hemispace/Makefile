all : test-1.t

test-1.t: test-1.cc allocator.cc allocator.hh Makefile
	g++ -g test-1.cc allocator.cc -o $@

test : all
	set -ex; for t in ./*.t ;\
	do \
	  time $$t ;\
	done
