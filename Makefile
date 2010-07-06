GC=gc-7.0

CFLAGS_OPTIMIZE = -O2
CFLAGS = -Iinclude -I$(GC)/include -Wall -Werror -g $(CFLAGS_OPTIMIZE)
LDFLAGS = -L$(GC)/.libs
LIBS=-lgc

######################################################################

LIB_CFILES = $(shell ls src/*.c)
LIB_OFILES = $(LIB_CFILES:.c=.o)

TEST_CFILES = $(shell ls t/*.c)
TEST_PFILES = $(TEST_CFILES:.c=)

######################################################################

all : gc test

src/libtort.a : $(LIB_OFILES)
	$(AR) $(ARFLAGS) $@ $(LIB_OFILES)
	ranlib $@ || true

$(TEST_PFILES) : src/libtort.a
	$(CC) $(CFLAGS) $(LDFLAGS) $(@:=.c) src/libtort.a $(LIBS) -o $@

$(TEST_PFILES) $(LIB_OFILES) : include/tort/*.h

gc : $(GC)/.libs/libgc.a

$(GC)/.libs/libgc.a : $(GC).tar.gz
	if [ ! -d $(GC) ]; then tar -zxvf $^; fi
	cd $(GC) && if [ ! -f Makefile ]; then ./configure; fi
	cd $(GC) && make

run-test : $(TEST_PFILES)
	for f in $(TEST_PFILES); do \
	  $$f ;\
	done

test : $(TEST_PFILES)
	@for f in $(TEST_PFILES); do \
	   echo -n "  Testing $$f: " ;\
           $$f 2>&1 </dev/null | t/filter-output > $$f.out ;\
	   diff -U 10 $$f.exp $$f.out ;\
	   echo "ok" ;\
	done

accept-test : $(TEST_PFILES)
	for f in $(TEST_PFILES); do \
	  cp $$f.out $$f.exp ;\
	done

gdb : t/tort_test
	gdb --args t/tort_test 

disasm : t/tort_test
	objdump -DS t/tort_test | less "+/<main>"

clean :
	rm -f $(TEST_PFILES) src/libtort.a src/*.o t/*.out

very-clean : clean
	cd $(GC) && make clean

