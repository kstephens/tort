GC=gc-7.0

CFLAGS_OPTIMIZE = -O2
CFLAGS = -Iinclude -I$(GC)/include -Wall -Werror -g $(CFLAGS_OPTIMIZE)
LDFLAGS = -L$(GC)/.libs
LIBS=-lgc

all : gc tort_test

CFILES = $(shell ls src/*.c)
OFILES = $(CFILES:.c=.o)

libtort.a : $(OFILES)
	$(AR) $(ARFLAGS) $@ $(OFILES)
	ranlib $@ || true

tort_test : tort_test.c libtort.a
	$(CC) $(CFLAGS) $(LDFLAGS) $^ $(LIBS) -o $@

tort_test $(OFILES) : include/tort/*.h

gc : $(GC)/.libs/libgc.a

$(GC)/.libs/libgc.a : $(GC).tar.gz
	if [ ! -d $(GC) ]; then tar -zxvf $^; fi
	cd $(GC) && if [ ! -f Makefile ]; then ./configure; fi
	cd $(GC) && make

run-test : tort_test
	./tort_test 

test : ./tort_test
	./tort_test 2>&1 </dev/null | t/filter-output > t/tort_test.out
	diff -U 10 t/tort_test.exp t/tort_test.out

accept-test : tort_test
	./tort_test 2>&1 </dev/null | t/filter-output > t/tort_test.exp

gdb : tort_test
	gdb --args ./tort_test 

disasm : tort_test
	objdump -DS ./tort_test | less "+/<main>"

clean :
	rm -f tort_test libtort.a src/*.o

very-clean : clean
	cd $(GC) && make clean

