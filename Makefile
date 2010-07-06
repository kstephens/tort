GC=gc-7.0

CFLAGS = -Iinclude -I$(GC)/include -Wall -Werror -g # -O2
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

$(OFILES) : include/tort/*.h

gc : $(GC)/.libs/libgc.a

$(GC)/.libs/libgc.a : $(GC).tar.gz
	if [ ! -d $(GC) ]; then tar -zxvf $^; fi
	cd $(GC) && if [ ! -f Makefile ]; then ./configure; fi
	cd $(GC) && make

test : tort_test
	./tort_test 

gdb : tort_test
	gdb --args ./tort_test 

clean :
	rm -f tort_test libtort.a src/*.o

very-clean : clean
	cd $(GC) && make clean

