GC=gc-7.0

CFLAGS = -Iinclude -I$(GC)/include -g # -O2
LDFLAGS = -L$(GC)/.libs
LIBS=-lgc

all : gc tort_test

CFILES = src/tort.c src/error.c src/io.c src/write.c
OFILES = $(CFILES:.c=.o)

libtort.a : $(OFILES)
	$(AR) $(ARFLAGS) $@ $(OFILES)
	ranlib $@ || true

tort_test : tort_test.c libtort.a
	$(CC) $(CFLAGS) $(LDFLAGS) $^ $(LIBS) -o $@

# tort_test : include/tort/*.h

gc : $(GC)/.libs/libgc.a

$(GC)/.libs/libgc.a : $(GC) $(GC)/*.c $(GC)/*.h
	cd $(GC) && [ ! -f Makefile ] && ./configure
	cd $(GC) && make

$(GC) : $(GC).tar.gz
	tar -zxvf $^

test : tort_test
	gdb --args ./tort_test 

clean :
	rm -f tort_test libtort.a src/*.o

very-clean : clean
	cd $(GC) && make clean

