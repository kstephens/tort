
CFLAGS = -Iinclude -g # -O2

all : tort_test

tort_test : tort_test.c src/tort.c src/error.c src/io.c src/write.c

tort_test : include/tort/*.h

test : tort_test
	gdb --args ./tort_test 

clean :
	rm -f tort_test

