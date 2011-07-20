
CFLAGS= #
CFLAGS+= -O3 #
CFLAGS+= -g #

all: threadcomp.i threadcomp

threadcomp : threadcomp.c isn.h
	gcc $(CFLAGS) threadcomp.c -o $@

threadcomp.i : threadcomp.c isn.h
	gcc $(CFLAGS) -E threadcomp.c -o $@

threadcomp.s : threadcomp.c isn.h
	gcc $(CFLAGS) -S threadcomp.c -o $@

