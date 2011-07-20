
CFLAGS= #
CFLAGS+= -O3 #
CFLAGS+= -DTAK_C_CALL=$(TAK_C_CALL) #
CFLAGS+= -g

all: threadcomp
threadcomp : threadcomp.c isn.h
	gcc -O3 -g threadcomp.c -o $@

threadcomp.s : threadcomp.c isn.h
	gcc -O3 -S threadcomp.c -o $@

