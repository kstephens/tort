
ifeq "$(strip $(TAK_C_CALL))" ""
TAK_C_CALL=0
endif

CFLAGS= #
CFLAGS+= -O3 #
CFLAGS+= -DTAK_C_CALL=$(TAK_C_CALL) #
CFLAGS+= -g

all: piumarta-$(TAK_C_CALL)-run

piumarta-$(TAK_C_CALL)-run : piumarta-$(TAK_C_CALL)
	time ./$(@:-run=) || gdb --args $(@:-run=)

piumarta-$(TAK_C_CALL) : piumarta.c Makefile
	gcc -E $(CFLAGS) piumarta.c > piumarta-$(TAK_C_CALL).i
	gcc -g $(CFLAGS) piumarta.c -o $@

threadcomp : threadcomp.c isn.h
	gcc -O3 -g threadcomp.c -o $@

threadcomp.s : threadcomp.c isn.h
	gcc -O3 -S threadcomp.c -o $@

