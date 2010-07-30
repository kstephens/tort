
ifeq "$(strip $(TAK_C_CALL))" ""
TAK_C_CALL=0
endif

C_FLAGS= #
C_FLAGS+= -O3 #
C_FLAGS+= -DTAK_C_CALL=$(TAK_C_CALL) #

piumarta-$(TAK_C_CALL)-run : piumarta-$(TAK_C_CALL)
	time ./$(@:-run=)
	#gdb --args $(@:-run=)

piumarta-$(TAK_C_CALL) : piumarta.c Makefile
	gcc -E $(C_FLAGS) piumarta.c > piumarta-$(TAK_C_CALL).i
	gcc -g $(C_FLAGS) piumarta.c -o $@
