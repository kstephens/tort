
C_FILES:=$(shell ls *.c)#
S_FILES:=$(C_FILES:%.c=%.s)#
O_FILES:=$(S_FILES:%.s=%.o)#

CFLAGS=--verbose -fPIC -DPIC -D__DYNAMIC__ -export-dynamic -fno-common -I ../../core/include #

all : $(S_FILES)

%.s : %.c
	gcc $(CFLAGS) -S -o $@ $<
%.o : %.s
	gcc $(CFLAGS) -c -o $@ $<

$(S_FILES) $(O_FILES) : Makefile

clean :
	rm -f *.o *.s

.PHONY : all clean
