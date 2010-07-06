GC=gc-7.0

CFLAGS_OPTIMIZE = -O2
CFLAGS = -Iinclude -I$(GC)/include -Wall -Werror -g $(CFLAGS_OPTIMIZE)
LDFLAGS = -L$(GC)/.libs
LIBS=-lgc

######################################################################

LIB_CFILES = $(shell ls src/*.c)
LIB_OFILES = $(LIB_CFILES:.c=.o)

######################################################################

TEST_C_FILES = $(shell ls t/*.c)
TEST_EXE_FILES = $(TEST_C_FILES:.c=.exe)
TEST_FILES = $(TEST_C_FILES:.c=)
TEST_OUT_FILES = $(TEST_C_FILES:.c=.out)

######################################################################

all : gc test

src/libtort.a : $(LIB_OFILES)
	$(AR) $(ARFLAGS) $@ $(LIB_OFILES)
	ranlib $@ || true

$(TEST_EXE_FILES) : src/libtort.a
	$(CC) $(CFLAGS) $(LDFLAGS) $(@:.exe=.c) src/libtort.a $(LIBS) -o $@

$(TEST_EXE_FILES) $(LIB_OFILES) : include/tort/*.h

gc : $(GC)/.libs/libgc.a

$(GC)/.libs/libgc.a : $(GC).tar.gz
	if [ ! -d $(GC) ]; then tar -zxvf $^; fi
	cd $(GC) && if [ ! -f Makefile ]; then ./configure; fi
	cd $(GC) && make

run-test : $(TEST_EXE_FILES)
	@set -ex; for f in $(TEST_EXE_FILES); do \
	  $$f ;\
	done

test : $(TEST_EXE_FILES)
	@echo "Testing:"
	@set -e ;\
	for f in $(TEST_FILES); do \
	  in=/dev/null ;\
	  if [ -f $$f.in ] ; then in=$$f.in ; fi ;\
	  echo -n "  test $$f.exe < $$in: " ;\
	  ($$f.exe <$$in || echo $$?) 2>&1 | t/filter-output > $$f.out ;\
	  if ! diff -U 10 $$f.exp $$f.out ; then \
	    echo "  ========== $$f.out ==========" 1>&2 ;\
	    cat $$f.out ;\
	    echo "  ========== To accept, run: " 1>&2 ;\
	    echo "    rm -f $$f.exp; make accept-test;" ;\
	  fi ;\
	  echo "ok" ;\
	done

accept-test : $(TEST_EXE_FILES)
	@set -ex; for f in $(TEST_EXE_FILES); do \
	  if [ ! -f $$f.exp ] ; then cp $$f.out $$f.exp ; fi ;\
	done
	git add t/*.c t/*.exp t/*.in

gdb : t/tort_test.exe
	gdb --args t/tort_test.exe 

disasm : t/tort_test.exe
	objdump -DS t/tort_test.exe | less "+/<main>"

clean :
	rm -f $(TEST_EXE_FILES) src/libtort.a src/*.o t/*.out
very-clean : clean
	cd $(GC) && make clean

stats :
	@echo "LoC:"
	@find src include -name '*.h' -o -name '*.c' | xargs wc -l
