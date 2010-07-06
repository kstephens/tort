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
# default:
#

all : gc include/tort/internal.h test


######################################################################
# include:
#

include/tort/internal.h : include/tort/internal.h.* $(LIB_CFILES) Makefile
	( \
	cat $@.begin; \
	cat $(LIB_CFILES) | perl -ne 'print "extern ", $$_, ";\n" if ( /^tort_v\s+_tort_[a-z0-9_]+\s*[(].*[)]\s*$$/ ); ' ; \
	cat $@.end; \
	) > $@

src/lisp.o : src/lispread.c


######################################################################
# library:
#

src/libtort.a : $(LIB_OFILES)
	$(AR) $(ARFLAGS) $@ $(LIB_OFILES)
	ranlib $@ || true

src/lisp.o : src/lispread.c

######################################################################
# libgc.a:
#

gc : $(GC)/.libs/libgc.a

$(GC)/.libs/libgc.a : $(GC).tar.gz
	if [ ! -d $(GC) ]; then tar -zxvf $^; fi
	cd $(GC) && if [ ! -f Makefile ]; then ./configure; fi
	cd $(GC) && make

######################################################################
# testing:
#

$(TEST_EXE_FILES) : src/libtort.a
	$(CC) $(CFLAGS) $(LDFLAGS) $(@:.exe=.c) src/libtort.a $(LIBS) -o $@

$(TEST_EXE_FILES) $(LIB_OFILES) : include/tort/*.h

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
	@set -ex; for f in $(TEST_FILES); do \
	  if [ ! -f $$f.exp ] ; then cp $$f.out $$f.exp ; fi ;\
	done
	git add t/*.c t/*.exp t/*.in

######################################################################
# debugging:
#

gdb : t/tort_test.exe
	gdb --args t/tort_test.exe 

disasm : t/tort_test.exe
	objdump -DS t/tort_test.exe | less "+/<main>"

######################################################################
# maint:
#

clean :
	rm -f $(TEST_EXE_FILES) src/libtort.a src/*.o t/*.out include/tort/internal.h

very-clean : clean
	cd $(GC) && make clean

stats :
	@echo "LoC:"
	@find Makefile src include t -name '*.h' -o -name '*.c' -o -name '*.in' -o -name Makefile | xargs wc -l
