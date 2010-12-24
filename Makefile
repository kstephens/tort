GC=bdwgc-20101223-cvs

CFLAGS_OPTIMIZE = -O2
CFLAGS = -fnested-functions -Iinclude -I$(GC)/include -Wall -Werror -g $(CFLAGS_OPTIMIZE)
LDFLAGS = -L$(GC)/.libs
LIBS=-lgc

######################################################################

GEN_H_FILES = include/tort/internal.h
GEN_C_FILES = src/symbol.c src/method.c

LIB_CFILES = $(shell ls src/*.c) $(GEN_C_FILES)
LIB_HFILES = $(shell ls include/tort/*.h) $(GEN_H_FILES)
LIB_OFILES = $(LIB_CFILES:.c=.o)

export LIB_CFILES
export LIB_HFILES
export LIB_OFILES

LIBTORT     = src/libtort.a

######################################################################

TEST_C_FILES = $(shell ls t/*.c)
TEST_EXE_FILES = $(TEST_C_FILES:.c=.exe)
TEST_FILES = $(TEST_C_FILES:.c=)
TEST_OUT_FILES = $(TEST_C_FILES:.c=.out)

######################################################################
# default:
#

all : gc $(GEN_H_FILES) $(GEN_C_FILES) $(LIBTORT) test stats


######################################################################
# include:
#


includes : $(GEN_H_FILES)

include/tort/internal.h : include/tort/internal.h.* $(LIB_CFILES)
	( set -e; \
	cat $@.begin; \
	cat $(LIB_CFILES) | perl -ne 'print "extern ", $$_, ";\n" if ( /^tort_v\s+_tort_[a-z0-9_]+\s*[(].*[)]\s*$$/ ); ' ; \
	cat $@.end; \
	) > $@

######################################################################
# src:
#

srcs : $(GEN_C_FILES)

src/symbol.c : src/symbol.c.* include/tort/tort.h
	$@.gen $@

src/method.c : src/method.c.* ${LIB_CFILES}
	$@.gen $@

$(GEN_C_FILES) $(GEN_H_FILES) : Makefile

######################################################################
# object:
#

src/lisp.o : src/lispread.c


######################################################################
# library:
#

$(LIBTORT) : $(LIB_OFILES)
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

%.exe : %.c 
	$(CC) $(CFLAGS) $(LDFLAGS) $(@:.exe=.c) src/libtort.a $(LIBS) -o $@

$(TEST_EXE_FILES) : src/libtort.a

$(TEST_EXE_FILES) $(LIB_OFILES) : $(GEN_H_FILES) include/tort/*.h

run-test : $(TEST_EXE_FILES)
	@set -ex; for f in $(TEST_EXE_FILES); do \
	  $$f ;\
	done

test : $(TEST_EXE_FILES)
	@echo "Testing:"
	@set -e ;\
	errors=0 ;\
	for f in $(TEST_FILES); do \
	  in=/dev/null ;\
	  if [ -f $$f.in ] ; then in=$$f.in ; fi ;\
	  echo -n "  test $$f.exe < $$in: " ;\
	  ($$f.exe <$$in || echo $$?) 2>&1 | t/filter-output > $$f.out ;\
	  if ! diff -U 10 $$f.exp $$f.out ; then \
	    echo "========== $$f.out ==========" 1>&2 ;\
	    cat $$f.out ;\
	    echo "========== To accept, run: " 1>&2 ;\
	    echo "  rm -f $$f.exp; make accept-test;" ;\
	    echo "  # OR make accept-all-test;" ;\
	    errors=1 ;\
	  fi ;\
	  echo "ok" ;\
	done ;\
	exit $$errors

valgrind : $(TEST_EXE_FILES)
	@echo "Valgrind:"
	@set -e ;\
	errors=0 ;\
	for f in $(TEST_FILES); do \
	  in=/dev/null ;\
	  if [ -f $$f.in ] ; then in=$$f.in ; fi ;\
	  echo -n "  valgrind $$f.exe < $$in: " ;\
	  (TORT_GC=0 valgrind $$f.exe <$$in || echo $$?) 2>&1 | t/filter-output ;\
	  echo "ok" ;\
	done ;\
	exit $$errors

accept-test : $(TEST_EXE_FILES)
	@set -ex; for f in $(TEST_FILES); do \
	  if [ ! -f $$f.exp ] ; then cp $$f.out $$f.exp ; fi ;\
	done
	git add t/*.c t/*.exp t/*.in

accept-all-test : $(TEST_EXE_FILES)
	@set -ex; for f in $(TEST_FILES); do \
	  if [ -f $$f.out ] ; then cp $$f.out $$f.exp ; fi ;\
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
	rm -f $(TEST_EXE_FILES) src/libtort.a src/*.o t/*.out include/tort/internal.h .stats/*

very-clean : clean
	cd $(GC) && make clean

FIND_STAT_FILES= \
   -name '*.h' -o -name '*.c' \
-o -name '*.in' \
-o -name '*.begin' -o -name '*.end' -o -name '*.gen' \
-o -name Makefile

stats :
	mkdir -p .stats
	ls $(GEN_H_FILES) $(GEN_C_FILES) | \
	  sort -u > .stats/files_gen
	@echo "Source LoC:"
	find Makefile src include $(FIND_STAT_FILES) | \
	  sort -u > .stats/files.t
	comm -3 .stats/files.t .stats/files_gen > .stats/files_src
	xargs wc -l < .stats/files_src
	@echo "Test LoC:"
	find t $(FIND_STAT_FILES) | \
	  sort -u > .stats/files.t
	comm -3 .stats/files.t .stats/files_gen > .stats/files_t
	xargs wc -l < .stats/files_t
	@echo "Generated LoC:"
	xargs wc -l < .stats/files_gen

