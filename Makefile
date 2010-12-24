GC=gc-20101223-cvs
USE_GC=1

PREFIX:=$(shell mkdir -p local && cd local && /bin/pwd)#
libdir=$(PREFIX)/lib#

LIBTOOL=$(GC)/libtool #
CC=gcc#
COMPILE.c = $(LIBTOOL) --mode=compile $(CC) $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH) -c #
CFLAGS += -shared -export-dynamic #
LIB_FLAGS += -rpath $(libdir) #

CFLAGS_OPTIMIZE = -O2
CFLAGS_OPTIMIZE = 
CFLAGS += -DLIB_DIR='"$(libdir)"' #
CFLAGS += -DUSE_GC=$(USE_GC) -fnested-functions -Iinclude -Iext/include -I$(GC)/include -Wall -Werror -g $(CFLAGS_OPTIMIZE)

ifeq "$(USE_GC)" "0"
else
LDFLAGS += -L$(PREFIX)/lib #
LIBS += -lgc #
endif

######################################################################

GEN_LIBS = 

GEN_H_FILES = include/tort/internal.h
GEN_C_FILES = src/symbol.c src/method.c

LIB_CFILES := $(shell ls src/*.c $(GEN_C_FILES) | sort -u) 
LIB_HFILES := $(shell ls include/tort/*.h $(GEN_H_FILES) | sort -u) 
LIB_OFILES = $(LIB_CFILES:.c=.lo)

LIB_TORT     = src/libtort.la
GEN_LIBS += $(LIB_TORT)

export LIB_CFILES
export LIB_HFILES
export LIB_OFILES

LIBEXT_CFILES := $(shell ls ext/src/*.c)
LIBEXT_HFILES := $(shell ls ext/include/tort/*.h)
LIBEXT_OFILES = $(LIBEXT_CFILES:.c=.lo)

LIB_TORTEXT   = ext/src/libtortext.la
GEN_LIBS += $(LIB_TORTEXT) #
GEN_LIBS += $(LIB_TORTEXT:.a=.so) #

export LIBEXT_CFILES
export LIBEXT_HFILES
export LIBEXT_OFILES

######################################################################

TEST_C_FILES = $(shell ls t/*.c ext/t/*.c | sort -u)
TEST_EXE_FILES = $(TEST_C_FILES:.c=.exe)
TEST_FILES = $(TEST_C_FILES:.c=)
TEST_OUT_FILES = $(TEST_C_FILES:.c=.out)

######################################################################
# default:
#

all : gc $(GEN_H_FILES) $(GEN_C_FILES) $(GEN_LIBS) test stats


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

%.lo : %.c
	$(COMPILE.c) -o $@ $<

src/lisp.lo : src/lispread.c


######################################################################
# library:
#

$(LIB_TORT) : $(LIB_OFILES)
	$(LIBTOOL) --mode=link $(CC) $(LDFLAGS) $(LIB_FLAGS) -o $@ $(LIB_OFILES)
	$(LIBTOOL) --mode=install cp $@ $(libdir)

$(LIB_TORTEXT) : $(LIBEXT_OFILES) 
	$(LIBTOOL) --mode=link $(CC) $(LDFLAGS) $(LIB_FLAGS) -o $@ $(LIBEXT_OFILES)
	$(LIBTOOL) --mode=install cp $@ $(libdir)

ext/src/lisp.o : ext/src/lispread.c

######################################################################
# libgc.a:
#

ifeq "$(USE_GC)" "0"
gc :
else
gc : $(GC)/.libs/libgc.a

$(GC)/.libs/libgc.a : $(GC).tar.gz
	if [ ! -d $(GC) ]; then tar -zxvf $^; fi
	cd $(GC) && if [ ! -f Makefile ]; then ./configure --enable-shared --prefix=$(PREFIX); fi
	cd $(GC) && make && make install
endif

######################################################################
# testing:
#

TEST_LIBS = $(LIB_TORTEXT) $(LIB_TORT)
# TEST_LIBS = $(LIB_TORT)

%.exe : %.c 
	$(LIBTOOL) --mode=link $(CC) $(CFLAGS) $(LDFLAGS) $(@:.exe=.c) $(TEST_LIBS) $(LIBS) -o $@

$(TEST_EXE_FILES) : $(TEST_LIBS)

$(TEST_EXE_FILES) $(LIB_OFILES) : $(GEN_H_FILES) include/tort/*.h ext/include/tort/*.h

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
	rm -f $(TEST_EXE_FILES) $(GEN_LIBS) {.,ext}/src/*{.o,.lo,.la} {.,ext}/t/*.{exe,out} include/tort/internal.h .stats/*
	find . -name '*.dSYM' -type d -print0 | xargs -0 rm -rf

very-clean : clean
	cd $(GC) && make clean

FIND_STAT_FILES= \
   -name '*.h' -o -name '*.c' \
-o -name '*.in' \
-o -name '*.begin' -o -name '*.end' -o -name '*.gen' \
-o -name Makefile

stats :
	mkdir -p .stats
	@echo "core Generated LoC:"
	@find $(GEN_H_FILES) $(GEN_C_FILES) | \
	  sort -u > .stats/files_gen
	@xargs wc -l < .stats/files_gen
	@echo "core Source LoC:"
	@find src include $(FIND_STAT_FILES) | \
	  sort -u > .stats/files.t
	@comm -23 .stats/files.t .stats/files_gen > .stats/files_src
	@xargs wc -l < .stats/files_src
	@echo "core Test LoC:"
	@find t $(FIND_STAT_FILES) | \
	  sort -u > .stats/files.t
	@comm -23 .stats/files.t .stats/files_gen > .stats/files_t
	@xargs wc -l < .stats/files_t
	@echo "ext/ Source LoC:"
	@find ext/src ext/include $(FIND_STAT_FILES) | \
	  sort -u > .stats/files.t
	@comm -23 .stats/files.t .stats/files_gen > .stats/files_src
	@xargs wc -l < .stats/files_src
	@echo "ext/ Test LoC:"
	@find ext/t $(FIND_STAT_FILES) | \
	  sort -u > .stats/files.t
	@comm -23 .stats/files.t .stats/files_gen > .stats/files_t
	@xargs wc -l < .stats/files_t

