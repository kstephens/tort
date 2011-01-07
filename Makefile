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
CFLAGS_OPTIMIZE = -O3
#CFLAGS_OPTIMIZE = 
CFLAGS += -DTORT_DLIB_DIR='"$(libdir)"' -DTORT_DLIB_SUFFIX='".dylib"' #
CFLAGS += -DUSE_GC=$(USE_GC) -fnested-functions -Iinclude -Iext/include -Iboot/include -I$(GC)/include -Wall -Werror -g $(CFLAGS_OPTIMIZE)

ifeq "$(USE_GC)" "0"
else
LDFLAGS += -L$(PREFIX)/lib #
LIBS += -lgc #
endif

######################################################################

GEN_FILES = 

GEN_LIBS = 

GEN_H_GEN_FILES = $(shell ls include/tort/*.h.gen | sort -u)
GEN_H_FILES = $(GEN_H_GEN_FILES:%.gen=%)
GEN_FILES += $(GEN_H_FILES)
GEN_C_FILES = #

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

export CC
export CFLAGS

######################################################################

TEST_C_FILES = $(shell ls t/*.c ext/t/*.c | sort -u)
TEST_T_FILES = $(TEST_C_FILES:.c=.t)
TEST_FILES = $(TEST_C_FILES:.c=)
TEST_OUT_FILES = $(TEST_C_FILES:.c=.out)

######################################################################
# default:
#

all : components tests

components : early $(GEN_H_FILES) $(GEN_C_FILES) libs 

libs : gc $(GEN_LIBS)

early : early-headers early-files check-gen-new

early-headers: boot/include/.touch

GEN_FILES_NEW=$(GEN_FILES:%.gen=%.new)#

check-gen-new : $(GEN_FILES_NEW)
	@set -e ;\
	for f in $(GEN_FILES_NEW:%.new=%) ;\
	do \
	  cmp -s "$$f.new" "$$f" || cp -p "$$f.new" "$$f" ;\
	done

boot: boot/include/.touch

boot/include/.touch :
	set -ex; for f in $(GEN_H_FILES); do \
	  dst="boot/$$f" ;\
	  mkdir -p `dirname $$dst` ;\
	  cat $$f.begin $$f.end > $$dst ;\
	  cp "$$dst" "$$f" ;\
	done
	touch $@


######################################################################
# include:
#

includes : $(GEN_H_FILES)

early-files: $(GEN_H_FILES:%=%.new)

include/tort/internal.h.new : include/tort/internal.h.gen ${LIB_CFILES} ${GEN_C_FILES} $(shell ls include/tort/integer.h)
include/tort/d_m.h.new  : include/tort/d_m.h.gen ${LIB_CFILES} ${GEN_C_FILES} $(shell ls include/tort/integer.h include/tort/internal.h)
include/tort/d_mt.h.new : include/tort/d_mt.h.gen $(LIB_CFILES) $(GEN_C_FILES) $(shell ls include/tort/d_m.h)
include/tort/d_s.h.new : include/tort/d_s.h.gen $(LIB_CFILES) $(GEN_C_FILES) $(shell ls include/tort/d_m.h)
include/tort/integer.h.new : include/tort/integer.h.gen src/integer.c

%.new : %.gen
	$(@:.new=).gen $(@:.new=) $@

######################################################################
# src:
#

srcs : $(GEN_C_FILES)

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

$(LIB_OFILES) : $(LIB_HFILES)
$(EXTLIB_OFILES) : $(LIB_HFILES) $(EXTLIB_HFILES)

######################################################################
# libgc.a:
#

ifeq "$(USE_GC)" "0"
gc :
else
gc : $(GC)/.libs/libgc.a

$(GC)/.libs/libgc.a : archive/$(GC).tar.gz
	if [ ! -d $(GC) ]; then tar -zxvf $^; fi
	cd $(GC) && if [ ! -f Makefile ]; then ./configure --enable-shared --prefix=$(PREFIX); fi
	cd $(GC) && make && make install
endif

######################################################################
# testing:
#

TEST_LIBS = $(LIB_TORTEXT) $(LIB_TORT)
# TEST_LIBS = $(LIB_TORT)

%.t : %.c 
	$(LIBTOOL) --mode=link $(CC) $(CFLAGS) $(LDFLAGS) $(@:.t=.c) $(TEST_LIBS) $(LIBS) -o $@

tests : components $(TEST_T_FILES) 

$(TEST_T_FILES) : $(TEST_LIBS)

$(TEST_T_FILES) $(LIB_OFILES) : $(GEN_H_FILES) include/tort/*.h ext/include/tort/*.h

run-test : tests
	@set -ex; for f in $(TEST_T_FILES); do \
	  $$f ;\
	done

test : tests
	@echo "Testing:"
	tool/t run $(TEST_FILES)

valgrind : tests
	@echo "Valgrind:"
	tool/t valgrind $(TEST_FILES)

accept-test : $(TEST_T_FILES)
	@set -ex; for f in $(TEST_FILES); do \
	  if [ ! -f $$f.exp ] ; then cp $$f.out $$f.exp ; git add `ls $$f.{c,exp,in}`; fi ;\
	done

accept-all-test : $(TEST_T_FILES)
	@set -ex; for f in $(TEST_FILES); do \
	  if [ -f $$f.out ] ; then cp $$f.out $$f.exp ; git add `ls $$f.{c,exp,in}`; fi ;\
	done

######################################################################
# debugging:
#

gdb : t/tort_test.t
	gdb --args t/tort_test.t 

disasm : t/tort_test.t
	objdump -DS t/tort_test.t | less "+/<main>"

######################################################################
# maint:
#

clean :
	rm -f $(TEST_T_FILES) $(GEN_LIBS) {.,ext}/src/*{.o,.lo,.la} {.,ext}/t/*.{t,out} $(GEN_C_FILES) $(GEN_H_FILES) .stats/*
	rm -rf boot/include
	find . -name '*.dSYM' -type d -print0 | xargs -0 rm -rf
	find src ext -name '.libs' -type d -print0 | xargs -0 rm -rf

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

