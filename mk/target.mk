GEN_H_GEN_FILES := $(shell ls include/tort/*.h.gen 2>/dev/null | sort -u | grep -v 'include/tort/*.h.gen')
GEN_H_FILES = $(GEN_H_GEN_FILES:%.gen=%)
GEN_FILES += $(GEN_H_FILES)
GEN_C_FILES = #

LIB_CFILES := $(shell ls src/*.c $(GEN_C_FILES) 2>/dev/null | fgrep -v src/_.c | sort -u)
LIB_HFILES := $(shell ls include/tort/*.h $(GEN_H_FILES) 2>/dev/null | sort -u | grep -v 'include/tort/*.h')
LIB_CFILES_ALL := $(LIB_CFILES)
LIB_CFILES := src/_.c
LIB_OFILES = $(LIB_CFILES:.c=.lo)

GEN_LIBS += $(LIB) #
GEN_LIBS += $(LIB:.a=.so) #

BIN_CFILES := $(shell ls bin/*.c 2>/dev/null | sort -u | grep -v 'bin/*.c')
BIN_OFILES := $(BIN_CFILES:%.c=%.lo)
BIN_EFILES := $(BIN_OFILES:%.lo=%)

GEN_BINS += $(BIN_OFILES) $(BIN_EFILES) #

export LIB_CFILES
export LIB_HFILES
export LIB_OFILES

export CC
export CFLAGS
export CPPFLAGS

######################################################################

TEST_C_FILES = $(shell ls t/*.c 2>/dev/null | sort -u)
TEST_T_FILES = $(TEST_C_FILES:.c=.t)
TEST_FILES = $(TEST_C_FILES:.c=)
TEST_OUT_FILES = $(TEST_C_FILES:.c=.out)

######################################################################

all : components tests $(OTHER_TARGETS)
	@for d in $(SUBDIRS) .; do [ $$d = '.' ] && break; $(MAKE) -wC "$$d" all; done
components : early $(GEN_H_FILES) $(GEN_C_FILES) libs bins
libs : $(LIBS_EARLY) $(GEN_LIBS)
bins : $(BINS_EARLY) $(GEN_BINS)
$(BINS_EARLY) $(GEN_BINS) : $(LINK_DEPS)
early : early-headers early-files check-gen-new
early-headers: boot/include/.touch
GEN_FILES_NEW=$(GEN_H_GEN_FILES:%.gen=%.new)#
gen-files-new: $(GEN_FILES_NEW)
check-gen-new : $(GEN_FILES_NEW)
	@set -e ;\
	for f in $(GEN_FILES_NEW:%.new=%) ;\
	do \
	  cmp -s "$$f.new" "$$f" || cp -p "$$f.new" "$$f" ;\
	done
boot: boot/include/.touch
boot/include/.touch :
	-rm -f $(GEN_FILES_NEW)
	@mkdir -p $$(dirname $@)
	set -ex; for f in $(GEN_H_FILES); do \
	  dst="boot/$$f" ;\
	  mkdir -p `dirname $$dst` ;\
	  cat $$f.begin $$f.end > $$dst ;\
	  touch $$dst.new ;\
	  cp "$$dst" "$$f" ;\
	done
	touch $@

make-gen-files : boot gen-files-new check-gen-new

.PHONY: all components libs bins early early-headers make-gen-files gen-files-new check-gen-new boot

######################################################################

includes : $(GEN_H_FILES)
early-files: $(GEN_H_FILES:%=%.new)
%.new : %.gen
	$(@:.new=).gen $(@:.new=) $@

######################################################################

srcs : $(GEN_C_FILES)

######################################################################

%.lo : %.c
	$(COMPILE.c) $(CFLAGS_DEBUG) -o $@ $<

%.i : %.c 
	$(CC_BASE) -E -o $@ $<

%.s : %.c 
	$(CC_BASE:%-g%=%%) -S -o $@ $<

######################################################################

% : %.lo
	$(LIBTOOL) --tag=LD --mode=link $(CC) $(CFLAGS) $(CFLAGS_DEBUG) $(LDFLAGS) $@.lo $(BIN_LIBS) $(LIBS) -o $@


######################################################################

$(LIB_CFILES) : $(LIB_CFILES_ALL)
	cat $(LIB_CFILES_ALL) > $@

$(LIB) : $(LIB_OFILES) $(LINK_DEPS)
	$(LIBTOOL) --tag=LD --mode=link $(CC) $(LDFLAGS) $(LIB_FLAGS) -o $@ $(LIB_OFILES) $(LIBS)
	$(LIBTOOL) --mode=install cp $@ $(libdir)
$(LIB_OFILES) : $(LIB_HFILES)

######################################################################

# TEST_LIBS = $(LIB_TORT)

%.t : %.c 
	$(LIBTOOL) --tag=LD --mode=link $(CC) $(CFLAGS) $(LDFLAGS) $(@:.t=.c) $(TEST_LIBS) $(LIBS) -o $@
tests : components $(TEST_T_FILES) 
$(TEST_T_FILES) : $(TEST_LIBS) $(LINK_DEPS)
$(TEST_T_FILES) $(LIB_OFILES) : $(GEN_H_FILES) include/tort/*.h 
run-test : tests
	@set -ex; for f in $(TEST_T_FILES); do \
	  $$f ;\
	done
	@for d in $(SUBDIRS) .; do [ $$d = '.' ] && break; $(MAKE) -wC "$$d" run-test; done
test : tests
	@echo "Testing:"
	$(BASE_DIR)/tool/t run $(TEST_FILES)
	@for d in $(SUBDIRS) .; do [ $$d = '.' ] && break; $(MAKE) -wC "$$d" test; done
valgrind : tests
	@echo "Valgrind:"
	$(BASE_DIR)/tool/t valgrind $(TEST_FILES)
	@for d in $(SUBDIRS) .; do [ $$d = '.' ] && break; $(MAKE) -wC "$$d" valgrind; done
accept-test : tests
	$(BASE_DIR)/tool/t accept $(TEST_FILES)
accept-all-test : tests
	$(BASE_DIR)/tool/t accept $(TEST_FILES)
	@for d in $(SUBDIRS) .; do [ $$d = '.' ] && break; $(MAKE) -wC "$$d" accept-all-test; done

######################################################################

clean ::
	rm -f $(TEST_T_FILES) $(GEN_LIBS) $(GEN_BINS) src/*.{o,lo,la} t/*.{t,out}
	find . -name '*.dSYM' -type d -print0 | xargs -0 rm -rf
	rm -rf src/.libs/ bin/.libs/ t/.libs/
	@for d in $(SUBDIRS) .; do [ "$$d" = '.' ] && break; $(MAKE) -wC "$$d" clean; done
veryclean :: very-clean
very-clean :: clean
	rm -f $(GEN_C_FILES) $(GEN_H_FILES) $(GEN_FILES_NEW) .stats/*
	rm -rf boot/include
	@for d in $(SUBDIRS) .; do [ "$$d" = '.' ] && break; $(MAKE) -wC "$$d" very-clean; done

.PHONY: clean veryclean very-clean

FIND_STAT_FILES= \
   -name '*.h' -o -name '*.c' -o -name '*.lisp' -o -name '*.scm' \
-o -name '*.in' \
-o -name '*.begin' -o -name '*.end' -o -name '*.gen' \
-o -name Makefile

stats :
	@mkdir -p .stats
	@echo "`/bin/pwd`:"
	@echo "  Generated LoC:"
	@find $(GEN_H_FILES) $(GEN_C_FILES) 2>/dev/null | \
	  sort -u > .stats/files_gen
	@xargs wc -l < .stats/files_gen
	@echo "  Source LoC:"
	@find src include lib $(FIND_STAT_FILES) 2>/dev/null | \
	  sort -u > .stats/files.t
	@comm -23 .stats/files.t .stats/files_gen > .stats/files_src
	@xargs wc -l < .stats/files_src
	@echo "  Test LoC:"
	@find t $(FIND_STAT_FILES) 2>/dev/null | \
	  sort -u > .stats/files.t
	@comm -23 .stats/files.t .stats/files_gen > .stats/files_t
	@xargs wc -l < .stats/files_t
	@for d in $(SUBDIRS) .; do [ "$$d" = '.' ] && break; $(MAKE) -wC "$$d" stats; done
