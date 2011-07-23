BASE_DIR=.#
SUBDIRS=ext lisp#
include $(BASE_DIR)/mk/config.mk

LIB=$(LIB_TORT)
TEST_LIBS = $(LIB) $(LIB_TORT)

LIBS_EARLY += gc #

include $(BASE_DIR)/mk/target.mk

######################################################################
# default:
#

bootstrap : clean
	$(MAKE) clean
	-rm -f include/tort/integer.h.new
	-$(MAKE) 
	-rm -f include/tort/internal.h.new
	-$(MAKE) 
	-rm -f include/tort/d_m.h.new
	-$(MAKE) 
	-rm -f include/tort/symbol.h.new
	-$(MAKE) 
	$(MAKE)

######################################################################
# include:
#

include/tort/internal.h.new : include/tort/internal.h.gen ${LIB_CFILES} ${GEN_C_FILES} $(shell ls include/tort/integer.h)
include/tort/d_m.h.new  : include/tort/d_m.h.gen ${LIB_CFILES} ${GEN_C_FILES} $(shell ls include/tort/integer.h include/tort/internal.h)
include/tort/d_mt.h.new : include/tort/d_mt.h.gen $(LIB_CFILES) $(GEN_C_FILES) $(shell ls include/tort/d_m.h)
include/tort/d_s.h.new : include/tort/d_s.h.gen $(LIB_CFILES) $(GEN_C_FILES) $(shell ls include/tort/d_m.h)
include/tort/integer.h.new : include/tort/integer.h.gen src/integer.c

######################################################################
# libgc.a:
#

ifeq "$(TORT_GC)" "0"
gc :
else
gc : $(GC)/.libs/libgc.a

$(GC)/.libs/libgc.a : $(BASE_DIR)/archive/$(GC_VERSION).tar.gz
	if [ ! -d $(GC) ]; then tar -zxvf $^; fi
	unset CFLAGS LDFLAGS; cd $(GC) && if [ ! -f Makefile ]; then ./configure --enable-shared --prefix=$(PREFIX); fi
	unset CFLAGS LDFLAGS; cd $(GC) && make && make install

$(BASE_DIR)/archive/$(GC_VERSION).tar.gz :
	curl -Lk http://www.hpl.hp.com/personal/Hans_Boehm/gc/gc_source/$(GC_VERSION).tar.gz -o $@

endif

######################################################################
# debugging:
#

gdb : t/tort_test.t
	gdb --args t/tort_test.t 

disasm : t/tort_test.t
	objdump -DS t/tort_test.t | less "+/<main>"

