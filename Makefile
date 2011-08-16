BASE_DIR=.#
SUBDIRS=core ext lisp#
include $(BASE_DIR)/mk/config.mk

LIB=# $(LIB_TORT)
TEST_LIBS = $(LIB) $(LIB_TORT)

LIBS_EARLY += gc #

include $(BASE_DIR)/mk/target.mk

bootstrap : clean
	$(MAKE) -C core bootstrap

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

