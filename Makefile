BASE_DIR=.#
SUBDIRS=core ext lisp compiler#
include $(BASE_DIR)/mk/config.mk

#LIB=$(LIB_TORT)
#TEST_LIBS = $(LIB) $(LIB_TORT)

include $(BASE_DIR)/mk/target.mk

bootstrap : very-clean clean
	$(MAKE) -C core bootstrap
#	$(MAKE) clean 
#	$(MAKE)

clean ::
	rm -f tmp/*.* GPATH GRTAGS GTAGS

tags :
	gtags

######################################################################
# libgc.a:
#

ifneq "$(TORT_GC_BDW)" "0"
LIBS_EARLY += gc-bdw #

gc-bdw : $(PREFIX)/lib/libatomic_ops.a $(PREFIX)/lib/libgc.a

$(PREFIX)/lib/libgc.a : $(BASE_DIR)/archive/$(GC_BDW_VERSION).tar.gz
	if [ ! -d $(GC_BDW) ]; then tar -zxvf $^; fi
	set -xe ;\
	unset CFLAGS LDFLAGS ;\
	export CC='$(CC) -I$(PREFIX)/include -L$(PREFIX)/lib' ;\
	cd $(GC_BDW) ;\
	if [ ! -f Makefile ]; then ./configure --enable-shared --enable-parallel-mark --enable-threads=posix --enable-large-config --enable-gc-debug USE_I686_PREFETCH=1 --prefix=$(PREFIX); fi ;\
	make && make install

$(PREFIX)/lib/libatomic_ops.a : $(BASE_DIR)/archive/$(LIBATOMIC_OPS_VERSION).tar.gz
	if [ ! -d $(LIBATOMIC_OPS) ]; then tar -zxvf $^; fi
	set -xe ;\
	unset CFLAGS LDFLAGS ;\
	export CC='$(CC) -I$(PREFIX)/include -L$(PREFIX)/lib' ;\
	cd $(LIBATOMIC_OPS) ;\
	if [ ! -f Makefile ]; then ./configure --enable-shared --enable-parallel-mark --enable-threads=posix --enable-large-config --enable-gc-debug USE_I686_PREFETCH=1 --prefix=$(PREFIX); fi ;\
	make && make install

$(BASE_DIR)/archive/$(GC_BDW_VERSION).tar.gz :
	mkdir -p $(BASE_DIR)/archive
	curl -Lk http://hboehm.info/gc/gc_source/$(GC_BDW_VERSION).tar.gz -o $@

$(BASE_DIR)/archive/$(LIBATOMIC_OPS_VERSION).tar.gz :
	mkdir -p $(BASE_DIR)/archive
	curl -Lk http://hboehm.info/gc/gc_source/$(LIBATOMIC_OPS_VERSION).tar.gz -o $@

endif

