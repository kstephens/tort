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
include $(BASE_DIR)/Makefile.gc
endif

