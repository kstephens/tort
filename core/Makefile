BASE_DIR=../#
include $(BASE_DIR)/mk/config.mk

LIB = src/libtortcore.la
TEST_LIBS = $(LIB) $(LIB_TORT)

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

include/tort/internal.h.new : include/tort/internal.h.gen ${LIB_CFILES} ${GEN_C_FILES} $(shell ls include/tort/integer.h 2>/dev/null)
include/tort/d_m.h.new  : include/tort/d_m.h.gen ${LIB_CFILES} ${GEN_C_FILES} $(shell ls include/tort/integer.h include/tort/internal.h 2>/dev/null)
include/tort/d_mt.h.new : include/tort/d_mt.h.gen $(LIB_CFILES) $(GEN_C_FILES) $(shell ls include/tort/d_m.h 2>/dev/null)
include/tort/d_s.h.new : include/tort/d_s.h.gen $(LIB_CFILES) $(GEN_C_FILES) $(shell ls include/tort/d_m.h 2>/dev/null)
include/tort/integer.h.new : include/tort/integer.h.gen src/integer.c

######################################################################
# debugging:
#

gdb : t/tort_test.t
	gdb --args t/tort_test.t 

disasm : t/tort_test.t
	objdump -DS t/tort_test.t | less "+/<main>"

