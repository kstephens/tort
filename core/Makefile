BASE_DIR=../#
include $(BASE_DIR)/mk/config.mk

LIB = src/libtortcore.la
TEST_LIBS = $(LIB) $(LIB_TORT)

include $(BASE_DIR)/mk/target.mk

######################################################################
# default:
#

bootstrap : veryclean
	-$(MAKE) make-gen-files
	$(MAKE)

.PHONY : bootstrap make-gen-files

######################################################################
# include:
#

include/tort/fixnum.h.new : include/tort/fixnum.h.gen src/fixnum.c
include/tort/internal.h.new : include/tort/internal.h.gen ${LIB_CFILES} ${GEN_C_FILES} $(shell ls include/tort/fixnum.h 2>/dev/null)
include/tort/d_m.h.new  : include/tort/d_m.h.gen ${LIB_CFILES} ${GEN_C_FILES} $(shell ls include/tort/fixnum.h include/tort/internal.h 2>/dev/null)
include/tort/d_mt.h.new : include/tort/d_mt.h.gen $(LIB_CFILES) $(GEN_C_FILES) include/tort/d_m.h.new
include/tort/d_slot.h.new : include/tort/d_slot.h.gen include/tort/d_slot.h.begin $(LIB_CFILES) $(GEN_C_FILES) 
include/tort/d_s.h.new : include/tort/d_s.h.gen $(LIB_CFILES) $(GEN_C_FILES) include/tort/d_m.h.new include/tort/d_slot.h.new 

######################################################################
# debugging:
#

gdb : t/tort_test.t
	gdb --args t/tort_test.t 

disasm : t/tort_test.t
	objdump -DS t/tort_test.t | less "+/<main>"

