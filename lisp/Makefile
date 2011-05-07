BASE_DIR=..#
include $(BASE_DIR)/mk/config.mk
CFLAGS += -DTORT_LISP_LIB_DIR='"$(BASE_DIR)/lisp/lib"' #

LIB = src/libtortlisp.la
TEST_LIBS = $(LIB) $(BASE_DIR)/ext/src/libtortext.la $(LIB_TORT)
CFLAGS_INC += -I$(BASE_DIR)/ext/include #

include $(BASE_DIR)/mk/target.mk
