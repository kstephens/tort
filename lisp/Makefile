BASE_DIR=..#
include $(BASE_DIR)/mk/config.mk

LIB = src/libtortlisp.la
TEST_LIBS = $(LIB) $(BASE_DIR)/ext/src/libtortext.la $(LIB_TORT)
CFLAGS_INC += -I$(BASE_DIR)/ext/include #

include $(BASE_DIR)/mk/target.mk
