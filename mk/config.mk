UNAME_S:=$(shell uname -s 2>/dev/null)#
BASE_DIR:=$(shell cd "$(BASE_DIR)" && /bin/pwd)#
GC_BDW_VERSION=gc-20101223-cvs#
GC_BDW_VERSION=gc-7.2alpha6#
GC_BDW=$(BASE_DIR)/$(GC_BDW_VERSION)#
TORT_GC_BDW=1#
TORT_GC_SMAL=0#

PREFIX:=$(shell mkdir -p $(BASE_DIR)/local && cd $(BASE_DIR)/local && /bin/pwd)#
libdir=$(PREFIX)/lib#
export LD_LIBRARY_PATH
LD_LIBRARY_PATH:=$(libdir):$(LD_LIBRARY_PATH)

LIBTOOL=$(GC_BDW)/libtool #
CC=gcc-4.3# # for -fnested-functions support
CC=gcc#
#CC=gcc-mp-4.3#
CC_VERBOSE= #--verbose#
CC_BASE=$(CC) $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH) $(CC_VERBOSE)#
COMPILE.c = $(LIBTOOL) --tag=CC --mode=compile $(CC_BASE) -c #
CFLAGS_SHARED=-shared -export-dynamic # 
CFLAGS += $(CFLAGS_SHARED) #
LIB_FLAGS += -rpath $(libdir) #

CFLAGS_DEBUG = -g #
CFLAGS_OPTIMIZE = -O2
CFLAGS_OPTIMIZE = -O3
ifneq "$(TORT_NO_OPTIMIZE)" ""
CFLAGS_OPTIMIZE = #
endif
CPPFLAGS += -DTORT_DLIB_DIR='"$(libdir)"' #
CPPFLAGS += -DTORT_GC_BDW=$(TORT_GC_BDW) #
CPPFLAGS += -DTORT_GC_SMAL=$(TORT_GC_SMAL) #
CPPFLAGS += $(CFLAGS_INC) -Iinclude -I$(BASE_DIR)/core/include -I$(BASE_DIR)/core/boot/include $(INCS) 
ifeq "$(UNAME_S)" "Linux"
LIBS += -ldl
else
# CFLAGS_OPTIMIZE = -fast
# FIXME: -fnested-functions not supported on linux gcc 4.4.5
CFLAGS += -fnested-functions
endif
CFLAGS += $(CPPFLAGS) -Wall -Werror $(CFLAGS_DEBUG) $(CFLAGS_OPTIMIZE)

ifneq "$(TORT_GC_BDW)" "0"
LDFLAGS += -L$(PREFIX)/lib #
INCS += -I$(GC_BDW)/include 
LIBS += -lgc #
endif

SMAL=$(BASE_DIR)/../smal#
ifneq "$(TORT_GC_SMAL)" "0"
LDFLAGS += -L$(SMAL)/lib
INCS += -I$(SMAL)/include
LIBS += -lsmal
LINK_DEPS += $(SMAL)/lib/libsmal.a
endif

LIB_TORT = $(BASE_DIR)/core/src/libtortcore.la

GEN_FILES = 
GEN_LIBS = 
GEN_BINS =

BIN_LIBS = $(TEST_LIBS) #
