BASE_DIR:=$(shell cd "$(BASE_DIR)" && /bin/pwd)#
GC_VERSION=gc-20101223-cvs#
GC_VERSION=gc-7.2alpha6#
GC=$(BASE_DIR)/$(GC_VERSION)#
TORT_GC=1

PREFIX:=$(shell mkdir -p $(BASE_DIR)/local && cd $(BASE_DIR)/local && /bin/pwd)#
libdir=$(PREFIX)/lib#
export LD_LIBRARY_PATH
LD_LIBRARY_PATH:=$(libdir):$(LD_LIBRARY_PATH)

LIBTOOL=$(GC)/libtool #
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
#CFLAGS_OPTIMIZE = 
CFLAGS += -DTORT_DLIB_DIR='"$(libdir)"' #
CFLAGS += -DTORT_GC=$(TORT_GC) #
CFLAGS += -fnested-functions #
CFLAGS += $(CFLAGS_INC) -Iinclude -I$(BASE_DIR)/core/include -I$(BASE_DIR)/core/boot/include -I$(GC)/include -Wall -Werror $(CFLAGS_OPTIMIZE)

ifeq "$(TORT_GC)" "0"
else
LDFLAGS += -L$(PREFIX)/lib #
LIBS += -lgc #
endif
SMAL=$(BASE_DIR)/../smal#
CFLAGS += -I$(SMAL)/include
LDFLAGS += -L$(SMAL)/src
LIBS += -lsmal
LINK_DEPS += $(SMAL)/src/libsmal.a

LIB_TORT = $(BASE_DIR)/core/src/libtortcore.la

GEN_FILES = 
GEN_LIBS = 
GEN_BINS =

BIN_LIBS = $(TEST_LIBS) #
