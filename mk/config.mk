GC=$(BASE_DIR)gc-20101223-cvs
TORT_GC=1

PREFIX:=$(shell mkdir -p $(BASE_DIR)local && cd $(BASE_DIR)local && /bin/pwd)#
libdir=$(PREFIX)/lib#

LIBTOOL=$(GC)/libtool #
CC=gcc#
CC_BASE=$(CC) $(CFLAGS) $(CPPFLAGS) $(TARGET_ARCH)#
COMPILE.c = $(LIBTOOL) --mode=compile $(CC_BASE) -c #
CFLAGS += -shared -export-dynamic #
LIB_FLAGS += -rpath $(libdir) #

CFLAGS_OPTIMIZE = -O2
CFLAGS_OPTIMIZE = -O3
#CFLAGS_OPTIMIZE = 
CFLAGS += -DTORT_DLIB_DIR='"$(libdir)"' -DTORT_DLIB_SUFFIX='".dylib"' #
CFLAGS += -DTORT_GC=$(TORT_GC) -fnested-functions $(CFLAGS_INC) -Iinclude -I$(BASE_DIR)include -I$(BASE_DIR)boot/include -I$(GC)/include -Wall -Werror -g $(CFLAGS_OPTIMIZE)

ifeq "$(TORT_GC)" "0"
else
LDFLAGS += -L$(PREFIX)/lib #
LIBS += -lgc #
endif

LIB_TORT = $(BASE_DIR)src/libtort.la

