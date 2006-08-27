#(C)2004-2005 AMX Mod X Development Team
# Makefile written by David "BAILOPAN" Anderson

HLSDK = ../hlsdk/SourceCode
MM_ROOT = ../metamod/metamod

### EDIT BELOW FOR OTHER PROJECTS ###

OPT_FLAGS = -O2 -fno-strict-aliasing -funroll-loops -s -fomit-frame-pointer -pipe
DEBUG_FLAGS = -g -ggdb3
CPP = gcc-4.1
NAME = amxmodx

BIN_SUFFIX_32 = mm_i386.so
BIN_SUFFIX_64 = mm_amd64.so

OBJECTS = meta_api.cpp CFile.cpp CVault.cpp vault.cpp float.cpp file.cpp modules.cpp \
	CMisc.cpp CTask.cpp string.cpp amxmodx.cpp CEvent.cpp CCmd.cpp CLogEvent.cpp \
	srvcmd.cpp strptime.cpp amxcore.cpp amxtime.cpp power.cpp amxxlog.cpp fakemeta.cpp \
	amxxfile.cpp CLang.cpp md5.cpp emsg.cpp CForward.cpp CPlugin.cpp CModule.cpp \
	CMenu.cpp util.cpp amx.cpp amxdbg.cpp natives.cpp newmenus.cpp debugger.cpp \
	optimizer.cpp format.cpp messages.cpp libraries.cpp vector.cpp sorting.cpp

LINK = /lib/libstdc++.a

INCLUDE = -I. -I$(HLSDK) -I$(HLSDK)/dlls -I$(HLSDK)/engine -I$(HLSDK)/game_shared -I$(HLSDK)/game_shared \
	-I$(MM_ROOT) -Lzlib -I$(HLSDK)/common

GCC_VERSION := $(shell $(CPP) -dumpversion >&1 | cut -b1)

ifeq "$(GCC_VERSION)" "4"
	OPT_FLAGS += -fvisibility=hidden -fvisibility-inlines-hidden
endif

ifeq "$(DEBUG)" "true"
	BIN_DIR = Debug
	CFLAGS = $(DEBUG_FLAGS)
else
	BIN_DIR = Release
	CFLAGS = $(OPT_FLAGS)
endif

ifeq "$(BINLOG)" "true"
	NAME := $(NAME)_bl
	BIN_DIR := $(BIN_DIR)BinLog
	OBJECTS += binlog.cpp
	CFLAGS += -DBINLOG_ENABLED
endif

CFLAGS += -DLINUX -DNDEBUG -DAMX_NOPROPLIST -fPIC -Wall -Werror -DHAVE_STDINT_H -static-libgcc -fno-rtti -fno-exceptions

ifeq "$(AMD64)" "true"
	BINARY = $(NAME)_$(BIN_SUFFIX_64)
	CFLAGS += -DPAWN_CELL_SIZE=64 -DHAVE_I64 -DAMD64 -m64
	LINK += -lz64
	OBJECTS += JIT/natives-amd64.o
else
	BINARY = $(NAME)_$(BIN_SUFFIX_32)
	OBJECTS += JIT/amxexecn.o JIT/amxjitsn.o JIT/natives-x86.o
	OBJECTS += JIT/helpers-x86.o
	CFLAGS += -DPAWN_CELL_SIZE=32 -DJIT -DASM32
	LINK += -lz
	OPT_FLAGS += -march=i586
endif

OBJ_LINUX := $(OBJECTS:%.cpp=$(BIN_DIR)/%.o)

$(BIN_DIR)/%.o: %.cpp
	$(CPP) $(INCLUDE) $(CFLAGS) -o $@ -c $<

all:
	mkdir -p $(BIN_DIR)
	$(MAKE) amxmodx

amd64:
	rm -f zlib/libz.a
	$(MAKE) all AMD64=true

amd64_debug:
	rm -f zlib/libz.a
	$(MAKE) all AMD64=true DEBUG=true

amd64_binlog:
	rm -f zlib/libz.a
	$(MAKE) all AMD64=true BINLOG=true

amd64_binlog_debug:
	rm -f zlib/libz.a
	$(MAKE) all AMD64=true BINLOG=true DEBUG=true
	
binlog:
	$(MAKE) all BINLOG=true
	
binlog_debug:
	$(MAKE) all BINLOG=true DEBUG=true

amxmodx: $(OBJ_LINUX)
	$(CPP) $(INCLUDE) $(CFLAGS) $(OBJ_LINUX) $(LINK) -shared -ldl -lm -o$(BIN_DIR)/$(BINARY)

debug:	
	$(MAKE) all DEBUG=true

default: all

clean:
	rm -rf Release/*.o
	rm -rf Release/$(NAME)_$(BIN_SUFFIX_32)
	rm -rf Release/$(NAME)_$(BIN_SUFFIX_64)
	rm -rf ReleaseBinLog/*.o
	rm -rf ReleaseBinLog/$(NAME)_bl_$(BIN_SUFFIX_32)
	rm -rf ReleaseBinLog/$(NAME)_bl_$(BIN_SUFFIX_64)
	rm -rf Debug/*.o
	rm -rf Debug/$(NAME)_$(BIN_SUFFIX_32)
	rm -rf Debug/$(NAME)_$(BIN_SUFFIX_64)
	rm -rf DebugBinLog/*.o
	rm -rf DebugBinLog/$(NAME)_bl_$(BIN_SUFFIX_32)
	rm -rf DebugBinLog/$(NAME)_bl_$(BIN_SUFFIX_64)
