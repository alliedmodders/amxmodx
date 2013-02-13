# (C)2004-2013 AMX Mod X Development Team
# Makefile written by David "BAILOPAN" Anderson

###########################################
### EDIT THESE PATHS FOR YOUR OWN SETUP ###
###########################################

HLSDK = ../../hlsdk/multiplayer
MM_ROOT = ../../metamod/metamod

#####################################
### EDIT BELOW FOR OTHER PROJECTS ###
#####################################

PROJECT = amxmodx

OBJECTS = meta_api.cpp CFile.cpp CVault.cpp vault.cpp float.cpp file.cpp modules.cpp \
	  CMisc.cpp CTask.cpp string.cpp amxmodx.cpp CEvent.cpp CCmd.cpp CLogEvent.cpp \
	  srvcmd.cpp strptime.cpp amxcore.cpp amxtime.cpp power.cpp amxxlog.cpp fakemeta.cpp \
	  amxxfile.cpp CLang.cpp md5.cpp emsg.cpp CForward.cpp CPlugin.cpp CModule.cpp \
	  CMenu.cpp util.cpp amx.cpp amxdbg.cpp natives.cpp newmenus.cpp debugger.cpp \
	  optimizer.cpp format.cpp messages.cpp libraries.cpp vector.cpp sorting.cpp \
	  amxmod_compat.cpp nongpl_matches.cpp CFlagManager.cpp datastructs.cpp \
	  trie_natives.cpp

##############################################
### CONFIGURE ANY OTHER FLAGS/OPTIONS HERE ###
##############################################

C_OPT_FLAGS = -DNDEBUG -O2 -funroll-loops -fomit-frame-pointer -pipe
C_DEBUG_FLAGS = -D_DEBUG -DDEBUG -g -ggdb3
C_GCC4_FLAGS = -fvisibility=hidden
CPP_GCC4_FLAGS = -fvisibility-inlines-hidden
CPP = gcc
CPP_OSX = clang

LINK = -Lzlib

INCLUDE = -I. -I$(HLSDK) -I$(HLSDK)/common -I$(HLSDK)/dlls -I$(HLSDK)/engine -I$(HLSDK)/game_shared \
	  -I$(MM_ROOT)

################################################
### DO NOT EDIT BELOW HERE FOR MOST PROJECTS ###
################################################

OS := $(shell uname -s)

ifeq "$(OS)" "Darwin"
	OBJECTS += JIT/amxexecn-darwin.o JIT/amxjitsn-darwin.o JIT/natives-darwin-x86.o \
		   JIT/helpers-darwin-x86.o

	CPP = $(CPP_OSX)
	LIB_EXT = dylib
	LIB_SUFFIX = _mm
	CFLAGS += -DOSX
	LINK += -dynamiclib -lstdc++ -mmacosx-version-min=10.5 -Wl,-read_only_relocs,suppress -lz-darwin
else
	OBJECTS += JIT/amxexecn.o JIT/amxjitsn.o JIT/natives-x86.o JIT/helpers-x86.o

	LIB_EXT = so
	LIB_SUFFIX = _mm_i386
	CFLAGS += -DLINUX
	LINK += -shared -lz
endif

LINK += -m32 -lm -ldl

CFLAGS += -DAMX_NOPROPLIST -DPAWN_CELL_SIZE=32 -DJIT -DASM32 -DHAVE_STDINT_H -fno-strict-aliasing \
	  -m32 -Wall -Werror
CPPFLAGS += -fno-exceptions -fno-rtti

BINARY = $(PROJECT)$(LIB_SUFFIX).$(LIB_EXT)

ifeq "$(DEBUG)" "true"
	BIN_DIR = Debug
	CFLAGS += $(C_DEBUG_FLAGS)
else
	BIN_DIR = Release
	CFLAGS += $(C_OPT_FLAGS)
	LINK += -s
endif

ifeq "$(BINLOG)" "true"
	LIB_SUFFIX := _bl$(LIB_SUFFIX)
	BIN_DIR := $(BIN_DIR)BinLog
	OBJECTS += binlog.cpp
	CFLAGS += -DBINLOG_ENABLED
endif

IS_CLANG := $(shell $(CPP) --version | head -1 | grep clang > /dev/null && echo "1" || echo "0")

ifeq "$(IS_CLANG)" "1"
	CPP_MAJOR := $(shell $(CPP) --version | grep clang | sed "s/.*version \([0-9]\)*\.[0-9]*.*/\1/")
	CPP_MINOR := $(shell $(CPP) --version | grep clang | sed "s/.*version [0-9]*\.\([0-9]\)*.*/\1/")
else
	CPP_MAJOR := $(shell $(CPP) -dumpversion >&1 | cut -b1)
	CPP_MINOR := $(shell $(CPP) -dumpversion >&1 | cut -b3)
endif

# Clang || GCC >= 4
ifeq "$(shell expr $(IS_CLANG) \| $(CPP_MAJOR) \>= 4)" "1"
	CFLAGS += $(C_GCC4_FLAGS)
	CPPFLAGS += $(CPP_GCC4_FLAGS)
endif

# Clang >= 3 || GCC >= 4.7
ifeq "$(shell expr $(IS_CLANG) \& $(CPP_MAJOR) \>= 3 \| $(CPP_MAJOR) \>= 4 \& $(CPP_MINOR) \>= 7)" "1"
	CFLAGS += -Wno-delete-non-virtual-dtor
endif

# OS is Linux and not using clang
ifeq "$(shell expr $(OS) \= Linux \& $(IS_CLANG) \= 0)" "1"
	LINK += -static-libgcc
endif

OBJ_BIN := $(OBJECTS:%.cpp=$(BIN_DIR)/%.o)

# This will break if we include other Makefiles, but is fine for now. It allows
#  us to make a copy of this file that uses altered paths (ie. Makefile.mine)
#  or other changes without mucking up the original.
MAKEFILE_NAME := $(CURDIR)/$(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))

$(BIN_DIR)/%.o: %.cpp
	$(CPP) $(INCLUDE) $(CFLAGS) $(CPPFLAGS) -o $@ -c $<

all:
	mkdir -p $(BIN_DIR)
	$(MAKE) -f $(MAKEFILE_NAME) $(PROJECT)

binlog:
	$(MAKE) -f $(MAKEFILE_NAME) all BINLOG=true

binlog_debug:
	$(MAKE) -f $(MAKEFILE_NAME) all BINLOG=true DEBUG=true

$(PROJECT): $(OBJ_BIN)
	$(CPP) $(INCLUDE) $(OBJ_BIN) $(LINK) -o $(BIN_DIR)/$(BINARY)

debug:
	$(MAKE) -f $(MAKEFILE_NAME) all DEBUG=true

default: all

clean:
	rm -rf $(BIN_DIR)/*.o
	rm -f $(BIN_DIR)/$(BINARY)

