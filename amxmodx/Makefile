#(C)2004-2005 AMX Mod X Development Team
# Makefile written by David "BAILOPAN" Anderson

HLSDK = ../hlsdk/SourceCode
MM_ROOT = ../metamod/metamod

### EDIT BELOW FOR OTHER PROJECTS ###

OPT_FLAGS = -O3 -fno-rtti -funroll-loops -s -pipe
DEBUG_FLAGS = -g -ggdb3
CPP = gcc
NAME = amxmodx_mm

OBJECTS = meta_api.cpp CFile.cpp CVault.cpp vault.cpp float.cpp file.cpp modules.cpp \
	CMisc.cpp CTask.cpp string.cpp amxmodx.cpp CEvent.cpp CCmd.cpp CLogEvent.cpp \
	srvcmd.cpp strptime.cpp amxcore.cpp amxtime.cpp power.cpp amxxlog.cpp fakemeta.cpp \
	amxxfile.cpp CLang.cpp md5.cpp emsg.cpp CForward.cpp CPlugin.cpp CModule.cpp \
	CMenu.cpp util.cpp amx.cpp

LINK = -lz JIT/amxexecn.o JIT/amxjitsn.o

INCLUDE = -I. -I$(HLSDK) -I$(HLSDK)/dlls -I$(HLSDK)/engine -I$(HLSDK)/game_shared -I$(HLSDK)/game_shared \
	-I$(MM_ROOT) -Lzlib -I$(HLSDK)/common

ifeq "$(DEBUG)" "true"
	BIN_DIR = Debug
	CFLAGS = $(DEBUG_FLAGS)
else
	BIN_DIR = Release
	CFLAGS = $(OPT_FLAGS)
endif

ifeq "$(MMGR)" "true"
	OBJECTS += MMGR/MMGR.CPP
endif

CFLAGS += -DLINUX -DNDEBUG -fPIC -Wno-deprecated -fno-exceptions -DHAVE_STDINT_H

ifeq "$(AMD64)" "true"
	BINARY = $(NAME)_amd64.so
	CFLAGS += -DPAWN_CELL_SIZE=64 -DHAVE_I64 -m64
else
	BINARY = $(NAME)_i386.so
	CFLAGS += -DPAWN_CELL_SIZE=32 
	OPT_FLAGS += -march=i686
endif

OBJ_LINUX := $(OBJECTS:%.cpp=$(BIN_DIR)/%.o)

$(BIN_DIR)/%.o: %.cpp
	$(CPP) $(INCLUDE) $(CFLAGS) -o $@ -c $<

all:
	mkdir -p $(BIN_DIR)
	$(MAKE) amxmodx

amd64:
	$(MAKE) all AMD64=true

amd64_mmgr:
	$(MAKE) all AMD64=true MMGR=true

amd64_debug_mmgr:
	$(MAKE) all AMD64=true DEBUG=true MMGR=true

amd64_debug:
	$(MAKE) all AMD64=true DEBUG=true

mmgr:
	$(MAKE) all MMGR=true

debug_mmgr:
	$(MAKE) all MMGR=true DEBUG=true

amxmodx: $(OBJ_LINUX)
	$(CPP) $(INCLUDE) $(CFLAGS) $(OBJ_LINUX) $(LINK) -shared -ldl -lm -o$(BIN_DIR)/$(BINARY)

debug:	
	$(MAKE) all DEBUG=true

default: all

clean:
	rm -rf Release/*.o
	rm -rf Release/$(BINARY)
	rm -rf Debug/*.o
	rm -rf Debug/$(BINARY)

