#(C)2004-2005 AMX Mod X Development Team
# Makefile written by David "BAILOPAN" Anderson

HLSDK = ../../../hlsdk
MM_ROOT = ../../metamod/metamod

### EDIT BELOW FOR OTHER PROJECTS ###

OPT_FLAGS = -O2 -funroll-loops -s -pipe -fomit-frame-pointer -fno-strict-aliasing
DEBUG_FLAGS = -g -ggdb3
CPP = gcc-4.1
NAME = engine

BIN_SUFFIX_32 = amxx_i386.so
BIN_SUFFIX_64 = amxx_amd64.so

OBJECTS = sdk/amxxmodule.cpp amxxapi.cpp engine.cpp entity.cpp globals.cpp forwards.cpp \
	amxmod_compat.cpp

LINK = 

INCLUDE = -I. -I$(HLSDK) -I$(HLSDK)/dlls -I$(HLSDK)/engine -I$(HLSDK)/game_shared -I$(HLSDK)/game_shared \
	-I$(MM_ROOT) -I$(HLSDK)/common -Isdk

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

CFLAGS += -DNDEBUG -fPIC -Wall -Wno-non-virtual-dtor -Werror -fno-exceptions -DHAVE_STDINT_H -fno-rtti -static-libgcc

ifeq "$(AMD64)" "true"
	BINARY = $(NAME)_$(BIN_SUFFIX_64)
	CFLAGS += -DPAWN_CELL_SIZE=64 -DHAVE_I64 -m64 
else
	BINARY = $(NAME)_$(BIN_SUFFIX_32)
	CFLAGS += -DPAWN_CELL_SIZE=32 -DJIT -DASM32
	OPT_FLAGS += -march=i586
endif

OBJ_LINUX := $(OBJECTS:%.cpp=$(BIN_DIR)/%.o)

$(BIN_DIR)/%.o: %.cpp
	$(CPP) $(INCLUDE) $(CFLAGS) -o $@ -c $<

all:
	mkdir -p $(BIN_DIR)
	mkdir -p $(BIN_DIR)/sdk
	$(MAKE) engine

amd64:
	$(MAKE) all AMD64=true

engine: $(OBJ_LINUX)
	$(CPP) $(INCLUDE) $(CFLAGS) $(OBJ_LINUX) $(LINK) -shared -ldl -lm -o$(BIN_DIR)/$(BINARY)

debug:	
	$(MAKE) all DEBUG=true

default: all

clean:
	rm -rf Release/sdk/*.o
	rm -rf Release/*.o
	rm -rf Release/$(NAME)_$(BIN_SUFFIX_32)
	rm -rf Release/$(NAME)_$(BIN_SUFFIX_64)
	rm -rf Debug/sdk/*.o
	rm -rf Debug/*.o
	rm -rf Debug/$(NAME)_$(BIN_SUFFIX_32)
	rm -rf Debug/$(NAME)_$(BIN_SUFFIX_64)
