#(C)2004-2005 AMX Mod X Development Team
# Makefile written by David "BAILOPAN" Anderson

HLSDK = ../../../hlsdk
MM_ROOT = ../../../metamod/metamod

### EDIT BELOW FOR OTHER PROJECTS ###


CRAZY_OPT_FLAGS = -DCRAZY_OPTS -O3 -funroll-loops -ffast-math -s -pipe -fomit-frame-pointer -fno-strict-aliasing -DNDEBUG -fmerge-all-constants -fmodulo-sched -fgcse-sm -fgcse-las -fgcse-after-reload -floop-optimize2 -funsafe-loop-optimizations -ftree-loop-linear -ftree-loop-im -ftree-loop-ivcanon -fivopts -ftree-vectorize -fvariable-expansion-in-unroller -funsafe-math-optimizations -ffinite-math-only  -fpeel-loops -funswitch-loops -fvisibility=hidden -fvisibility-inlines-hidden -Wall -Wno-unknown-pragmas -Wno-deprecated -fno-exceptions -DHAVE_STDINT_H -static-libgcc -fno-rtti -Wpointer-arith -Wcast-qual -Wcast-align -Wconversion -Wsign-compare -Wmissing-noreturn -Winline -Wlong-long -Wunsafe-loop-optimizations -Wctor-dtor-privacy -Wno-non-virtual-dtor -Wreorder -Woverloaded-virtual -Wsign-promo -Wsynth -shared 

CRAZY_LINK_FLAGS = 
#-fwhole-program
#-fwhole-program -combine

SANE_OPT_FLAGS = -O3 -funroll-loops -s -pipe -fomit-frame-pointer -fno-strict-aliasing -DNDEBUG

OPT_FLAGS =

DEBUG_FLAGS = -g -ggdb3
CPP = gcc-4.1
NAME = ns

BIN_SUFFIX = amxx_i386.so

OBJECTS = sdk/amxxmodule.cpp dllapi.cpp utils.cpp amxxapi.cpp engineapi.cpp \
TitleManager.cpp ParticleManager.cpp MessageHandler.cpp GameManager.cpp \
natives/general.cpp natives/player.cpp natives/player_memory.cpp \
natives/structure.cpp natives/weapons.cpp natives/particles.cpp \
natives/memberfuncs.cpp

LINK = 

INCLUDE = -I. -I$(HLSDK) -I$(HLSDK)/dlls -I$(HLSDK)/engine -I$(HLSDK)/game_shared -I$(HLSDK)/game_shared \
	-I$(MM_ROOT) -I$(HLSDK)/common -I$(HLSDK)/pm_shared

GCC_VERSION := $(shell $(CPP) -dumpversion >&1 | cut -b1)

ifeq "$(DEBUG)" "true"
	BIN_DIR = Debug
	CFLAGS = $(DEBUG_FLAGS)
else
	ifeq "$(CRAZY)" "true"
		BIN_DIR = Optimized
		OPT_FLAGS = $(CRAZY_OPT_FLAGS)
		LINK = $(CRAZY_LINK_FLAGS)
	else
		BIN_DIR = Release
		OPT_FLAGS = $(SANE_OPT_FLAGS)
	endif
	ifeq "$(GCC_VERSION)" "4"
		OPT_FLAGS += -fvisibility=hidden -fvisibility-inlines-hidden
	endif
	CFLAGS = $(OPT_FLAGS)
endif

CFLAGS += -Wall -Wno-non-virtual-dtor -Werror -fno-exceptions -DHAVE_STDINT_H -static-libgcc -fno-rtti -m32

BINARY = $(NAME)_$(BIN_SUFFIX)
CFLAGS += -DPAWN_CELL_SIZE=32 -DJIT -DASM32
OPT_FLAGS += -march=i586

OBJ_LINUX := $(OBJECTS:%.cpp=$(BIN_DIR)/%.o)

$(BIN_DIR)/%.o: %.cpp
	$(CPP) $(INCLUDE) $(CFLAGS) -o $@ -c $<

all:
	mkdir -p $(BIN_DIR)
	mkdir -p $(BIN_DIR)/sdk
	mkdir -p $(BIN_DIR)/natives
	$(MAKE) ns

ns: $(OBJ_LINUX)
	$(CPP) $(INCLUDE) $(CFLAGS) $(OBJ_LINUX) $(LINK) -shared -ldl -lm -o$(BIN_DIR)/$(BINARY)

debug:	
	$(MAKE) all DEBUG=true

default: all

crazy:
	$(MAKE) all CRAZY=true

clean:
	rm -rf Release/*.o
	rm -rf Release/sdk/*.o
	rm -rf Release/natives/*.o
	rm -rf Release/$(NAME)_$(BIN_SUFFIX)
	rm -rf Debug/*.o
	rm -rf Debug/sdk/*.o
	rm -rf Debug/natives/*.o
	rm -rf Debug/$(NAME)_$(BIN_SUFFIX)
	rm -rf Optimized/*.o
	rm -rf Optimized/sdk/*.o
	rm -rf Optimized/natives/*.o
	rm -rf Optimized/$(NAME)_$(BIN_SUFFIX)
	
