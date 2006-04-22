#(C)2004-2005 AMX Mod X Development Team
# Makefile written by David "BAILOPAN" Anderson

### EDIT BELOW FOR OTHER PROJECTS ###

OPT_FLAGS = -funroll-loops -s -pipe -fomit-frame-pointer-fexceptions
DEBUG_FLAGS = -g -ggdb3
CPP = gcc
NAME = array_amxx

OBJECTS = amxxmodule.cpp Array.cpp BinTrieNatives.cpp Capsule.cpp CArray.cpp CBinTrie.cpp CKeytable.cpp JudyExtra.cpp ListNatives.cpp MapNatives.cpp

LINK = Judy.a libstdc++.a

ifeq "$(DEBUG)" "true"
	BIN_DIR = Debug
	CFLAGS = $(DEBUG_FLAGS)
else
	BIN_DIR = Release
	CFLAGS = $(OPT_FLAGS)
endif

CFLAGS += -DNDEBUG -fPIC -Wno-deprecated -DHAVE_STDINT_H -fexceptions -fno-rtti

ifeq "$(AMD64)" "true"
	BINARY = $(NAME)_amd64.so
	CFLAGS += -DPAWN_CELL_SIZE=64 -DHAVE_I64 -m64 
else
	BINARY = $(NAME)_i386.so
	CFLAGS += -DPAWN_CELL_SIZE=32 -DJIT -DASM32
	OPT_FLAGS += -march=i586
endif

OBJ_LINUX := $(OBJECTS:%.cpp=$(BIN_DIR)/%.o)

$(BIN_DIR)/%.o: %.cpp
	$(CPP) $(INCLUDE) $(CFLAGS) -o $@ -c $<

all:
	mkdir -p $(BIN_DIR)
	$(MAKE) engine

amd64:
	$(MAKE) all AMD64=true

engine: $(OBJ_LINUX)
	$(CPP) $(INCLUDE) $(CFLAGS) $(OBJ_LINUX) $(LINK) -shared -lm -o$(BIN_DIR)/$(BINARY)

debug:	
	$(MAKE) all DEBUG=true

default: all

clean:
	rm -rf Release/*.o
	rm -rf Release/$(BINARY)
	rm -rf Debug/*.o
	rm -rf Debug/$(BINARY)