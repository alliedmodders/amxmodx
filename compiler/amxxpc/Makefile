#(C)2004-2005 AMX Mod X Development Team
# Makefile written by David "BAILOPAN" Anderson

### EDIT BELOW FOR OTHER PROJECTS ###

OPT_FLAGS = -O3 -fno-rtti -funroll-loops -s -pipe
DEBUG_FLAGS = -g -ggdb3
CPP = gcc
BINARY = amxxpc

OBJECTS = amx.cpp amxxpc.cpp

LINK = -lz

INCLUDE = -I.  -L.

ifeq "$(DEBUG)" "true"
	BIN_DIR = Debug
	CFLAGS = $(DEBUG_FLAGS)
else
	BIN_DIR = Release
	CFLAGS = $(OPT_FLAGS)
endif

CFLAGS += -DLINUX -DNDEBUG -Wno-deprecated -fno-exceptions -DHAVE_STDINT_H -DAMX_ANSIONLY

OBJ_LINUX := $(OBJECTS:%.cpp=$(BIN_DIR)/%.o)

$(BIN_DIR)/%.o: %.cpp
	$(CPP) $(INCLUDE) $(CFLAGS) -o $@ -c $<

all:
	mkdir -p $(BIN_DIR)
	$(MAKE) amxxpc

amxxpc: $(OBJ_LINUX)
	$(CPP) $(INCLUDE) $(CFLAGS) $(OBJ_LINUX) $(LINK) -ldl -lm -o$(BIN_DIR)/$(BINARY)

default: all

clean:
	rm -rf Release/*.o
	rm -rf Release/$(BINARY)
	rm -rf Debug/*.o
	rm -rf Debug/$(BINARY)

