# (C)2004-2013 AMX Mod X Development Team
# Makefile written by David "BAILOPAN" Anderson

#####################################
### EDIT BELOW FOR OTHER PROJECTS ###
#####################################

PROJECT = amxxpc
OBJECTS = amx.cpp amxxpc.cpp Binary.cpp

##############################################
### CONFIGURE ANY OTHER FLAGS/OPTIONS HERE ###
##############################################

C_OPT_FLAGS = -DNDEBUG -O2 -funroll-loops -fomit-frame-pointer -pipe
C_DEBUG_FLAGS = -D_DEBUG -DDEBUG -g -ggdb3
C_GCC4_FLAGS = -fvisibility=hidden
CPP_GCC4_FLAGS = -fvisibility-inlines-hidden
CPP = gcc
CPP_OSX = clang

LINK =

INCLUDE = -I.

################################################
### DO NOT EDIT BELOW HERE FOR MOST PROJECTS ###
################################################

OS := $(shell uname -s)

ifeq "$(OS)" "Darwin"
	CPP = $(CPP_OSX)
	LIB_SUFFIX = _osx
	CFLAGS += -DOSX
	LINK += -lstdc++ -mmacosx-version-min=10.5 -lz-darwin
else
	LIB_SUFFIX =
	CFLAGS += -DLINUX
	LINK += -lz /lib32/libstdc++.a
endif

LINK += -m32 -lm -ldl -L.

CFLAGS += -DAMX_ANSIONLY -DHAVE_STDINT_H -fno-strict-aliasing \
	  -m32 -Wall -Werror
CPPFLAGS += -fexceptions -fno-rtti

BINARY = $(PROJECT)$(LIB_SUFFIX)

ifeq "$(DEBUG)" "true"
	BIN_DIR = Debug
	CFLAGS += $(C_DEBUG_FLAGS)
else
	BIN_DIR = Release
	CFLAGS += $(C_OPT_FLAGS)
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

$(PROJECT): $(OBJ_BIN)
	$(CPP) $(INCLUDE) $(OBJ_BIN) $(LINK) -o $(BIN_DIR)/$(BINARY)

default: all

clean:
	rm -rf $(BIN_DIR)/*.o
	rm -f $(BIN_DIR)/$(BINARY)

