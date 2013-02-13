# (C)2004-2013 AMX Mod X Development Team
# Makefile written by David "BAILOPAN" Anderson

###########################################
### EDIT THESE PATHS FOR YOUR OWN SETUP ###
###########################################

HLSDK = ../../../hlsdk/multiplayer
MM_ROOT = ../../../metamod/metamod
SQL = sqlite-source

#####################################
### EDIT BELOW FOR OTHER PROJECTS ###
#####################################

PROJECT = sqlite

OBJECTS = basic_sql.cpp handles.cpp module.cpp threading.cpp sdk/amxxmodule.cpp oldcompat_sql.cpp \
          thread/BaseWorker.cpp thread/ThreadWorker.cpp thread/PosixThreads.cpp \
	  sqlitepp/SqliteQuery.cpp sqlitepp/SqliteResultSet.cpp sqlitepp/SqliteDatabase.cpp \
	  sqlitepp/SqliteDriver.cpp
OBJECTS += $(SQL)/alter.c $(SQL)/analyze.c $(SQL)/attach.c $(SQL)/auth.c $(SQL)/btree.c \
           $(SQL)/build.c $(SQL)/callback.c $(SQL)/complete.c $(SQL)/date.c $(SQL)/delete.c \
	   $(SQL)/expr.c $(SQL)/func.c $(SQL)/hash.c $(SQL)/insert.c $(SQL)/legacy.c \
	   $(SQL)/loadext.c $(SQL)/main.c $(SQL)/opcodes.c $(SQL)/os.c $(SQL)/os_unix.c \
	   $(SQL)/pager.c $(SQL)/parse.c $(SQL)/pragma.c $(SQL)/prepare.c $(SQL)/printf.c \
	   $(SQL)/random.c $(SQL)/select.c $(SQL)/table.c $(SQL)/tokenize.c $(SQL)/trigger.c \
           $(SQL)/update.c $(SQL)/utf.c $(SQL)/util.c $(SQL)/vacuum.c $(SQL)/vdbe.c $(SQL)/vdbeapi.c \
	   $(SQL)/vdbeaux.c $(SQL)/vdbefifo.c $(SQL)/vdbemem.c $(SQL)/vtab.c $(SQL)/where.c

##############################################
### CONFIGURE ANY OTHER FLAGS/OPTIONS HERE ###
##############################################

C_OPT_FLAGS = -DNDEBUG -O3 -funroll-loops -fomit-frame-pointer -pipe
C_DEBUG_FLAGS = -D_DEBUG -DDEBUG -g -ggdb3
C_GCC4_FLAGS = -fvisibility=hidden
CPP_GCC4_FLAGS = -fvisibility-inlines-hidden
CPP = gcc
CPP_OSX = clang

LINK =

INCLUDE = -I. -I$(HLSDK) -I$(HLSDK)/common -I$(HLSDK)/dlls -I$(HLSDK)/engine -I$(HLSDK)/game_shared \
          -I$(MM_ROOT) -I$(SQL) -Isqlitepp -Ithread -Isdk

################################################
### DO NOT EDIT BELOW HERE FOR MOST PROJECTS ###
################################################

OS := $(shell uname -s)

ifeq "$(OS)" "Darwin"
	CPP = $(CPP_OSX)
	LIB_EXT = dylib
	LIB_SUFFIX = _amxx
	CFLAGS += -DOSX
	LINK += -dynamiclib -lstdc++ -mmacosx-version-min=10.5
else
	LIB_EXT = so
	LIB_SUFFIX = _amxx_i386
	CFLAGS += -DLINUX
	LINK += -shared
endif

LINK += -m32 -lm -ldl -lpthread

CFLAGS += -DSM_DEFAULT_THREADER -DPAWN_CELL_SIZE=32 -DJIT -DASM32 -DHAVE_STDINT_H \
	  -Dstricmp=strcasecmp -fno-strict-aliasing -m32 -Wall -Werror
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
OBJ_BIN := $(OBJ_BIN:%.c=$(BIN_DIR)/%.o)

# This will break if we include other Makefiles, but is fine for now. It allows
#  us to make a copy of this file that uses altered paths (ie. Makefile.mine)
#  or other changes without mucking up the original.
MAKEFILE_NAME := $(CURDIR)/$(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))

$(BIN_DIR)/%.o: %.cpp
	$(CPP) $(INCLUDE) $(CFLAGS) $(CPPFLAGS) -o $@ -c $<

$(BIN_DIR)/%.o: %.c
	$(CPP) $(INCLUDE) $(CFLAGS) -o $@ -c $<

all:
	mkdir -p $(BIN_DIR)
	mkdir -p $(BIN_DIR)/sdk
	mkdir -p $(BIN_DIR)/thread
	mkdir -p $(BIN_DIR)/sqlitepp
	mkdir -p $(BIN_DIR)/$(SQL)
	$(MAKE) -f $(MAKEFILE_NAME) $(PROJECT)

$(PROJECT): $(OBJ_BIN)
	$(CPP) $(INCLUDE) $(OBJ_BIN) $(LINK) -o $(BIN_DIR)/$(BINARY)

debug:
	$(MAKE) -f $(MAKEFILE_NAME) all DEBUG=true

default: all

clean:
	rm -rf $(BIN_DIR)/*.o
	rm -rf $(BIN_DIR)/sdk/*.o
	rm -rf $(BIN_DIR)/thread/*.o
	rm -rf $(BIN_DIR)/sqlitepp/*.o
	rm -rf $(BIN_DIR)/$(SQL)/*.o
	rm -f $(BIN_DIR)/$(BINARY)

