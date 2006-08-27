#(C)2004-2005 AMX Mod X Development Team
# Makefile written by David "BAILOPAN" Anderson

HLSDK = ../../hlsdk/SourceCode
MM_ROOT = ../../metamod/metamod

### EDIT BELOW FOR OTHER PROJECTS ###

OPT_FLAGS = -O2 -funroll-loops -s -pipe -fomit-frame-pointer -fno-strict-aliasing
DEBUG_FLAGS = -g -ggdb3
CPP = gcc-4.1
NAME = mysql
#MYSQL_DIR = ../../../mysql-5.0.22
MYSQL_DIR = ../../../mysql-5.0

BIN_SUFFIX_32 = amxx_i386.so
BIN_SUFFIX_64 = amxx_amd64.so

OBJECTS = basic_sql.cpp handles.cpp module.cpp threading.cpp sdk/amxxmodule.cpp oldcompat_sql.cpp
OBJECTS += thread/BaseWorker.cpp thread/ThreadWorker.cpp thread/PosixThreads.cpp
OBJECTS += mysql/MysqlQuery.cpp mysql/MysqlResultSet.cpp mysql/MysqlDatabase.cpp mysql/MysqlDriver.cpp 

LINK = -static-libgcc -lmysqlclient_r -lz -lpthread /lib/libgcc_eh.a /lib/libstdc++.a

INCLUDE = -I. -I$(HLSDK) -I$(HLSDK)/dlls -I$(HLSDK)/engine -I$(HLSDK)/game_shared -I$(HLSDK)/game_shared \
	-I$(MM_ROOT) -I$(HLSDK)/common -I$(MYSQL_DIR)/include -L$(MYSQL_DIR)/lib -Ithread \
	-Imysql -Isdk

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

CFLAGS += -DNDEBUG -fPIC -Wall -Wno-non-virtual-dtor -Werror -fno-exceptions -DHAVE_STDINT_H -Dstricmp=strcasecmp -fno-rtti -static-libgcc
CFLAGS += -DSM_DEFAULT_THREADER

ifeq "$(AMD64)" "true"
	BINARY = $(NAME)_$(BIN_SUFFIX_64)
	CFLAGS += -DPAWN_CELL_SIZE=64 -DHAVE_I64 -m64 
	INCLUDE += -Lextra/lib_linux64
else
	BINARY = $(NAME)_$(BIN_SUFFIX_32)
	CFLAGS += -DPAWN_CELL_SIZE=32 -DJIT -DASM32
	OPT_FLAGS += -march=i686
	INCLUDE += -Lextra/lib_linux
endif

OBJ_LINUX := $(OBJECTS:%.cpp=$(BIN_DIR)/%.o)

$(BIN_DIR)/%.o: %.cpp
	$(CPP) $(INCLUDE) $(CFLAGS) -o $@ -c $<

all:
	mkdir -p $(BIN_DIR)/thread
	mkdir -p $(BIN_DIR)/mysql
	mkdir -p $(BIN_DIR)/sdk
	mkdir -p $(BIN_DIR)
	$(MAKE) mysql

amd64:
	$(MAKE) all AMD64=true

mysql: $(OBJ_LINUX)
	$(CPP) $(INCLUDE) $(CFLAGS) $(OBJ_LINUX) $(LINK) -shared -ldl -lm -o$(BIN_DIR)/$(BINARY)

debug:	
	$(MAKE) all DEBUG=true

default: all

clean:
	rm -rf Release/thread/*.o
	rm -rf Release/mysql/*.o
	rm -rf Release/sdk/*.o
	rm -rf Release/*.o
	rm -rf Release/$(NAME)_$(BIN_SUFFIX_32)
	rm -rf Release/$(NAME)_$(BIN_SUFFIX_64)
	rm -rf Debug/thread/*.o
	rm -rf Debug/mysql/*.o
	rm -rf Debug/sdk/*.o
	rm -rf Debug/*.o
	rm -rf Debug/$(NAME)_$(BIN_SUFFIX_32)
	rm -rf Debug/$(NAME)_$(BIN_SUFFIX_64)
