#(C)2004-2005 AMX Mod X Development Team
# Makefile written by David "BAILOPAN" Anderson

HLSDK = ../../hlsdk/SourceCode
MM_ROOT = ../../metamod/metamod

### EDIT BELOW FOR OTHER PROJECTS ###

OPT_FLAGS = -O2 -funroll-loops -s -pipe -fomit-frame-pointer
DEBUG_FLAGS = -g -ggdb3
CPP = gcc
NAME = mysqlx_amxx
MYSQL_DIR = m_inc

OBJECTS = basic_sql.cpp handles.cpp module.cpp threading.cpp sdk/amxxmodule.cpp
OBJECTS += thread/BaseWorker.cpp thread/ThreadWorker.cpp thread/PosixThreads.cpp
OBJECTS += mysql/MysqlQuery.cpp mysql/MysqlResultSet.cpp mysql/MysqlDatabase.cpp mysql/MysqlDriver.cpp 

LINK = -lmysqlclient -lz -lpthread

INCLUDE = -I. -I$(HLSDK) -I$(HLSDK)/dlls -I$(HLSDK)/engine -I$(HLSDK)/game_shared -I$(HLSDK)/game_shared \
	-I$(MM_ROOT) -Lzlib -I$(HLSDK)/common -I$(MYSQL_DIR)/include -L$(MYSQL_DIR)/lib -Ithread \
	-Imysql -Isdk

ifeq "$(DEBUG)" "true"
	BIN_DIR = Debug
	CFLAGS = $(DEBUG_FLAGS)
else
	BIN_DIR = Release
	CFLAGS = $(OPT_FLAGS)
endif

CFLAGS += -DNDEBUG -fPIC -Wno-deprecated -fno-exceptions -DHAVE_STDINT_H -Dstricmp=strcasecmp -fno-rtti -static-libgcc
CFLAGS += -DSM_DEFAULT_THREADER

ifeq "$(AMD64)" "true"
	BINARY = $(NAME)_amd64.so
	CFLAGS += -DPAWN_CELL_SIZE=64 -DHAVE_I64 -m64 
	INCLUDE += -Lextra/lib_linux64
	LINK += /lib/libgcc_eh.a /lib/libstdc++.a
else
	BINARY = $(NAME)_i386.so
	CFLAGS += -DPAWN_CELL_SIZE=32 -DJIT -DASM32
	OPT_FLAGS += -march=i686
	INCLUDE += -Lextra/lib_linux
	LINK += /lib/libgcc_eh.a /lib/libstdc++.a
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
	rm -rf Release/*.o
	rm -rf Release/$(BINARY)
	rm -rf Debug/*.o
	rm -rf Debug/$(BINARY)

