#(C)2004-2006 AMX Mod X Development Team
# Makefile written by David "BAILOPAN" Anderson

HLSDK = ../../hlsdk/SourceCode
MM_ROOT = ../../metamod/metamod

### EDIT BELOW FOR OTHER PROJECTS ###

OPT_FLAGS = -O3 -funroll-loops -s -pipe -fomit-frame-pointer
DEBUG_FLAGS = -g -ggdb3
CPP = gcc
CC = cc
NAME = sqlite
SQL = sqlite-source

BIN_SUFFIX_32 = amxx_i386.so
BIN_SUFFIX_64 = amxx_amd64.so

CPP_OBJECTS = basic_sql.cpp handles.cpp module.cpp threading.cpp sdk/amxxmodule.cpp oldcompat_sql.cpp
CPP_OBJECTS += thread/BaseWorker.cpp thread/ThreadWorker.cpp thread/PosixThreads.cpp
CPP_OBJECTS += sqlitepp/SqliteQuery.cpp sqlitepp/SqliteResultSet.cpp sqlitepp/SqliteDatabase.cpp sqlitepp/SqliteDriver.cpp

C_OBJECTS = $(SQL)/attach.c $(SQL)/auth.c $(SQL)/btree.c $(SQL)/build.c \
	$(SQL)/date.c $(SQL)/delete.c $(SQL)/func.c $(SQL)/hash.c \
	$(SQL)/insert.c $(SQL)/legacy.c $(SQL)/main.c $(SQL)/opcodes.c \
	$(SQL)/os.c $(SQL)/os_unix.c $(SQL)/pager.c $(SQL)/parse.c \
	$(SQL)/pragma.c $(SQL)/printf.c $(SQL)/random.c $(SQL)/select.c \
	$(SQL)/table.c $(SQL)/tokenize.c $(SQL)/trigger.c $(SQL)/update.c \
	$(SQL)/utf.c $(SQL)/util.c $(SQL)/vacuum.c $(SQL)/vdbe.c \
	$(SQL)/vdbeapi.c $(SQL)/vdbeaux.c $(SQL)/vdbemem.c $(SQL)/where.c \
	$(SQL)/prepare.c $(SQL)/expr.c $(SQL)/callback.c $(SQL)/alter.c \
	$(SQL)/vdbefifo.c $(SQL)/complete.c $(SQL)/analyze.c

LINK = -static-libgcc -lpthread /lib/libgcc_eh.a /lib/libstdc++.a

INCLUDE = -I. -I$(HLSDK) -I$(HLSDK)/dlls -I$(HLSDK)/engine -I$(HLSDK)/game_shared -I$(HLSDK)/game_shared \
	-I$(MM_ROOT) -I$(HLSDK)/common -I$(SQL) -I./sqlitepp -I./sdk -I./thread

ifeq "$(DEBUG)" "true"
	BIN_DIR = Debug
	CFLAGS = $(DEBUG_FLAGS)
	CPPFLAGS = $(DEBUG_FLAGS)
else
	BIN_DIR = Release
	CFLAGS = $(OPT_FLAGS)
	CPPFLAGS = $(OPT_FLAGS)
endif

CPPFLAGS += -DNDEBUG -fPIC -Wno-deprecated -fno-exceptions -DHAVE_STDINT_H -fno-rtti
CPPFLAGS += -DSM_DEFAULT_THREADER -Dstricmp=strcasecmp
CFLAGS += -DNDEBUG -fPIC -DHAVE_STDINT_H -DSM_DEFAULT_THREADER -Dstricmp=strcasecmp

ifeq "$(AMD64)" "true"
	BINARY = $(NAME)_$(BIN_SUFFIX_64)
	CFLAGS += -DPAWN_CELL_SIZE=64 -DHAVE_I64 -m64
	CPPFLAGS += -DPAWN_CELL_SIZE=64 -DHAVE_I64 -m64
else
	BINARY = $(NAME)_$(BIN_SUFFIX_32)
	CFLAGS += -DPAWN_CELL_SIZE=32 -DJIT -DASM32
	CPPFLAGS += -DPAWN_CELL_SIZE=32 -DJIT -DASM32
	OPT_FLAGS += -march=i586
endif

OBJ_LINUX_CPP := $(CPP_OBJECTS:%.cpp=$(BIN_DIR)/%.o)

$(BIN_DIR)/%.o: %.cpp
	$(CPP) $(INCLUDE) $(CPPFLAGS) -o $@ -c $<

OBJ_LINUX_C := $(C_OBJECTS:%.c=$(BIN_DIR)/%.o)

$(BIN_DIR)/%.o: %.c
	$(CC) $(INCLUDE) $(CFLAGS) -c -o $@ $<

all:
	mkdir -p $(BIN_DIR)/thread
	mkdir -p $(BIN_DIR)/sqlitepp
	mkdir -p $(BIN_DIR)/sdk
	mkdir -p $(BIN_DIR)
	mkdir -p $(BIN_DIR)/$(SQL)
	$(MAKE) sqlite

amd64:
	$(MAKE) all AMD64=true

sqlite: $(OBJ_LINUX_C) $(OBJ_LINUX_CPP)
	$(CPP) $(INCLUDE) $(CPPFLAGS) $(OBJ_LINUX_C) $(OBJ_LINUX_CPP) $(LINK) -shared -ldl -lm -o$(BIN_DIR)/$(BINARY)

debug:	
	$(MAKE) all DEBUG=true

default: all

clean:
	rm -rf Release/$(SQL)/*.o
	rm -rf Release/*.o
	rm -rf Release/$(NAME)_$(BIN_SUFFIX_32)
	rm -rf Release/$(NAME)_$(BIN_SUFFIX_64)
	rm -rf Debug/$(SQL)/*.o
	rm -rf Debug/*.o
	rm -rf Debug/$(NAME)_$(BIN_SUFFIX_32)
	rm -rf Debug/$(NAME)_$(BIN_SUFFIX_64)
