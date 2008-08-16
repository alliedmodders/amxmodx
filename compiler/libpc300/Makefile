#(C)2004-2005 AMX Mod X Development Team
# Makefile written by David "BAILOPAN" Anderson

### EDIT BELOW FOR OTHER PROJECTS ###

OPT_FLAGS = -O3 -funroll-loops -s -pipe -fno-strict-aliasing -fvisibility=hidden
DEBUG_FLAGS = -g -ggdb3
CPP = gcc-4.1
NAME = amxxpc

OBJECTS = sc1.c sc2.c sc3.c sc4.c sc5.c sc6.c sc7.c scvars.c scmemfil.c \
	scstate.c sclist.c sci18n.c scexpand.c pawncc.c libpawnc.c prefix.c \
	memfile.c

LINK = -lpthread

INCLUDE = -I. 

ifeq "$(PAWN64)" "true"
	BINARY = $(NAME)64.so
	BIN_DIR = Release64
	CFLAGS += -DPAWN_CELL_SIZE=64 -DHAVE_I64 -Dpc_printf=pc_printf64
else
	BINARY = $(NAME)32.so
	BIN_DIR = Release32
	CFLAGS += -DPAWN_CELL_SIZE=32 
endif

CFLAGS += -DLINUX -DNDEBUG -DHAVE_STDINT_H -DENABLE_BINRELOC -DNO_MAIN -DPAWNC_DLL -static-libgcc

CFLAGS += $(OPT_FLAGS)
OBJ_LINUX := $(OBJECTS:%.c=$(BIN_DIR)/%.o)

$(BIN_DIR)/%.o: %.c
	$(CPP) $(INCLUDE) $(CFLAGS) -m32 -o $@ -c $<

all:
	mkdir -p Release32
	mkdir -p Release64
	$(MAKE) pawn_make
	$(MAKE) pawn_make PAWN64=true

pawn_make: $(OBJ_LINUX)
	$(CPP) $(INCLUDE) $(CFLAGS) -m32 $(OBJ_LINUX) $(LINK) -shared -ldl -lm -o$(BIN_DIR)/$(BINARY)

debug:	
	$(MAKE) all DEBUG=true

default: all

clean:
	rm -rf Release32/*.o
	rm -rf Release32/$(BINARY)
	rm -rf Release64/*.o
	rm -rf Release64/$(BINARY)

