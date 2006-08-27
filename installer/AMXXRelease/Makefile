#(C)2004-2005 AMX Mod X Development Team
# Makefile written by David "BAILOPAN" Anderson

### EDIT BELOW FOR OTHER PROJECTS ###

CS = /usr/mono/bin/mcs
NAME = amxxrelease
BINARY = $(NAME).bin
OUTPATH = .

OBJECTS = Main.cs Config.cs ABuilder.cs AMod.cs Build.cs Win32Builder.cs LinuxBuilder.cs \
	CoreMod.cs ModDoD.cs ModEsf.cs ModCstrike.cs ModTFC.cs \
	ModTs.cs ModNs.cs

default: all

all:	$(OBJECTS)
	$(CS) $(OBJECTS) -o $(OUTPATH)/$(BINARY)

clean:
	rm -rf amxxrelease

