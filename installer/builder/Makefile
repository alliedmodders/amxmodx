#(C)2004-2013 AMX Mod X Development Team
# Makefile written by David "BAILOPAN" Anderson

### EDIT BELOW FOR OTHER PROJECTS ###

CS = gmcs
NAME = builder
BINARY = $(NAME).exe

OBJECTS = Main.cs Config.cs ABuilder.cs AMod.cs Build.cs Win32Builder.cs LinuxBuilder.cs \
	CoreMod.cs ModDoD.cs ModEsf.cs ModCstrike.cs ModTFC.cs \
	ModTs.cs ModNs.cs MacBuilder.cs

default: all

all:	$(OBJECTS)
	$(CS) $(OBJECTS) -out:$(BINARY)

clean:
	rm -rf $(BINARY)
