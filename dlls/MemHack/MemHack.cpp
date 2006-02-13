#include "MemMisc.h"

extern AMX_NATIVE_INFO read_natives[];
extern AMX_NATIVE_INFO write_natives[];
extern AMX_NATIVE_INFO misc_natives[];

void OnAmxxAttach() 
{
	// Get Base Addresses for dll and engine; exit if unavailable.
	if(GetBaseAddresses() == false) return MF_LogError(NULL,AMX_ERR_MEMACCESS,"Unable to get base address of game or mod .dll!");

	// Add natives if base is found.
	MF_AddNatives(read_natives);
	MF_AddNatives(write_natives);
	MF_AddNatives(misc_natives);
}