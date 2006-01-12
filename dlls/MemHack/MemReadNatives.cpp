#include "MemRead.h"

#define NATIVE_HACK_BASEADDRESS PickBaseAddress(params[2])
#define NATIVE_HACK_ADDRESS params[1]
#define NATIVE_HACK_FLAGS params[3]
#define NATIVE_HACK_SIGNED params[4]
#define NATIVE_HACK_MEMORY NATIVE_HACK_BASEADDRESS, NATIVE_HACK_ADDRESS, NATIVE_HACK_FLAGS

static cell AMX_NATIVE_CALL memhack_get_char(AMX *amx, cell *params)
{
	if(NATIVE_HACK_SIGNED)
	{
		char HackedMemory = UTIL_ReadMemory_Byte(NATIVE_HACK_MEMORY);
		return (cell)(HackedMemory);
	}
	else
	{
		unsigned char HackedMemory = UTIL_ReadMemory_UnsignedByte(NATIVE_HACK_MEMORY);
		return (cell)(HackedMemory);
	}
}

static cell AMX_NATIVE_CALL memhack_get_short(AMX *amx, cell *params)
{
	if(NATIVE_HACK_SIGNED)
	{
		short HackedMemory = UTIL_ReadMemory_Word(NATIVE_HACK_MEMORY);
		return (cell)(HackedMemory);
	}
	else
	{
		unsigned short HackedMemory = UTIL_ReadMemory_UnsignedWord(NATIVE_HACK_MEMORY);
		return (cell)(HackedMemory);
	}
}

static cell AMX_NATIVE_CALL memhack_get_long(AMX *amx, cell *params)
{
	if(NATIVE_HACK_SIGNED)
	{
		long HackedMemory = UTIL_ReadMemory_Dword(NATIVE_HACK_MEMORY);
		return (cell)(HackedMemory);
	}
	else
	{
		unsigned long HackedMemory = UTIL_ReadMemory_UnsignedDword(NATIVE_HACK_MEMORY);
		return (cell)(HackedMemory);
	}
}

static cell AMX_NATIVE_CALL memhack_get_quad(AMX *amx, cell *params)
{
	long long HackedMemory = UTIL_ReadMemory_Qword(NATIVE_HACK_MEMORY);
	return amx_ftoc(float(HackedMemory));
}

static cell AMX_NATIVE_CALL memhack_get_float(AMX *amx, cell *params)
{
	float HackedMemory = UTIL_ReadMemory_Float(NATIVE_HACK_MEMORY);
	return amx_ftoc(HackedMemory);
}

static cell AMX_NATIVE_CALL memhack_get_pointer(AMX *amx, cell *params)
{
	maddress HackedMemory = UTIL_ReadMemory_Pointer(NATIVE_HACK_MEMORY);
	return (cell)(HackedMemory);
}

AMX_NATIVE_INFO read_natives[] = {
	{ "memhack_get_char",	memhack_get_char	},
	{ "memhack_get_short",	memhack_get_short	},
	{ "memhack_get_long",	memhack_get_long	},

	{ "memhack_get_float",	memhack_get_float	},
	{ "memhack_get_quad",	memhack_get_quad	},
	{ "memhack_get_pointer",	memhack_get_pointer	},
	{ NULL, NULL }
};