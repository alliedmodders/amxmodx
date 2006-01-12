#include "MemWrite.h"

#define NATIVE_PATCH_BASEADDRESS PickBaseAddress(params[2])
#define NATIVE_PATCH_ADDRESS params[1]
#define NATIVE_PATCH_FLAGS params[4]
#define NATIVE_PATCH_SIGNED params[5]
#define NATIVE_PATCH_PARAMETER params[3]

#define NATIVE_PATCH_MEMORY NATIVE_PATCH_BASEADDRESS, NATIVE_PATCH_ADDRESS

static cell AMX_NATIVE_CALL memhack_set_char(AMX *amx, cell *params)
{
	if(NATIVE_PATCH_SIGNED)
	{
		return (cell)UTIL_PatchMemory_Byte(NATIVE_PATCH_MEMORY, (char)(NATIVE_PATCH_PARAMETER), NATIVE_PATCH_FLAGS);
	}
	else
	{
		return (cell)UTIL_PatchMemory_UnsignedByte(NATIVE_PATCH_MEMORY, (unsigned char)(NATIVE_PATCH_PARAMETER), NATIVE_PATCH_FLAGS);
	}
}

static cell AMX_NATIVE_CALL memhack_set_short(AMX *amx, cell *params)
{
	if(NATIVE_PATCH_SIGNED)
	{
		return (cell)UTIL_PatchMemory_Word(NATIVE_PATCH_MEMORY, (short)(NATIVE_PATCH_PARAMETER), NATIVE_PATCH_FLAGS);
	}
	else
	{
		return (cell)UTIL_PatchMemory_UnsignedWord(NATIVE_PATCH_MEMORY, (unsigned short)(NATIVE_PATCH_PARAMETER), NATIVE_PATCH_FLAGS);
	}
}

static cell AMX_NATIVE_CALL memhack_set_long(AMX *amx, cell *params)
{
	if(NATIVE_PATCH_SIGNED)
	{
		return (cell)UTIL_PatchMemory_Byte(NATIVE_PATCH_MEMORY, (long)(NATIVE_PATCH_PARAMETER), NATIVE_PATCH_FLAGS);
	}
	else
	{
		return (cell)UTIL_PatchMemory_UnsignedByte(NATIVE_PATCH_MEMORY, (unsigned long)(NATIVE_PATCH_PARAMETER), NATIVE_PATCH_FLAGS);
	}
}

static cell AMX_NATIVE_CALL memhack_set_quad(AMX *amx, cell *params)
{
	return (cell)UTIL_PatchMemory_Qword(NATIVE_PATCH_MEMORY, (long long)(NATIVE_PATCH_PARAMETER), NATIVE_PATCH_FLAGS);
}

static cell AMX_NATIVE_CALL memhack_set_float(AMX *amx, cell *params)
{
	return (cell)UTIL_PatchMemory_Float(NATIVE_PATCH_MEMORY, amx_ctof(NATIVE_PATCH_PARAMETER), NATIVE_PATCH_FLAGS);
}

static cell AMX_NATIVE_CALL memhack_set_pointer(AMX *amx, cell *params)
{
	return (cell)UTIL_PatchMemory_Pointer(NATIVE_PATCH_MEMORY, (maddress)(NATIVE_PATCH_PARAMETER), NATIVE_PATCH_FLAGS);
}

AMX_NATIVE_INFO write_natives[] = {
	{ "memhack_set_char",	memhack_set_char	},
	{ "memhack_set_short",	memhack_set_short	},
	{ "memhack_set_long",	memhack_set_long	},

	{ "memhack_set_float",	memhack_set_float	},
	{ "memhack_set_quad",	memhack_set_quad	},
	{ "memhack_set_pointer",	memhack_set_pointer	},
	{ NULL, NULL }
};

