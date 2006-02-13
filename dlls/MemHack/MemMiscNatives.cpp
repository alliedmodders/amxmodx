#include "MemMisc.h"

#define NATIVE_MISC_ADDRESS params[1]
#define NATIVE_MISC_BASEADDRESS PickBaseAddress(params[2])
#define NATIVE_MISC_FLAGS params[3]

static cell AMX_NATIVE_CALL memhack_get_base(AMX *amx, cell *params)
{
	cell *success = MF_GetAmxAddr(amx, params[2]);   
	maddress BaseAddr = NULL;

	bool is_success = GetBaseAddress((void*)(params[1]), BaseAddr);
	*success = is_success;

	return cell(BaseAddr);
}

static cell AMX_NATIVE_CALL memhack_get_realaddr(AMX *amx, cell *params)
{
	return (cell)GetRealMemoryAddress(NATIVE_MISC_ADDRESS,NATIVE_MISC_BASEADDRESS,NATIVE_MISC_FLAGS);
}

static cell AMX_NATIVE_CALL memhack_return_addr(AMX *amx, cell *params)
{
		return (cell)PickBaseAddress(params[1]);
}

AMX_NATIVE_INFO misc_natives[] = {
	{ "memhack_get_base",	memhack_get_base	},
	{ "memhack_get_realaddr",	memhack_get_realaddr	},
	{ "memhack_return_addr",	memhack_return_addr	},
	{ NULL, NULL }
};
