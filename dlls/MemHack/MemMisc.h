#ifndef __MEMMISC_H__
#define __MEMMISC_H__

#include "MemConst.h"

#define SAMPLE_DLLFUNC reinterpret_cast<void*>(gpGamedllFuncs->dllapi_table->pfnThink)
#define SAMPLE_ENGFUNC reinterpret_cast<void*>(*g_engfuncs.pfnChangeLevel)

extern maddress gameDllAddress;
extern maddress gameEngAddress;

inline maddress PickBaseAddress(long num)
{
	if(num == 0) return gameDllAddress;
	else if(num == 1) return gameEngAddress;

	return NULL;
}

extern int MemoryProtect(void *addr, size_t len, unsigned long newProt, unsigned long *oldProt, char memType = MEMTYPE_CODE);
extern maddress GetRealMemoryAddress(maddress baseaddress,maddress address, char memType);

extern bool GetBaseAddress(void *pAddr, maddress &pBaseAddr);

inline bool GetBaseAddresses( void )
{
	bool success = false;

	success = GetBaseAddress(SAMPLE_DLLFUNC, gameDllAddress );
	if(success == false) return false;

	success = GetBaseAddress(SAMPLE_ENGFUNC, gameEngAddress );
	if(success == false) return false;

	return true;
}

#endif