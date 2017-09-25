#include "amxxmodule.h"
#include "memory.h"

// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2017, SNMetamorph.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

libwrapper gamedll;
bool FindGameLibAddress()
{
	#if defined(__linux__)

	#else
		MEMORY_BASIC_INFORMATION meminfo;
		VirtualQuery((void *)MDLL_FUNC->pfnGetGameDescription(), &meminfo, sizeof(meminfo));
		IMAGE_DOS_HEADER *dos = (IMAGE_DOS_HEADER*)meminfo.AllocationBase;
		IMAGE_NT_HEADERS *pe = (IMAGE_NT_HEADERS*)((unsigned long)dos + (unsigned long)dos->e_lfanew);

		if (pe->Signature != IMAGE_NT_SIGNATURE)
			return false;

		gamedll.base = (void *)meminfo.AllocationBase;
		gamedll.size = pe->OptionalHeader.SizeOfImage;
		gamedll.codebase = (void *)((size_t)gamedll.base + (size_t)pe->OptionalHeader.BaseOfCode);
		gamedll.end = (size_t)gamedll.base + gamedll.size;

		return true;
	#endif
}

void *FindPatternAddress(size_t startaddr, size_t endaddr, byte *pattern, char *mask)
{
	for (size_t i = startaddr; i < endaddr - strlen(mask); i++)
	{
		byte failscount = 0;
		for (size_t j = 0; j < strlen(mask); j++)
		{
			if (mask[j] == 'x' && (byte)pattern[j] != *(byte *)(i + j))
			{
				failscount++;
				break;
			}
		}
		if (failscount == 0)
			return (void *)i;
	}
	return nullptr;
}