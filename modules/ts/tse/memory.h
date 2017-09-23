#include "amxxmodule.h"
#pragma once

// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2017, SNMetamorph.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

// Platform-specific inclusions
#if defined(__linux__)

#else
	#include <windows.h>
#endif

struct libwrapper
{
	size_t size;
	size_t end;
	void *codebase;
	void *base;
};

extern libwrapper gamedll;
extern bool FindGameDllAddress();
extern void *FindPatternAddress(size_t startaddr, size_t endaddr, byte *pattern, char *mask);
