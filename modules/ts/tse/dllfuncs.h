#include "amxxmodule.h"
#include "memory.h"
#pragma once

// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2017, SNMetamorph.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

// Platform-specific inclusions
#ifdef WIN32
	#include <windows.h>
#else
	
#endif

extern void InitFuncPointers();

extern int (*GetPlayersCount)();

