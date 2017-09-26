// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2017 SNMetamorph.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxxmodule.h"
#include "dllfuncs.h"

// Declare function pointers
int (*GetPlayersCount)() = nullptr;

void InitFuncPointers()
{
	#ifdef WIN32
		// Windows offsets (from mp.dll)
		GetPlayersCount = (int(*)())FindPatternAddress((size_t)gamelib.codebase, gamelib.end, (byte *)"\xA1\x14\xE7\x04\x09\x56\x57", "x?x??xx"); // example CountPlayers() function
	#else
		// Linux offsets (from ts_i386.so)
	#endif
}
