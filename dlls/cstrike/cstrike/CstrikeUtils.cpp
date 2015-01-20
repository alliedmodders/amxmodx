// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Counter-Strike Module
//

#include "amxxmodule.h"
#include "MemoryUtils.h"

bool UTIL_IsPlayer(AMX* amx, edict_t* pPlayer) 
{
	bool player = false;

	if (strcmp(STRING(pPlayer->v.classname), "player") == 0)
	{
		player = true;
	}

	return player;
}

void UTIL_TextMsg_Generic(edict_t* pPlayer, const char* message)
{
	MESSAGE_BEGIN(MSG_ONE, GET_USER_MSG_ID(PLID, "TextMsg", NULL), NULL, pPlayer);
	WRITE_BYTE(HUD_PRINTCENTER); // 1 = console, 2 = console, 3 = chat, 4 = center, 5 = radio
	WRITE_STRING(message);
	MESSAGE_END();
	/*
	The byte above seems to use these:
	#define HUD_PRINTNOTIFY		1
	#define HUD_PRINTCONSOLE	2
	#define HUD_PRINTTALK		3
	#define HUD_PRINTCENTER		4
	#define HUD_PRINTRADIO		5
	However both 1 and 2 seems to go to console with Steam CS.
	*/
}

void *UTIL_FindAddressFromEntry(const char *entry, bool isHidden, const char *library)
{
	void *addressInBase = NULL;
	void *finalAddress;

	if (strcmp(library, "mod") == 0)
	{
		addressInBase = (void *)MDLL_Spawn;
	}
	else if (strcmp(library, "engine") == 0)
	{
		addressInBase = (void *)gpGlobals;
	}

	finalAddress = NULL;

	if (*entry != '\\')
	{
#if defined(WIN32)

		MEMORY_BASIC_INFORMATION mem;

		if (VirtualQuery(addressInBase, &mem, sizeof(mem)))
		{
			finalAddress = g_MemUtils.ResolveSymbol(mem.AllocationBase, entry);
		}

#elif defined(__linux__) || defined(__APPLE__)

		Dl_info info;

		if (dladdr(addressInBase, &info) != 0)
		{
			void *handle = dlopen(info.dli_fname, RTLD_NOW);
			if (handle)
			{
				if (isHidden)
				{
					finalAddress = g_MemUtils.ResolveSymbol(handle, entry);
				}
				else
				{
					finalAddress = dlsym(handle, entry);
				}

				dlclose(handle);
			}
		}
#endif
	}
	else
	{
		finalAddress = g_MemUtils.DecodeAndFindPattern(addressInBase, entry);
	}

	return finalAddress != NULL ? finalAddress : NULL;
}

bool UTIL_CheckForPublic(const char *publicname)
{
	AMX* amx;
	int iFunctionIndex;
	int i = 0;
	char blah[64];

	strncpy(blah, publicname, sizeof(blah)-  1);

	while ((amx = MF_GetScriptAmx(i++)) != NULL)
	{
		if (MF_AmxFindPublic(amx, blah, &iFunctionIndex) == AMX_ERR_NONE)
		{
			return true;
		}
	}

	return false; 
}

