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

extern int MessageIdTextMsg;

bool UTIL_IsPlayer(edict_t *pPlayer)
{
	return strcmp(STRING(pPlayer->v.classname), "player") == 0;
}

void UTIL_TextMsg_Generic(edict_t* pPlayer, const char* message)
{
	MESSAGE_BEGIN(MSG_ONE, MessageIdTextMsg, nullptr, pPlayer);
		WRITE_BYTE(HUD_PRINTCENTER); // 1 = console, 2 = console, 3 = chat, 4 = center, 5 = radio
		WRITE_STRING(message);
	MESSAGE_END();
}

bool UTIL_CheckForPublic(const char *publicname)
{
	AMX* amx;
	int iFunctionIndex;
	int i = 0;
	char blah[64];

	strncpy(blah, publicname, sizeof(blah) - 1);

	while ((amx = MF_GetScriptAmx(i++)))
	{
		if (MF_AmxFindPublic(amx, blah, &iFunctionIndex) == AMX_ERR_NONE)
		{
			return true;
		}
	}

	return false;
}
