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

#include "CstrikePlayer.h"

CPlayer Players[33];
ke::Vector<int> ModelsUpdateQueue;

void ClientDisconnect(edict_t *pEntity)
{
	int index = ENTINDEX(pEntity);

	Players[index].ResetModel();
	Players[index].ResetZoom();

	RETURN_META(MRES_IGNORED);
}

void ClientUserInfoChanged(edict_t *pEntity, char *infobuffer)
{
	if (pEntity->pvPrivateData)
	{
		Players[ENTINDEX(pEntity)].UpdateModel(pEntity);
	}

	RETURN_META(MRES_IGNORED);
}

void SetClientKeyValue(int clientIndex, char *infobuffer, const char *key, const char *value)
{
	if (!strcmp(key, "model") && Players[clientIndex].HasModel(value))
	{
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void StartFrame()
{
	if (ModelsUpdateQueue.empty())
	{
		g_pFunctionTable->pfnStartFrame = nullptr;

		for (int i = 1; i < gpGlobals->maxClients; ++i)
		{
			if (Players[i].HasModel())
			{
				RETURN_META(MRES_IGNORED);
			}
		}

		g_pFunctionTable->pfnClientUserInfoChanged = nullptr;
		g_pengfuncsTable->pfnSetClientKeyValue = nullptr;

		RETURN_META(MRES_IGNORED);
	}

	ServerStatic->clients[ModelsUpdateQueue.popCopy()].sendinfo = true;

	RETURN_META(MRES_IGNORED);
}
