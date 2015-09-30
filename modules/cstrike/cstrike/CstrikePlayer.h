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

#ifndef _CSTRIKE_PLAYER_H_
#define _CSTRIKE_PLAYER_H_

#include <amxxmodule.h>
#include "CstrikeUtils.h"
#include "CstrikeHacks.h"
#include <amtl/am-vector.h>

extern ke::Vector<int> ModelsUpdateQueue;

void StartFrame();
void ClientUserInfoChanged(edict_t *pEntity, char *infobuffer);
void SetClientKeyValue(int clientIndex, char *infobuffer, const char *key, const char *value);

class CPlayer  
{
	public:

		CPlayer()
		{
			ResetModel();
			ResetZoom();
		}

		bool HasModel(const char *model = nullptr)
		{
			if (*m_Model != '\0')
			{
				if (model && *model)
				{
					return strcmp(m_Model, model) != 0;
				}

				return true;
			}

			return false;
		}

		void SetModel(const char* modelIn)
		{
			if (modelIn && *modelIn)
			{
				strncopy(m_Model, modelIn, sizeof(m_Model));
			}
			else
			{
				ResetModel();
			}
		}

		void ResetModel(edict_t *pPlayer = nullptr)
		{
			*m_Model = '\0';

			if (pPlayer)
			{
				MDLL_ClientUserInfoChanged(pPlayer, GETINFOKEYBUFFER(pPlayer));

				PostponeModelUpdate(ENTINDEX(pPlayer) - 1);
			}
		}

		void UpdateModel(edict_t *pPlayer)
		{
			if (!HasModel())
			{
				return;
			}

			char *infobuffer = GETINFOKEYBUFFER(pPlayer);

			if (strcmp(GETCLIENTKEYVALUE(infobuffer, "model"), m_Model) != 0)
			{
				int index = ENTINDEX(pPlayer);

				SETCLIENTKEYVALUE(index, infobuffer, "model", m_Model);

				PostponeModelUpdate(index - 1);
			}
		}

		int GetZoom()
		{
			return m_Zooming;
		}

		void SetZoom(int value)
		{
			m_Zooming = value;
		}

		void ResetZoom()
		{
			m_Zooming = 0;
		}

	private:

		void PostponeModelUpdate(int index)
		{
			if (!g_pFunctionTable->pfnStartFrame)
			{
				g_pFunctionTable->pfnStartFrame = StartFrame;
				g_pFunctionTable->pfnClientUserInfoChanged = ClientUserInfoChanged;
				g_pengfuncsTable->pfnSetClientKeyValue = SetClientKeyValue;
			}

			if (!ServerStatic)
			{
				MF_Log("Postponing of model update disabled, check your gamedata files");
				return;
			}

			ServerStatic->clients[index].sendinfo = false;

			ModelsUpdateQueue.append(index);
		}

	private:

		char m_Model[32];
		int  m_Zooming;
};

extern CPlayer Players[33];

#endif // _CSTRIKE_PLAYER_H_
