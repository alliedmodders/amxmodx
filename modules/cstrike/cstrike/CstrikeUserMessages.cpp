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

#include <amxxmodule.h>
#include "CstrikeUserMessages.h"
#include "CstrikeUtils.h"
#include "CstrikeHacks.h"
#include "CstrikePlayer.h"

bool ShouldBlock;
bool ShouldBlockHLTV;

int MessageIdArmorType;
int MessageIdHLTV;
int MessageIdMoney;
int MessageIdResetHUD;
int MessageIdScoreAttrib;
int MessageIdScoreInfo;
int MessageIdSetFOV;
int MessageIdStatusIcon;
int MessageIdTeamInfo;
int MessageIdTextMsg;

struct UserMsg
{
	const char* name;
	int*        id;
};

UserMsg MessagesList[] =
{
	{ "ArmorType"  , &MessageIdArmorType   },
	{ "HLTV"       , &MessageIdHLTV        },
	{ "CurWeapon"  , &MessageIdMoney       },
	{ "ResetHUD"   , &MessageIdResetHUD    },
	{ "ScoreAttrib", &MessageIdScoreAttrib },
	{ "ScoreInfo"  , &MessageIdScoreInfo   },
	{ "SetFOV"     , &MessageIdSetFOV      },
	{ "StatusIcon" , &MessageIdStatusIcon  },
	{ "TeamInfo"   , &MessageIdTeamInfo    },
	{ "TextMsg"    , &MessageIdTextMsg     },
	{ nullptr      , nullptr               }
};

int OnRegUserMsg_Post(const char *pszName, int iSize)
{
	for (size_t i = 0; MessagesList[i].name; ++i)
	{
		if (!*MessagesList[i].id && strcmp(MessagesList[i].name, pszName) == 0)
		{
			*MessagesList[i].id = META_RESULT_ORIG_RET(int);
			break;
		}
	}

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void OnMessageBegin(int msg_dest, int msg_type, const float *pOrigin, edict_t *pEntity)
{
	switch (msg_dest)
	{
		case MSG_ONE:
		case MSG_ONE_UNRELIABLE:
		{
			if (msg_type == MessageIdSetFOV)
			{
				int index = ENTINDEX(pEntity);

				if (Zooming[index])
				{
					GET_OFFSET_NO_ERROR("CBasePlayer", m_iFOV);

					if (get_pdata<int>(pEntity, m_iFOV) != Zooming[index])
					{
						set_pdata<int>(pEntity, m_iFOV, Zooming[index]);

						ShouldBlock = true;
						ShouldBlockHLTV = true;

						RETURN_META(MRES_SUPERCEDE);
					}
				}
			}
			else if (msg_type == MessageIdResetHUD)
			{
				int index = ENTINDEX(pEntity);

				if (Zooming[index])
					Zooming[index] = 0;

				if (g_players[index].GetModelled())
					g_players[index].SetInspectModel(true);
			}
			break;
		}
		case MSG_SPEC:
		{
			if (msg_type == MessageIdHLTV && ShouldBlockHLTV)
			{
				ShouldBlock = true;
				ShouldBlockHLTV = false;

				RETURN_META(MRES_SUPERCEDE);
			}
			break;
		}
	}

	if (ShouldBlockHLTV)
	{
		ShouldBlockHLTV = false;
	}

	RETURN_META(MRES_IGNORED);
}

void OnWriteByte(int iValue)
{
	if (ShouldBlock) 
	{
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void OnMessageEnd(void)
{
	if (ShouldBlock)
	{
		ShouldBlock = false;
		RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}

void EnableMessageHooks()
{
	if (!g_pengfuncsTable->pfnMessageBegin)
	{
		g_pengfuncsTable->pfnMessageBegin = OnMessageBegin;
		g_pengfuncsTable->pfnWriteByte    = OnWriteByte;
		g_pengfuncsTable->pfnMessageEnd   = OnMessageEnd;
	}
}
