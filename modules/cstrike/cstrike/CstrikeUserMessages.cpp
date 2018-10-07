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

#include "CstrikeUserMessages.h"
#include "CstrikeUtils.h"
#include "CstrikeHacks.h"
#include "CstrikePlayer.h"
#include "CstrikeItemsInfos.h"
#include <amtl/am-string.h>

bool ShouldBlock;
bool ShouldBlockHLTV;
bool ShouldDisableHooks;
bool RetrieveWeaponList;
ItemInfos CurrentWeaponList;
int ArgPosition;

int MessageIdArmorType;
int MessageIdItemStatus;
int MessageIdHLTV;
int MessageIdMoney;
int MessageIdResetHUD;
int MessageIdScoreAttrib;
int MessageIdScoreInfo;
int MessageIdSetFOV;
int MessageIdStatusIcon;
int MessageIdTeamInfo;
int MessageIdTextMsg;
int MessageIdWeaponList;

extern bool OnMessageStatusIcon(edict_t *pPlayer);
extern bool OnMessageItemStatus(edict_t *pPlayer);

struct UserMsg
{
	const char* name;
	int*        id;
};

UserMsg MessagesList[] =
{
	{ "ArmorType"  , &MessageIdArmorType   },
	{ "ItemStatus" , &MessageIdItemStatus  },
	{ "HLTV"       , &MessageIdHLTV        },
	{ "Money"      , &MessageIdMoney       },
	{ "ResetHUD"   , &MessageIdResetHUD    },
	{ "ScoreAttrib", &MessageIdScoreAttrib },
	{ "ScoreInfo"  , &MessageIdScoreInfo   },
	{ "SetFOV"     , &MessageIdSetFOV      },
	{ "StatusIcon" , &MessageIdStatusIcon  },
	{ "TeamInfo"   , &MessageIdTeamInfo    },
	{ "TextMsg"    , &MessageIdTextMsg     },
	{ "WeaponList" , &MessageIdWeaponList  },
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
				int index = TypeConversion.edict_to_id(pEntity);
				int zoom = Players[index].GetZoom();

				if (zoom)
				{
					GET_OFFSET_NO_ERROR("CBasePlayer", m_iFOV);

					if (get_pdata<int>(pEntity, m_iFOV) != zoom)
					{
						set_pdata<int>(pEntity, m_iFOV, zoom);

						ShouldBlock = true;
						ShouldBlockHLTV = true;

						RETURN_META(MRES_SUPERCEDE);
					}
				}
			}
			else if (msg_type == MessageIdResetHUD)
			{
				int index = TypeConversion.edict_to_id(pEntity);

				if (Players[index].GetZoom())
				{
					Players[index].ResetZoom();
					DisableMessageHooks();
				}
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
		case MSG_INIT:
		{
			if (msg_type == MessageIdWeaponList)
			{
				//	MESSAGE_BEGIN(MSG_INIT, gmsgWeaponList);
				//	  WRITE_STRING(pszName);
				//	  WRITE_BYTE(CBasePlayer::GetAmmoIndex(II.pszAmmo1));
				//	  WRITE_BYTE(II.iMaxAmmo1);
				//	  WRITE_BYTE(CBasePlayer::GetAmmoIndex(II.pszAmmo2));
				//	  WRITE_BYTE(II.iMaxAmmo2);
				//	  WRITE_BYTE(II.iSlot);
				//	  WRITE_BYTE(II.iPosition);
				//	  WRITE_BYTE(II.iId);
				//	  WRITE_BYTE(II.iFlags);
				//	MESSAGE_END();
				//
				RetrieveWeaponList = true;
			}
		}
	}

	if (ShouldBlockHLTV)
	{
		ShouldBlockHLTV = false;
	}

	RETURN_META(MRES_IGNORED);
}

void OnWriteByte(int value)
{
	if (ShouldBlock)
	{
		RETURN_META(MRES_SUPERCEDE);
	}

	if (RetrieveWeaponList)
	{
		switch (++ArgPosition)
		{
			case 1: CurrentWeaponList.ammoIndex1 = value;
			case 2: CurrentWeaponList.maxAmmo1 = value;
			case 3: CurrentWeaponList.ammoIndex2 = value;
			case 4: CurrentWeaponList.maxAmmo2 = value;
			case 5: CurrentWeaponList.slot = value;
			case 6: CurrentWeaponList.position = value;
			case 7: CurrentWeaponList.id = value;
			case 8: CurrentWeaponList.flags = value;
		}
	}

	RETURN_META(MRES_IGNORED);
}

void OnWriteString(const char *value)
{
	if (ShouldBlock)
	{
		RETURN_META(MRES_SUPERCEDE);
	}

	if (RetrieveWeaponList)
	{
		CurrentWeaponList.name = value;
	}

	RETURN_META(MRES_IGNORED);
}

void OnMessageEnd(void)
{
	if (ShouldBlock)
	{
		ShouldBlock = false;

		if (ShouldDisableHooks)
		{
			ShouldDisableHooks = false;
			DisableMessageHooks();
		}

		RETURN_META(MRES_SUPERCEDE);
	}

	if (RetrieveWeaponList)
	{
		RetrieveWeaponList = false;
		ArgPosition = 0;

		WeaponsList[CurrentWeaponList.id] = CurrentWeaponList;
	}

	RETURN_META(MRES_IGNORED);
}

size_t RefCount;

void EnableMessageHooks()
{
	++RefCount;

	if (!g_pengfuncsTable->pfnMessageBegin)
	{
		g_pengfuncsTable->pfnMessageBegin = OnMessageBegin;
		g_pengfuncsTable->pfnWriteByte    = OnWriteByte;
		g_pengfuncsTable->pfnWriteString  = OnWriteString;
		g_pengfuncsTable->pfnMessageEnd   = OnMessageEnd;
	}
}

void DisableMessageHooks(bool force)
{
	if (force)
	{
		RefCount = 1;
	}

	if (--RefCount == 0)
	{
		g_pengfuncsTable->pfnMessageBegin = nullptr;
		g_pengfuncsTable->pfnWriteByte    = nullptr;
		g_pengfuncsTable->pfnWriteString  = nullptr;
		g_pengfuncsTable->pfnMessageEnd   = nullptr;
	}
}
