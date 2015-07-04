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
#include "CstrikeUtils.h"
#include "CstrikeDatas.h"
#include "CstrikeHacks.h"
#include "CstrikeHLTypeConversion.h"
#include <IGameConfigs.h>

IGameConfig *MainConfig;
IGameConfig *OffsetConfig;
IGameConfigManager *ConfigManager;

struct UserMsg
{
	const char* name;
	int*        id;
};

UserMsg MessagesList[] =
{
	{ "ArmorType"  , &MessageIdArmorType   },
	{ "CurWeapon"  , &MessageIdMoney       },
	{ "ResetHUD"   , &MessageIdResetHUD    },
	{ "ScoreAttrib", &MessageIdScoreAttrib },
	{ "ScoreInfo"  , &MessageIdScoreInfo   },
	{ "StatusIcon" , &MessageIdStatusIcon  },
	{ "TeamInfo"   , &MessageIdTeamInfo    },
	{ "TextMsg"    , &MessageIdTextMsg     },
	{ nullptr      , nullptr               }
};

int AmxxCheckGame(const char *game)
{
	if (strcasecmp(game, "cstrike") == 0 ||
		strcasecmp(game, "czero") == 0)
	{
		return AMXX_GAME_OK;
	}
	return AMXX_GAME_BAD;
}

void OnAmxxAttach()
{
	MF_AddNatives(CstrikeNatives);

	ConfigManager = MF_GetConfigManager();

	char error[256];
	error[0] = '\0';

	if (!ConfigManager->LoadGameConfigFile("modules.games", &MainConfig, error, sizeof(error)) && error[0] != '\0')
	{
		MF_Log("Could not read module.games gamedata: %s", error);
		return;
	}

	error[0] = '\0';

	if (!ConfigManager->LoadGameConfigFile("common.games", &OffsetConfig, error, sizeof(error)) && error[0] != '\0')
	{
		MF_Log("Could not read common.games gamedata: %s", error);
		return;
	}

	InitializeHacks();
}

void OnPluginsLoaded()
{
	ForwardInternalCommand = MF_RegisterForward("CS_InternalCommand", ET_STOP, FP_CELL, FP_STRING, FP_DONE);
	ForwardOnBuy           = MF_RegisterForward("CS_OnBuy"          , ET_STOP, FP_CELL, FP_CELL, FP_DONE);
	ForwardOnBuyAttempt    = MF_RegisterForward("CS_OnBuyAttempt"   , ET_STOP, FP_CELL, FP_CELL, FP_DONE);

	// Checking whether such public forwards are used in plugins.
	// Resetting variable to -1 to avoid running unnecessary code in ClientCommand.
	if (!UTIL_CheckForPublic("CS_InternalCommand"))   { ForwardInternalCommand = -1; }
	if (!UTIL_CheckForPublic("CS_OnBuy"))             { ForwardOnBuy = -1; }
	if (!UTIL_CheckForPublic("CS_OnBuyAttempt"))      { ForwardOnBuyAttempt = -1; }

	// And enable/disable detours when necessary.
	ToggleDetour_ClientCommands(ForwardInternalCommand != -1 || ForwardOnBuy != -1 || ForwardOnBuyAttempt != -1);
	ToggleDetour_BuyCommands(ForwardOnBuy != -1);

	// Search pev offset automatically.
	if (!G_OffsetHandler)
	{
		G_OffsetHandler = new OffsetHandler;
	}
}

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

void OnAmxxDetach()
{
	ConfigManager->CloseGameConfigFile(MainConfig);
	ConfigManager->CloseGameConfigFile(OffsetConfig);

	ShutdownHacks();
}
