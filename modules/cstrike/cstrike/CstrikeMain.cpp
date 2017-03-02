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
#include "CstrikeHacks.h"
#include "CstrikeItemsInfos.h"
#include "CstrikeUserMessages.h"
#include <IGameConfigs.h>
#include <reapi/mod_rehlds_api.h>

IGameConfig *MainConfig;
IGameConfig *CommonConfig;
IGameConfigManager *ConfigManager;

HLTypeConversion TypeConversion;

extern StringHashMap<int> ModelsList;

int AmxxCheckGame(const char *game)
{
	if (strcasecmp(game, "cstrike") == 0 ||
		strcasecmp(game, "czero") == 0)
	{
		return AMXX_GAME_OK;
	}
	return AMXX_GAME_BAD;
}

void SV_ActivateServer_RH(IRehldsHook_SV_ActivateServer *chain, int runPhysics)
{
	chain->callNext(runPhysics);

	auto numResources = RehldsData->GetResourcesNum();

	if (!numResources)
	{
		return;
	}

	ModelsList.clear();

	for (auto i = 0; i < numResources; ++i) // Saves all the precached models into a list.
	{
		auto resource = RehldsData->GetResource(i);

		if (resource->type == t_model)
		{
			ModelsList.insert(resource->szFileName, i);
		}
	}
}

void OnAmxxAttach()
{
	MF_AddNatives(CstrikeNatives);

	ConfigManager = MF_GetConfigManager();

	char error[256] = "";
	ConfigManager->AddUserConfigHook("ItemInfos", &ItemsManager);

	if (!ConfigManager->LoadGameConfigFile("modules.games", &MainConfig, error, sizeof(error)) && *error)
	{
		MF_Log("Could not read module.games gamedata: %s", error);
		return;
	}

	*error = '\0';

	if (!ConfigManager->LoadGameConfigFile("common.games", &CommonConfig, error, sizeof(error)) && *error)
	{
		MF_Log("Could not read common.games gamedata: %s", error);
		return;
	}

	InitializeHacks();

	RehldsHookchains->SV_ActivateServer()->registerHook(SV_ActivateServer_RH);
}

void OnPluginsLoaded()
{
	TypeConversion.init();

	ForwardInternalCommand = MF_RegisterForward("CS_InternalCommand", ET_STOP, FP_CELL, FP_STRING, FP_DONE);
	ForwardOnBuy           = MF_RegisterForward("CS_OnBuy"          , ET_STOP, FP_CELL, FP_CELL, FP_DONE);
	ForwardOnBuyAttempt    = MF_RegisterForward("CS_OnBuyAttempt"   , ET_STOP, FP_CELL, FP_CELL, FP_DONE);
}

void OnServerActivate(edict_t *pEdictList, int edictCount, int clientMax)
{
	// Used to catch WeaponList message at map change.
	EnableMessageHooks();

	if (!ClientCommandDetour) // All CS_* forwards requires ClientCommand. Unlikely to fail. 
	{
		ToggleDetour_ClientCommands(false);
		ToggleDetour_BuyCommands(false);

		RETURN_META(MRES_IGNORED);
	}

	auto haveBotDetours = UseBotArgs && BotArgs;
	auto haveBuyDetours = BuyGunAmmoDetour && GiveNamedItemDetour && AddAccountDetour && CanPlayerBuyDetour && CanBuyThisDetour;

	HasInternalCommandForward = haveBotDetours && UTIL_CheckForPublic("CS_InternalCommand");
	HasOnBuyAttemptForward    = haveBuyDetours && UTIL_CheckForPublic("CS_OnBuyAttempt");
	HasOnBuyForward           = haveBuyDetours && UTIL_CheckForPublic("CS_OnBuy");

	ToggleDetour_ClientCommands(HasInternalCommandForward || HasOnBuyAttemptForward || HasOnBuyForward);
	ToggleDetour_BuyCommands(HasOnBuyForward);

	RETURN_META(MRES_IGNORED);
}

void OnServerActivate_Post(edict_t *pEdictList, int edictCount, int clientMax)
{
	DisableMessageHooks();

	RETURN_META(MRES_IGNORED);
}

void OnServerDeactivate()
{
	if (!ClientCommandDetour)
	{
		RETURN_META(MRES_IGNORED);
	}

	ToggleDetour_ClientCommands(false);
	ToggleDetour_BuyCommands(false);

	RETURN_META(MRES_IGNORED);
}

void OnPluginsUnloaded()
{
	// Used with model natives, enabled on demand.
	g_pengfuncsTable->pfnSetClientKeyValue     = nullptr;
	g_pFunctionTable->pfnClientUserInfoChanged = nullptr;
	g_pFunctionTable->pfnStartFrame            = nullptr;

	// Force to disable all event hooks at map change.
	DisableMessageHooks(true);
}

void OnAmxxDetach()
{
	ConfigManager->RemoveUserConfigHook("ItemInfos", &ItemsManager);

	ConfigManager->CloseGameConfigFile(MainConfig);
	ConfigManager->CloseGameConfigFile(CommonConfig);

	ShutdownHacks();
}
