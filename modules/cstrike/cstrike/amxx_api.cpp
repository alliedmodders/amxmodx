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
#include "CstrikeHLTypeConversion.h"

extern AMX_NATIVE_INFO CstrikeNatives[];

extern int ForwardInternalCommand;
extern int ForwardOnBuy;
extern int ForwardOnBuyAttempt;

void InitializeHacks();
void ShutdownHacks();
void ToggleDetour_ClientCommands(bool enable);
void ToggleDetour_BuyCommands(bool enable);

CreateNamedEntityFunc CS_CreateNamedEntity = nullptr;
UTIL_FindEntityByStringFunc CS_UTIL_FindEntityByString = nullptr;

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

	InitializeHacks();

	// cs_create_entity()
	CS_CreateNamedEntity = reinterpret_cast<CreateNamedEntityFunc>(UTIL_FindAddressFromEntry(CS_IDENT_CREATENAMEDENTITY, CS_IDENT_HIDDEN_STATE));

	if (CS_CreateNamedEntity <= 0)
	{
		MF_Log("CREATE_NAMED_ENITTY is not available - native cs_create_entity() has been disabled");
	}

	// cs_find_ent_by_class()
	CS_UTIL_FindEntityByString = reinterpret_cast<UTIL_FindEntityByStringFunc>(UTIL_FindAddressFromEntry(CS_IDENT_UTIL_FINDENTITYBYSTRING, CS_IDENT_HIDDEN_STATE));
	
	if (CS_UTIL_FindEntityByString <= 0)
	{
		MF_Log("UTIL_FindEntByString is not available - native cs_find_ent_by_class() has been disabled");
	}
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
	ToggleDetour_ClientCommands(ForwardInternalCommand != -1 || ForwardOnBuy != -1 || ForwardOnBuy != -1);
	ToggleDetour_BuyCommands(ForwardOnBuy != -1);

	// Search pev offset automatically.
	if (!G_OffsetHandler)
	{
		G_OffsetHandler = new OffsetHandler;
	}
}

void OnAmxxDetach()
{
	ShutdownHacks();
}
