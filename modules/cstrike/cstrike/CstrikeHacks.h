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

#ifndef CSTRIKE_HACKS_H
#define CSTRIKE_HACKS_H

#include <amxxmodule.h>
#include <IGameConfigs.h>
#include <CDetour/detours.h>
#include <engine_strucs.h>
#include "CstrikeDatas.h"

void InitializeHacks();
void InitFuncsAddresses();
void InitClassMembers();
void InitGlobalVars();
void ShutdownHacks();

void CtrlDetours_ClientCommand(bool set);
void CtrlDetours_BuyCommands(bool set);
void CtrlDetours_Natives(bool set);

void ToggleDetour_ClientCommands(bool enable);
void ToggleDetour_BuyCommands(bool enable);

extern AMX_NATIVE_INFO CstrikeNatives[];

extern IGameConfig *MainConfig;
extern IGameConfig *CommonConfig;

extern int ForwardInternalCommand;
extern int ForwardOnBuy;
extern int ForwardOnBuyAttempt;

extern bool HasInternalCommandForward;
extern bool HasOnBuyAttemptForward;
extern bool HasOnBuyForward;

extern CDetour *ClientCommandDetour;
extern CDetour *GiveNamedItemDetour;
extern CDetour *AddAccountDetour;
extern CDetour *CanPlayerBuyDetour;
extern CDetour *CanBuyThisDetour;
extern CDetour *GiveDefaultItemsDetour;
extern CDetour *BuyGunAmmoDetour;

typedef edict_t* (*CreateNamedEntityFunc)(string_t iszClassname);
typedef void*    (*UTIL_FindEntityByStringFunc)(void* pStartEntity, const char *szKeyword, const char *szValue);
typedef WeaponInfoStruct* (*GetWeaponInfoFunc)(int id);

extern CreateNamedEntityFunc       CS_CreateNamedEntity;
extern UTIL_FindEntityByStringFunc CS_UTIL_FindEntityByString;
extern GetWeaponInfoFunc           GetWeaponInfo;

extern CDetour *GiveDefaultItemsDetour;
extern enginefuncs_t *g_pengfuncsTable;
extern DLL_FUNCTIONS *g_pFunctionTable;
extern bool NoKnivesMode;

extern server_static_t *ServerStatic;
extern server_t *Server;
extern void **GameRules;

extern int *UseBotArgs;
extern const char **BotArgs;

#endif // CSTRIKE_HACKS_H
