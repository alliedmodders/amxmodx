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
#include <resdk/cstrike/regamedll_api.h>

void InitializeHacks();
void InitFuncsAddresses();
void InitClassMembers();
void InitGlobalVars();
void ShutdownHacks();

void CtrlDetours_ClientCommand(bool set);
void CtrlDetours_BuyCommands(bool set);
void CtrlDetours_Natives(bool set);

void ToggleHook_ClientCommands(bool enable);
void ToggleHook_BuyCommands(bool enable);
void ToggleHook_GiveDefaultItems(bool enable);

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

enum class HashType
{
	Classname
};

typedef edict_t* (*CreateNamedEntityFunc)(string_t iszClassname);
typedef class CBaseEntity* (*UTIL_FindEntityByStringFunc)(class CBaseEntity *pStartEntity, const char *szKeyword, const char *szValue);
typedef WeaponInfoStruct* (*GetWeaponInfoFunc)(int id);
typedef void (*AddEntityHashValueFunc)(entvars_s *pev, const char *value, hash_types_e fieldType);
typedef void (*RemoveEntityHashValueFunc)(entvars_t *pev, const char *value, hash_types_e fieldType);

extern CreateNamedEntityFunc       CS_CreateNamedEntity;
extern UTIL_FindEntityByStringFunc CS_UTIL_FindEntityByString;
extern GetWeaponInfoFunc           GetWeaponInfo;
extern AddEntityHashValueFunc      AddEntityHashValue;
extern RemoveEntityHashValueFunc   RemoveEntityHashValue;

extern CDetour *GiveDefaultItemsDetour;
extern enginefuncs_t *g_pengfuncsTable;
extern DLL_FUNCTIONS *g_pFunctionTable;
extern bool NoKnivesMode;

extern server_static_t *ServerStatic;
extern server_t *Server;
extern void **GameRules;
extern void *GameRulesRH;

extern int *UseBotArgs;
extern const char **BotArgs;

extern bool HasReHlds;
extern bool HasReGameDll;

#endif // CSTRIKE_HACKS_H
