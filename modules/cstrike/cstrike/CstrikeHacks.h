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
void InitGlobalVars();
void ShutdownHacks();
void ToggleDetour_ClientCommands(bool enable);
void ToggleDetour_BuyCommands(bool enable);
void OnEmitSound(edict_t *entity, int channel, const char *sample, float volume, float attenuation, int fFlags, int pitch);

extern AMX_NATIVE_INFO CstrikeNatives[];

extern IGameConfig *MainConfig;
extern IGameConfig *CommonConfig;

extern int ForwardInternalCommand;
extern int ForwardOnBuy;
extern int ForwardOnBuyAttempt;

typedef edict_t* (*CreateNamedEntityFunc)(string_t iszClassname);
typedef void*    (*UTIL_FindEntityByStringFunc)(void* pStartEntity, const char *szKeyword, const char *szValue);
typedef WeaponInfoStruct* (*GetWeaponInfoFunc)(int id);

extern CreateNamedEntityFunc       CS_CreateNamedEntity;
extern UTIL_FindEntityByStringFunc CS_UTIL_FindEntityByString;
extern GetWeaponInfoFunc           GetWeaponInfo;

extern CDetour *GiveDefaultItemsDetour;
extern enginefuncs_t *g_pengfuncsTable;
extern DLL_FUNCTIONS *g_pFunctionTable;
extern bool NoKifesMode;

extern server_static_t *ServerStatic;
extern server_t *Server;

#endif // CSTRIKE_HACKS_H
