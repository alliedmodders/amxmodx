// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Fakemeta Module
//

#ifndef _FAKEMETA_INCLUDE_H
#define _FAKEMETA_INCLUDE_H

#include "amxxmodule.h"
#include <entity_state.h>
#include <usercmd.h>
#include "engfunc.h"
#include "dllfunc.h"
#include "pev.h"
#include "forward.h"
#include "fm_tr.h"
#include "glb.h"
#include <amtl/am-string.h>
#include <amtl/am-vector.h>
#include <IGameConfigs.h>
#include <HLTypeConversion.h>

#ifdef DONT_TOUCH_THIS_AGAIN_BAIL
#define CHECK_ENTITY(x) \
	if (x < 0 || x > gpGlobals->maxEntities) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity out of range (%d)", x); \
		return 0; \
	} else { \
		if (x <= gpGlobals->maxClients) { \
			if (!MF_IsPlayerIngame(x)) { \
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid player %d (not in-game)", x); \
				return 0; \
			} \
		} else { \
			if (x != 0 && FNullEnt(TypeConversion.id_to_edict(x))) { \
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid entity %d", x); \
				return 0; \
			} \
		} \
	}
#endif

#define CHECK_ENTITY(x) if (x != 0 && (FNullEnt(TypeConversion.id_to_edict(x)) || x < 0 || x > gpGlobals->maxEntities)) { MF_LogError(amx, AMX_ERR_NATIVE, "Invalid entity"); return 0; }
#define CHECK_OFFSET(x) if (x < 0) { MF_LogError(amx, AMX_ERR_NATIVE, "Invalid offset"); return 0; }

extern AMX_NATIVE_INFO engfunc_natives[];
extern AMX_NATIVE_INFO dllfunc_natives[];
extern AMX_NATIVE_INFO forward_natives[];
extern AMX_NATIVE_INFO pdata_natives[];
extern AMX_NATIVE_INFO pdata_entities_natives[];
extern AMX_NATIVE_INFO pdata_gamerules_natives[];
extern AMX_NATIVE_INFO tr_Natives[];
extern AMX_NATIVE_INFO pev_natives[];
extern AMX_NATIVE_INFO glb_natives[];
extern AMX_NATIVE_INFO misc_natives[];
extern TraceResult g_tr;

/* Wouldnt modifying the table AFTER it's memcpy'd be ... pointless?
extern enginefuncs_t g_EngineFuncs_Table;
extern enginefuncs_t g_EngineFuncs_Post_Table;
*/
// mark down pointers when the calls are made instead (this means amxxmodule.cpp needs to be changed slightly for this module)
extern DLL_FUNCTIONS *g_pFunctionTable;
extern DLL_FUNCTIONS *g_pFunctionTable_Post;
extern enginefuncs_t *g_pengfuncsTable;
extern enginefuncs_t *g_pengfuncsTable_Post;
extern NEW_DLL_FUNCTIONS *g_pNewFunctionsTable;
extern NEW_DLL_FUNCTIONS *g_pNewFunctionsTable_Post;

extern IGameConfig *CommonConfig;
extern IGameConfig *GamerulesConfig;
extern IGameConfigManager *ConfigManager;

extern HLTypeConversion TypeConversion;
extern void **GameRulesAddress;

#endif //_FAKEMETA_INCLUDE_H

