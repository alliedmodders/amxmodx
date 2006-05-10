#ifndef _FAKEMETA_INCLUDE_H
#define _FAKEMETA_INCLUDE_H

#include "sdk/amxxmodule.h"
#include <entity_state.h>
#include <usercmd.h>
#include "CVector.h"
#include "engfunc.h"
#include "dllfunc.h"
#include "pev.h"
#include "forward.h"
#include "fm_tr.h"
#include "glb.h"

extern edict_t *g_player_edicts[33];

inline edict_t* INDEXENT2( int iEdictNum )
{ 
	if (iEdictNum >= 1 && iEdictNum <= gpGlobals->maxClients)
		return MF_GetPlayerEdict(iEdictNum);
	else
		return (*g_engfuncs.pfnPEntityOfEntIndex)(iEdictNum); 
}
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
			if (x != 0 && FNullEnt(INDEXENT(x))) { \
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid entity %d", x); \
				return 0; \
			} \
		} \
	}
#endif

#define CHECK_ENTITY(x) if (x != 0 && (FNullEnt(INDEXENT2(x)) || x < 0 || x > gpGlobals->maxEntities)) { MF_LogError(amx, AMX_ERR_NATIVE, "Invalid entity"); return 0; }
extern AMX_NATIVE_INFO engfunc_natives[];
extern AMX_NATIVE_INFO dllfunc_natives[];
extern AMX_NATIVE_INFO forward_natives[];
extern AMX_NATIVE_INFO pdata_natives[];
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

#endif //_FAKEMETA_INCLUDE_H

