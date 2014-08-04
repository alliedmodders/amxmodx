// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2004 Lukasz Wlasinski.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// DoD Fun Module
//

#ifndef DODFUN_H
#define DODFUN_H

#include "amxxmodule.h"
#include "CMisc.h"

#define GET_PLAYER_POINTER(e)   (&players[ENTINDEX(e)])
#define GET_PLAYER_POINTER_I(i) (&players[i])

extern AMX_NATIVE_INFO base_Natives[];
extern AMX_NATIVE_INFO pd_Natives[];

extern int mState;
extern int mDest;
extern int mPlayerIndex;

void Client_CurWeapon(void*);
void Client_InitObj(void*);
void Client_SetObj(void*);

typedef void (*funEventCall)(void*);

extern int gmsgScoreShort;
extern int gmsgPTeam;
extern int gmsgInitObj;
extern int gmsgSetObj;
extern int gmsgFrags;
extern int gmsgObjScore;

extern int iFGrenade;
extern int iFRocket;
extern int iFInitCP;

extern CPlayer players[33];
extern CPlayer* mPlayer;

extern CObjective mObjects;

edict_t *FindEntityByClassname(edict_t *pentStart, const char *szName);
edict_t *FindEntityByString(edict_t *pentStart, const char *szKeyword, const char *szValue);

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

#define CHECK_PLAYER(x) \
	if (x < 1 || x > gpGlobals->maxClients) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Player out of range (%d)", x); \
		return 0; \
	} else { \
		if (!MF_IsPlayerIngame(x) || FNullEnt(MF_GetPlayerEdict(x))) { \
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid player %d", x); \
			return 0; \
		} \
	}

#define CHECK_NONPLAYER(x) \
	if (x < 1 || x <= gpGlobals->maxClients || x > gpGlobals->maxEntities) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Non-player entity %d out of range", x); \
		return 0; \
	} else { \
		if (FNullEnt(INDEXENT(x))) { \
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid non-player entity %d", x); \
			return 0; \
		} \
	}

#define GETEDICT(n) \
	((n >= 1 && n <= gpGlobals->maxClients) ? MF_GetPlayerEdict(n) : INDEXENT(n))


#define GET_CAPTURE_AREA(x) \
	if ( mObjects.obj[x].areaflags == 0 ){\
		mObjects.obj[x].areaflags = 1;\
		while ( (mObjects.obj[x].pAreaEdict = FindEntityByString(mObjects.obj[x].pAreaEdict,"target",STRING(mObjects.obj[x].pEdict->v.targetname))) )\
			if ( strcmp( STRING(mObjects.obj[x].pAreaEdict->v.classname),"dod_capture_area" )==0){\
				mObjects.obj[x].areaflags = 2;\
				break;\
			}\
	}\
	if ( mObjects.obj[x].areaflags == 1 )\
		return 0;

#endif // DODFUN_H


