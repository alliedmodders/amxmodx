/*
 * dodfun 
 * Copyright (c) 2004 Lukasz Wlasinski
 *
 *
 *    This program is free software; you can redistribute it and/or modify it
 *    under the terms of the GNU General Public License as published by the
 *    Free Software Foundation; either version 2 of the License, or (at
 *    your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful, but
 *    WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software Foundation,
 *    Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *    In addition, as a special exception, the author gives permission to
 *    link the code of this program with the Half-Life Game Engine ("HL
 *    Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *    L.L.C ("Valve").  You must obey the GNU General Public License in all
 *    respects for all of the code used other than the HL Engine and MODs
 *    from Valve.  If you modify this file, you may extend this exception
 *    to your version of the file, but you are not obligated to do so.  If
 *    you do not wish to do so, delete this exception statement from your
 *    version.
 *
 */

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


