/*
 * DoDX
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

#ifndef DODX_H
#define DODX_H

#include "amxxmodule.h"
#include "CMisc.h"
#include "CRank.h"

#define GET_PLAYER_POINTER(e)   (&players[ENTINDEX(e)])
#define GET_PLAYER_POINTER_I(i) (&players[i])

#ifndef GETPLAYERAUTHID
	#define GETPLAYERAUTHID		(*g_engfuncs.pfnGetPlayerAuthId)
#endif

extern AMX_NATIVE_INFO stats_Natives[];
extern AMX_NATIVE_INFO base_Natives[];
extern AMX_NATIVE_INFO pd_Natives[];

// Weapons grabbing by type
enum
{
	DODWT_PRIMARY = 0,
	DODWT_SECONDARY,
	DODWT_MELEE,
	DODWT_GRENADE, 
	DODWT_OTHER
};

// Model Sequences
enum
{
	DOD_SEQ_PRONE_IDLE = 15,
	DOD_SEQ_PRONE_FORWARD,
	DOD_SEQ_PRONE_DOWN,
	DOD_SEQ_PRONE_UP
};

// Weapons Structure
struct weapon_t 
{
	bool needcheck;
	bool melee;
	char logname[16];
	char name[32];
	int ammoSlot;
	int type;
};

struct weaponlist_s
{
	int grp;
	int bitfield;
	int clip;
	bool changeable;
};

extern bool rankBots;
extern int mState;
extern int mDest;
extern int mCurWpnEnd;
extern int mPlayerIndex;

void Client_CurWeapon(void*);
void Client_CurWeapon_End(void*);
void Client_Health_End(void*);
void Client_ResetHUD_End(void*);
void Client_ObjScore(void*);
void Client_TeamScore(void*);
void Client_RoundState(void*);
void Client_AmmoX(void*);
void Client_AmmoShort(void*);
void Client_SetFOV(void*);
void Client_SetFOV_End(void*);
void Client_Object(void*);
void Client_Object_End(void*);
void Client_PStatus(void*);

typedef void (*funEventCall)(void*);

extern int AlliesScore;
extern int AxisScore;

extern int gmsgCurWeapon;
extern int gmsgCurWeaponEnd;
extern int gmsgHealth;
extern int gmsgResetHUD;
extern int gmsgObjScore;
extern int gmsgRoundState;
extern int gmsgTeamScore;
extern int gmsgScoreShort;
extern int gmsgPTeam;
extern int gmsgAmmoX;
extern int gmsgAmmoShort;
extern int gmsgSetFOV;
extern int gmsgSetFOV_End;
extern int gmsgObject;
extern int gmsgObject_End;
extern int gmsgPStatus;

extern int iFDamage;
extern int iFDeath;
extern int iFScore;
extern int iFSpawnForward;
extern int iFTeamForward;
extern int iFClassForward;
extern int iFScopeForward;
extern int iFProneForward;
extern int iFWpnPickupForward;
extern int iFCurWpnForward;
extern int iFGrenadeExplode;
extern int iFRocketExplode;
extern int iFObjectTouched;
extern int iFStaminaForward;

extern cvar_t* dodstats_maxsize;
extern cvar_t* dodstats_rank;
extern cvar_t* dodstats_reset;
extern cvar_t* dodstats_rankbots;
extern cvar_t* dodstats_pause;

extern weapon_t weaponData[DODMAX_WEAPONS];
extern traceVault traceData[MAX_TRACE];

extern Grenades g_grenades;
extern RankSystem g_rank;
extern CPlayer players[33];
extern CPlayer* mPlayer;
extern CMapInfo g_map;

int get_weaponid(CPlayer* player);
bool ignoreBots (edict_t *pEnt, edict_t *pOther = NULL );
bool isModuleActive();
edict_t *FindEntityByString(edict_t *pentStart, const char *szKeyword, const char *szValue);
edict_t *FindEntityByClassname(edict_t *pentStart, const char *szName);
edict_t *FindEntityInSphere(edict_t *pentStart, edict_t *origin, float radius);

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

#define CHECK_PLAYERRANGE(x) \
	if (x > gpGlobals->maxClients || x < 0) \
	{ \
		MF_LogError(amx, AMX_ERR_NATIVE, "Player out of range (%d)", x); \
		return 0; \
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

#endif // DODX_H
