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
#define GETPLAYERAUTHID   (*g_engfuncs.pfnGetPlayerAuthId)
#endif

extern AMX_NATIVE_INFO stats_Natives[];
extern AMX_NATIVE_INFO base_Natives[];
extern AMX_NATIVE_INFO pd_Natives[];

struct weapon_t {
	bool needcheck;
	bool melee;
	char logname[16];
	char name[32];
};

extern bool rankBots;
extern int mState;
extern int mPlayerIndex;

void Client_CurWeapon(void*);
void Client_Health_End(void*);
void Client_ResetHUD_End(void*);
void Client_ObjScore(void*);
void Client_TeamScore(void*);
void Client_RoundState(void*);

typedef void (*funEventCall)(void*);

extern int AlliesScore;
extern int AxisScore;

extern int gmsgScoreShort;
extern int gmsgPTeam;

extern Forward g_death_info;
extern Forward g_damage_info;

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
edict_t *FindEntityByClassname(edict_t *pentStart, const char *szName);

#endif // DODX_H


