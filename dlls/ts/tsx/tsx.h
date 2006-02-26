/*
 * Copyright (c) 2003-2004 Lukasz Wlasinski
 *
 *    This file is part of TS XMod.
 *
 *    TS XMod is free software; you can redistribute it and/or modify it
 *    under the terms of the GNU General Public License as published by the
 *    Free Software Foundation; either version 2 of the License, or (at
 *    your option) any later version.
 *
 *    TS XMod is distributed in the hope that it will be useful, but
 *    WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with TS XMod; if not, write to the Free Software Foundation,
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

#ifndef TSX_H
#define TSX_H

#include "amxxmodule.h"
#include "CMisc.h"
#include "CRank.h"

#define RANK_SVERSION "0.2"

#define GET_PLAYER_POINTER(e)   (&players[ENTINDEX(e)])
#define GET_PLAYER_POINTER_I(i) (&players[i])

#ifndef GETPLAYERAUTHID
#define GETPLAYERAUTHID   (*g_engfuncs.pfnGetPlayerAuthId)
#endif

#define STUNT_NONE 0
#define STUNT_DUCK 1
#define STUNT_ROLL 2
#define STUNT_DIVE 3
#define STUNT_GETUP 4
#define STUNT_FLIP 5

extern AMX_NATIVE_INFO stats_Natives[];
extern AMX_NATIVE_INFO base_Natives[];

extern int gmsgResetHUD;
extern int gmsgWeaponInfo;
extern int gmsgClipInfo;
extern int gmsgScoreInfo;
extern int gmsgTSHealth;
extern int gmsgTSState;

extern int gmsgWStatus;
extern int gmsgTSCash;
extern int gmsgTSSpace;
extern int gmsgPwUp;

void Client_ResetHUD_End(void*);
void Client_WeaponInfo(void*);
void Client_ClipInfo(void*);
void Client_ScoreInfo(void*);
void Client_TSHealth_End(void*);
void Client_TSState(void*);
void Client_WStatus(void* mValue);
void Client_TSCash(void* mValue);
void Client_TSSpace(void* mValue);
void Client_PwUp(void* mValue);

typedef void (*funEventCall)(void*);
extern funEventCall modMsgsEnd[MAX_REG_MSGS];
extern funEventCall modMsgs[MAX_REG_MSGS];
extern void (*function)(void*);
extern void (*endfunction)(void*);

extern cvar_t *tsstats_maxsize;
extern cvar_t* tsstats_rank;
extern cvar_t* tsstats_reset;

extern RankSystem g_rank;
extern CPlayer players[33];

extern CPlayer* mPlayer;
extern int mPlayerIndex;
extern int mState;

extern bool is_theonemode;
extern bool rankBots;

extern int g_death_info;
extern int g_damage_info;

struct weapon_t {
	bool melee; // 
	char name[24];
	char logname[24];
	int bonus;
	bool custom;
};

extern int gKnifeOffset;

extern weapon_t weaponData[TSMAX_WEAPONS];

bool isModuleActive();
bool ignoreBots (edict_t *pEnt, edict_t *pOther = NULL);

#endif //TSX_H


