// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// CSX Module
//

#ifndef RANK_H
#define RANK_H

#include "amxxmodule.h"
#include "CMisc.h"
#include "CRank.h"

#define GET_PLAYER_POINTER(e)   (&players[ENTINDEX(e)])
#define GET_PLAYER_POINTER_I(i) (&players[i])

extern AMX_NATIVE_INFO stats_Natives[];

struct weaponsVault {
  char name[32];
  char logname[16];
  short int ammoSlot;
  bool used;
  bool melee;
};

extern bool rankBots;
extern cvar_t* csstats_rankbots;
extern cvar_t* csstats_pause;

extern int iFGrenade;
extern int iFDeath;
extern int iFDamage;

extern int iFBPlanted;
extern int iFBDefused;
extern int iFBPlanting;
extern int iFBDefusing;
extern int iFBExplode;

extern int g_bombAnnounce;
extern int g_Planter;
extern int g_Defuser;

#define BOMB_PLANTING	1
#define BOMB_PLANTED	2
#define BOMB_EXPLODE	3
#define BOMB_DEFUSING	4
#define BOMB_DEFUSED	5

extern weaponsVault weaponData[MAX_WEAPONS+MAX_CWEAPONS];

typedef void (*funEventCall)(void*);
extern funEventCall modMsgsEnd[MAX_REG_MSGS];
extern funEventCall modMsgs[MAX_REG_MSGS];

extern void (*function)(void*);
extern void (*endfunction)(void*);

extern cvar_t *csstats_maxsize;
extern cvar_t* csstats_rank;
extern cvar_t* csstats_reset;

extern Grenades g_grenades;

extern RankSystem g_rank;

extern CPlayer players[33];

extern CPlayer* mPlayer;

extern int mPlayerIndex;

extern int mState;

extern int gmsgCurWeapon;
extern int gmsgDamageEnd;
extern int gmsgDamage;
extern int gmsgWeaponList;
extern int gmsgResetHUD;
extern int gmsgAmmoX;
extern int gmsgScoreInfo;
extern int gmsgAmmoPickup;

extern int gmsgSendAudio;
extern int gmsgTextMsg;
extern int gmsgBarTime;

void Client_AmmoX(void*);
void Client_CurWeapon(void*);
void Client_Damage(void*);
void Client_WeaponList(void*);
void Client_AmmoPickup(void*);
void Client_Damage_End(void*);
void Client_ScoreInfo(void*);
void Client_ResetHUD(void*);
void Client_SendAudio(void*);
void Client_TextMsg(void*);
void Client_BarTime(void*);
void Client_DeathMsg(void*);

bool ignoreBots (edict_t *pEnt, edict_t *pOther = NULL );
bool isModuleActive();

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

#endif // RANK_H



