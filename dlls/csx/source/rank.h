


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

bool ignoreBots (edict_t *pEnt, edict_t *pOther = NULL );
bool isModuleActive();

#endif // RANK_H



