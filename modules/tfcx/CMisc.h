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
// TFCX Module
//

#ifndef CMISC_H
#define CMISC_H

#include "amxxmodule.h"
#include "CRank.h"

#if defined(_WIN32)
	#define LINUXOFFSET         0
	#define PLAYER_LINUXOFFSET  0
	#define CLIP_LINUXOFFSET    0
#else
	#define LINUXOFFSET         4
	#define PLAYER_LINUXOFFSET  5
	#define CLIP_LINUXOFFSET    4
#endif

#define TFCMAX_CUSTOMWPNS   5
#define TFCMAX_WEAPONS      MAX_WEAPONS + TFCMAX_CUSTOMWPNS

#define PD_HAS_GOALITEM     26  + LINUXOFFSET
#define PD_GOALITEM_TEAM    36  + LINUXOFFSET
#define PD_SENTRY_OWNER     83	+ LINUXOFFSET
#define PD_TIMER_OWNER      932	+ PLAYER_LINUXOFFSET

// If somehow TFC updates, the following two offsets can be updated by
// disassembling CBaseEntity::KeyValuePartThree(KeyValueData_s *)
#define PD_REPLACE_MODEL    170 + LINUXOFFSET
#define PD_REPLACE_SKIN     172 + LINUXOFFSET


#define MAX_TRACE		13
//#define NADE_OFFSET		24

#define ACT_NADE_NONE		0
#define ACT_NADE_SHOT		1<<0
#define ACT_NADE_PUT		1<<1

#define PD_AMMO_SHELLS		53	+ LINUXOFFSET
#define PD_AMMO_BULLETS		55	+ LINUXOFFSET
#define PD_AMMO_CELLS		57	+ LINUXOFFSET
#define PD_AMMO_ROCKETS		59	+ LINUXOFFSET
#define PD_AMMO_NADE1		14	+ LINUXOFFSET
#define PD_AMMO_NADE2		15	+ LINUXOFFSET

#define PD_WEAPON_AMMO		307 + CLIP_LINUXOFFSET

// For tfc_get_user_goalitem
#define CARRYING_GOALITEM (1<<0)

enum {
	TFC_AMMO_SHELLS = 0,
	TFC_AMMO_BULLETS,
	TFC_AMMO_CELLS,
	TFC_AMMO_ROCKETS,
	TFC_AMMO_NADE1,
	TFC_AMMO_NADE2,
};

enum {
	TFC_WPN_NONE = 0,
	TFC_WPN_TIMER, // TFC_UNK_1,
	TFC_WPN_SENTRYGUN, //TFC_WPN_UNK2,
	TFC_WPN_MEDIKIT,
	TFC_WPN_SPANNER,
	TFC_WPN_AXE,
	TFC_WPN_SNIPERRIFLE,
	TFC_WPN_AUTORIFLE,
	TFC_WPN_SHOTGUN,
	TFC_WPN_SUPERSHOTGUN,
	TFC_WPN_NG,
	TFC_WPN_SUPERNG,
	TFC_WPN_GL,
	TFC_WPN_FLAMETHROWER,
	TFC_WPN_RPG,
	TFC_WPN_IC,
	TFC_WPN_FLAMES,//TFC_WPN_UNK16,
	TFC_WPN_AC,
	TFC_WPN_UNK18,
	TFC_WPN_UNK19,
	TFC_WPN_TRANQ,
	TFC_WPN_RAILGUN,
	TFC_WPN_PL,
	TFC_WPN_KNIFE,
	TFC_WPN_CALTROP, // 24
	TFC_WPN_CONCUSSIONGRENADE,
	TFC_WPN_NORMALGRENADE,
	TFC_WPN_NAILGRENADE,
	TFC_WPN_MIRVGRENADE,
	TFC_WPN_NAPALMGRENADE,
	TFC_WPN_GASGRENADE,
	TFC_WPN_EMPGRENADE,
};

enum {
	TFC_PC_SCOUT = 1,
	TFC_PC_SNIPER,
	TFC_PC_SOLDIER,
	TFC_PC_DEMOMAN,
	TFC_PC_MEDIC,
	TFC_PC_HWGUY,
	TFC_PC_PYRO,
	TFC_PC_SPY,
	TFC_PC_ENGENEER,	// Typo; preserved for backward compatibility
	TFC_PC_ENGINEER = 9,
	TFC_PC_CIVILIAN = 11,
};

struct weaponsVault {
  char name[32];
  char logName[32];
  short int ammoSlot;
  bool melee;
};

struct traceVault {
	const char* szName;
	char szTag[6];
	int iId;
	int iAction;
	float fDel;
	int start;
	int stop;
};



// *****************************************************
// class CPlayer
// *****************************************************

struct CPlayer {

	edict_t* pEdict;
	char ip[32];
	int index;
	int aiming;
	int current;

	bool ingame;
	bool bot;
	float clearStats;
	RankSystem::RankStats*	rank;

	struct PlayerWeapon : Stats {
		const char* name;
		int	ammo;
		int	clip;
	};

	PlayerWeapon	weapons[TFCMAX_WEAPONS];
	PlayerWeapon	attackers[33];
	PlayerWeapon	victims[33];
	Stats			weaponsRnd[TFCMAX_WEAPONS]; // DEC-Weapon (Round) stats
	Stats			life;

	int teamId;
	int classId;

	void Init(  int pi, edict_t* pe );
	void Connect(const char* ip );
	void PutInServer();
	void Disconnect();
	void saveKill(CPlayer* pVictim, int weapon, int hs, int tk);
	void saveHit(CPlayer* pVictim, int weapon, int damage, int aiming);
	void saveShot(int weapon);
	void restartStats(bool all = true);
	void killPlayer();
	inline bool IsBot(){
		const char* auth= (*g_engfuncs.pfnGetPlayerAuthId)(pEdict);
		return ( auth && !strcmp( auth , "BOT" ) );
	}
	inline bool IsAlive(){
		return ((pEdict->v.deadflag==DEAD_NO)&&(pEdict->v.health>0));
	}
};

// *****************************************************
// class Grenades
// *****************************************************

class Grenades
{
  struct Obj
  {
	CPlayer* player;
	edict_t* grenade;
	float time;
	int type;
	Obj* next;
	Obj* prev;
  } *head;


public:
  Grenades() { head = 0; }
  ~Grenades() { clear(); }
  void put( edict_t* grenade, float time, int type, CPlayer* player  );
  bool find( edict_t* enemy, CPlayer** p, int* type );
  void clear();
};

#endif // CMISC_H

