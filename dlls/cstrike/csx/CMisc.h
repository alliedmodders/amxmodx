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

#ifndef CMISC_H
#define CMISC_H

#include "amxxmodule.h"
#include "CRank.h"

#define MAX_CWEAPONS		6

#define CSW_HEGRENADE		4
#define CSW_C4				6
#define CSW_SMOKEGRENADE	9
#define CSW_KNIFE			29
#define CSW_FLASHBANG		25

// *****************************************************
// class CPlayer
// *****************************************************

struct CPlayer {
	edict_t* pEdict;
	char ip[32];
	int index;
	int aiming;
	int current;
	bool bot;
	float clearStats;
	RankSystem::RankStats*	rank;

	struct PlayerWeapon : Stats {
		const char* name;
		int	ammo;
		int	clip;
	};

	PlayerWeapon	weapons[MAX_WEAPONS+MAX_CWEAPONS];
	PlayerWeapon	attackers[33];
	PlayerWeapon	victims[33];
	Stats			weaponsRnd[MAX_WEAPONS+MAX_CWEAPONS]; // DEC-Weapon (Round) stats
	Stats			life;

	int teamId;

	void Init(  int pi, edict_t* pe );
	void Connect(const char* ip );
	void PutInServer();
	void Disconnect();
	void saveKill(CPlayer* pVictim, int weapon, int hs, int tk);
	void saveHit(CPlayer* pVictim, int weapon, int damage, int aiming);
	void saveShot(int weapon);

	void saveBPlant();
	void saveBExplode();
	void saveBDefusing();
	void saveBDefused();

	void restartStats(bool all = true);
	inline bool IsBot(){
		const char* auth= (*g_engfuncs.pfnGetPlayerAuthId)(pEdict);
		return ( (auth && !strcmp( auth , "BOT" )) || // AuthID of "BOT"
				 (pEdict->v.flags & FL_FAKECLIENT));  // FL_FAKECLIENT flag set
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



