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

#ifndef CMISC_H
#define CMISC_H

#include "CRank.h"

#define DODMAX_CUSTOMWPNS	5	// custom weapons
#define DODMAX_WEAPONS		42 + DODMAX_CUSTOMWPNS

#if defined(_WIN32)
#define LINUXOFFSET	0
#else
#define LINUXOFFSET	5
#endif

#define DOD_VERSION "0.1"

#define MAX_TRACE	6

struct traceVault 
{
	char szName[16];
	int iId;
	int iAction;
	float fDel;
};

#define ACT_NADE_NONE		(0)
#define ACT_NADE_SHOT		(1<<0)
#define ACT_NADE_PUT		(1<<1)
#define ACT_NADE_THROW		(1<<2)

#define ACT_ROCKET_NONE		(0)
#define ACT_ROCKET_SHOT		(1<<0)
#define ACT_ROCKET_PUT		(1<<3)


// *****************************************************
// class CPlayer
// *****************************************************

class CPlayer 
{
	private:
		char ip[32];

	public:
		edict_t* pEdict;
		int index;
		int aiming;
		int current;
		int old;
		int wpnModel;
		int wpnscount;
		int wpns_bitfield;
		int old_weapons[DODMAX_WEAPONS];

		float savedScore;
		int lastScore;
		int sendScore;

		bool ingame;
		bool bot;
		float clearStats;
		float clearRound;

		int oldteam;
		int oldclass;
		float oldstamina;

		struct ModelStruct
		{
			int body_num;
			bool is_model_set;
			char modelclass[64];
		}
		sModel;

		int oldprone;
		bool do_scoped;
		bool is_scoped;

		struct ObjectStruct
		{
			edict_t* pEdict;
			bool carrying;
			bool do_forward;
			int type;
		}
		object;

		struct PlayerWeapon : public Stats 
		{
			char*		name;
			int			ammo;
			int			clip;
		};

		PlayerWeapon	weapons[DODMAX_WEAPONS];
		PlayerWeapon	attackers[33];
		PlayerWeapon	victims[33];
		Stats			weaponsLife[DODMAX_WEAPONS]; // DEC-Weapon (Life) stats
		Stats			weaponsRnd[DODMAX_WEAPONS]; // DEC-Weapon (Round) stats
		Stats			life;
		Stats			round;

		RankSystem::RankStats*	rank;

		void Init(  int pi, edict_t* pe );
		void Connect(const char* name,const char* ip );
		void PutInServer();
		void Disconnect();
		void saveKill(CPlayer* pVictim, int weapon, int hs, int tk);
		void saveHit(CPlayer* pVictim, int weapon, int damage, int aiming);
		void saveShot(int weapon);
		void updateScore(int weapon, int score);
		void restartStats(bool all = true);
		void killPlayer();
		void initModel(char*);
		void clearModel();
		bool setModel();
		void setBody(int);
		void PreThink();
		void Scoping(int);
		void ScopingCheck();
		void WeaponsCheck(int);

		inline bool IsBot()
		{
			const char* auth= (*g_engfuncs.pfnGetPlayerAuthId)(pEdict);
			return ( auth && !strcmp( auth , "BOT" ) );
		}

		inline bool IsAlive()
		{
			return ((pEdict->v.deadflag==DEAD_NO)&&(pEdict->v.health>0));
		}
};

// *****************************************************
// class Grenades
// *****************************************************
class Grenades // : public CObject
{
  struct Obj
  {
    CPlayer* player;
    edict_t* grenade;
    float time;
    int type;
    Obj* next;
  } *head;


public:
	Grenades() { head = 0; }
	~Grenades() { clear(); }
	void put(edict_t* grenade, float time, int type, CPlayer* player);
	bool find(edict_t* enemy, CPlayer** p, int& type);
	void clear();
};

// *****************************************************
// class CMapInfo
// *****************************************************

class CMapInfo
{
public:
	edict_t* pEdict;
	bool initialized;

	int detect_axis_paras;
	int detect_allies_paras;
	int detect_allies_country;

	void Init();
};



#endif // CMISC_H

