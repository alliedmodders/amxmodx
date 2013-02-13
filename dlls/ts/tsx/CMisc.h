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

#ifndef CMISC_H
#define CMISC_H

#include "amxxmodule.h"
#include "CRank.h"

#define TSMAX_CUSTOMWPNS		5
#define TSMAX_WEAPONS			39 + TSMAX_CUSTOMWPNS


#if defined(_WIN32)
#define TSKNIFE_OFFSET			31 // owner offset in knife entity
#else
#define TSKNIFE_OFFSET			35
#endif

#define TSPWUP_SLOWMO			1
#define TSPWUP_INFAMMO			2
#define TSPWUP_KUNGFU			4
#define TSPWUP_SLOWPAUSE		8
#define TSPWUP_DFIRERATE		16
#define TSPWUP_SJUMP			256

#if defined(__linux__) || defined(__APPLE__)
#define EXTRAOFFSET	5
#else
#define EXTRAOFFSET 0
#endif

#define TSX_SDUCK_OFFSET		(27+EXTRAOFFSET)
#define TSX_SPEED1_OFFSET		(85+EXTRAOFFSET)
#define TSX_PHYSICS_OFFSET		(86+EXTRAOFFSET)
#define TSX_BTRAIL_OFFSET		(87+EXTRAOFFSET)
#define TSX_ISSLO_OFFSET		(89+EXTRAOFFSET)
#define TSX_SPEED2_OFFSET		(90+EXTRAOFFSET)
#define TSX_SROLL_OFFSET		(158+EXTRAOFFSET)
#define TSX_MONEY_OFFSET		(301+EXTRAOFFSET)
#define TSX_SLOTS_OFFSET		(304+EXTRAOFFSET)
#define TSX_SLOMO1_OFFSET		(423+EXTRAOFFSET)
#define TSX_SLOMO2_OFFSET		(425+EXTRAOFFSET)
#define TSX_MSG_OFFSET			(433+EXTRAOFFSET)
#define TSX_TIME_OFFSET			(444+EXTRAOFFSET)

#define TSITEM_KUNGFU			1<<0
#define TSITEM_SUPERJUMP		1<<1

#define TSKF_STUNTKILL			1<<0
#define TSKF_SLIDINGKILL		1<<1
#define TSKF_DOUBLEKILL			1<<2
#define TSKF_ISSPEC				1<<3 // is specialist
#define TSKF_KILLEDSPEC			1<<4 // killed specialist

// *****************************************************
// class CPlayer
// *****************************************************

struct CPlayer {

	edict_t* pEdict;
	char ip[32];
	int index;
	int teamId;
	int aiming;
	int current;

	int space;
	int money;
	int PwUp;
	int PwUpValue;
	int items; // "stale" przedmioty , super jump i kung fu bonus

	//
	int killingSpree;
	int is_specialist;
	int killFlags;
	int lastFrag; // oblicz ostatni frag, sprawdza czy poprawna jest detekcja broni i bonusow 
	float lastKill;  // kiedy ostatni , dla double kill
	//
	int frags; // suma dla kontroli ostatniego fraga, to - v.frags = lastfrag

	bool ingame;
	float clearStats;

	int checkstate;
	int state;
	int oldstate;
	
	struct PlayerWeapon : public Stats {
		char*		name;
		int			ammo;
		int			clip;
		int			mode; // burst , full auto ..
		int			attach; // scope ,laser
	};

	PlayerWeapon	weapons[TSMAX_WEAPONS];
	PlayerWeapon	attackers[33];
	PlayerWeapon	victims[33];
	Stats			weaponsRnd[TSMAX_WEAPONS]; // DEC-Weapon (Round) stats
	Stats			life;

	RankSystem::RankStats*	rank;

	void Init(  int pi, edict_t* pe );
	void Connect( const char* ip );
	void PutInServer();
	void Disconnect();
	void saveKill(CPlayer* pVictim, int weapon, int hs, int tk);
	void saveHit(CPlayer* pVictim, int weapon, int damage, int aiming);
	void saveShot(int weapon);
	void restartStats(bool all = true);
	void SetMoney(int money);
	void SetSlots(int slots);
	int GetSlots();
	void SetOffset(int offs, int val);
	int GetOffset(int offs);
	void SetOffsetF(int offs, float val);
	float GetOffsetF(int offs);
	float GetTime();

	inline bool IsBot(){
		const char* auth= (*g_engfuncs.pfnGetPlayerAuthId)(pEdict);
		return ( auth && !strcmp( auth , "BOT" ) );
	}
	inline bool IsAlive(){
		return ((pEdict->v.deadflag==DEAD_NO)&&(pEdict->v.health>0));
	}
};

#endif // CMISC_H

