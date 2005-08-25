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

#define TSMAX_CUSTOMWPNS		5
#define TSMAX_WEAPONS			39 + TSMAX_CUSTOMWPNS


#ifndef __linux__
#define TSKNIFE_OFFSET			31 // owner offset in knife entity
#else
#define TSKNIFE_OFFSET			36
#endif

#define TSPWUP_SLOWMO			1
#define TSPWUP_INFAMMO			2
#define TSPWUP_KUNGFU			4
#define TSPWUP_SLOWPAUSE		8
#define TSPWUP_DFIRERATE		16
#define TSPWUP_SJUMP			256

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

	float set_454;
	float set_455;
	int set_423;

	edict_t* pEdict;
	char ip[32];
	int index;
	int teamId;
	int aiming;
	int current;

	int checkstate;
	int state;
	int oldstate;
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
	
	struct PlayerWeapon  {
		char*		name;
		int			ammo;
		int			clip;
		int			mode; // burst , full auto ..
		int			attach; // scope ,laser
	};

	PlayerWeapon	weapons[TSMAX_WEAPONS];
	PlayerWeapon	attackers[33];
	PlayerWeapon	victims[33];

	void Init(  int pi, edict_t* pe );
	void Connect( const char* ip );
	void PutInServer();
	void Disconnect();
	
	long GetOffsetInt(long off);
	float GetOffsetFloat(long off);

	void SetOffsetInt(long off,long set);
	void SetOffsetFloat(long off, float set);

	inline long GetMoney()
	{
		return GetOffsetInt(301);
	}

	inline void SetMoney(long money)
	{
		SetOffsetInt(301,money);
	}

	inline float GetCurrentTime()
	{
		return GetOffsetFloat(444);
	}

	inline void SetCurrentTime(float time)
	{
		 SetOffsetFloat(444,time);
	}

	inline long GetSlots()
	{
		return GetOffsetInt(304);
	}

	inline void SetSlots(long slots)
	{
		SetOffsetInt(304,slots);
	}

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

#endif // CMISC_H

