/*
 * dodfun 
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

#ifndef __linux__
#define LINUXOFFSET	0
#else
#define LINUXOFFSET	5
#endif

#define DODFUN_VERSION "0.1"

#define STEAM_PDOFFSET_WDEPLOY	229	 + LINUXOFFSET // weapon deploy
#define STEAM_PDOFFSET_CLASS		366  + LINUXOFFSET // player class
#define STEAM_PDOFFSET_RCLASS		367  + LINUXOFFSET // random class  
#define STEAM_PDOFFSET_SCORE		475  + LINUXOFFSET // score
#define STEAM_PDOFFSET_DEATHS		476  + LINUXOFFSET // deaths
#define STEAM_PDOFFSET_TEAMNAME	1396 + LINUXOFFSET // team name

#define WON_PDOFFSET_DEATHS		456  + LINUXOFFSET // deaths
#define WON_PDOFFSET_CLASS		511  + LINUXOFFSET // player class
#define WON_PDOFFSET_RCLASS		512  + LINUXOFFSET // random class
#define WON_PDOFFSET_WDEPLOY	605	 + LINUXOFFSET // weapon deploy
#define WON_PDOFFSET_SCORE		626	 + LINUXOFFSET // score
#define WON_PDOFFSET_TEAMNAME	1896 + LINUXOFFSET // team name


// *****************************************************
// class CPlayer
// *****************************************************

class CPlayer {

public:
	edict_t* pEdict;
	int index;
	int current;

	int staminaMin;
	int staminaMax;
	bool staminaSet;

	bool fuseSet;
	int fuseType; // 1<<0 - for new , 1<<1 - for catched
	float nadeFuse;

	bool ingame;
	bool bot;

	void Init(  int pi, edict_t* pe );
	void Connect();
	void PutInServer();
	void Disconnect();
	void killPlayer();
	void setTeamName( char *szName );

	inline bool IsBot(){
		const char* auth= (*g_engfuncs.pfnGetPlayerAuthId)(pEdict);
		return ( auth && !strcmp( auth , "BOT" ) );
	}
	inline bool IsAlive(){
		return ((pEdict->v.deadflag==DEAD_NO)&&(pEdict->v.health>0));
	}
};



#endif // CMISC_H