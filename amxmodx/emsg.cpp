/* AMX Mod X
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

#include "amxmodx.h"

int gmsgAmmoPickup;
int gmsgAmmoX;
int gmsgBattery;
int gmsgCurWeapon;
int gmsgDamage;
int gmsgDeathMsg;
int gmsgHealth;
int gmsgMOTD;
int gmsgScoreInfo;
int gmsgSendAudio;
int gmsgServerName;
int gmsgShowMenu;
int gmsgTeamInfo;
int gmsgTextMsg;
int gmsgVGUIMenu;
int gmsgWeapPickup;
int gmsgWeaponList;
int gmsgintermission;
int gmsgResetHUD;
int gmsgRoundTime;

TeamIds g_teamsIds;
WeaponsVault g_weaponsData[MAX_WEAPONS];

void Client_VGUIMenu(void* mValue)
{
  if (!mPlayer) return;
  switch (mState++){
  case 0:
    mPlayer->menu = -(*(int*)mValue);
    break;
  case 1:
    mPlayer->keys = *(int*)mValue;
  }
}

void Client_ShowMenu(void* mValue)
{
  if (!mPlayer) return;
  switch (mState++){
  case 0:
    mPlayer->keys = *(int*)mValue;
    break;
  case 3:
    mPlayer->menu = g_menucmds.findMenuId( (char*)mValue );
  }
}

void Client_TeamInfo(void* mValue)
{
  if (mPlayer) return;
  static int index;
  switch (mState++) {
  case 0:
    index = *(int*)mValue;
    break;
  case 1:
	if ( index < 1 || index > gpGlobals->maxClients ) break;
	char* msg = (char*)mValue;
    g_players[ index ].team.assign( msg );
	g_teamsIds.registerTeam( msg , -1 );
  }
}

void Client_TextMsg(void* mValue)
{
	if ( mPlayer ) return;
	switch (mState++) {
	case 1:{
		char * msg = (char*)mValue;
		if (!msg) break;
		if ( !strncmp("#Game_C", msg , 7) ) {
			g_game_timeleft = g_game_restarting = gpGlobals->time + 3;
			//      g_endround_time = gpGlobals->time;
			//      g_newround_time = gpGlobals->time + CVAR_GET_FLOAT("mp_freezetime") + 3;
		}
		else if (!strncmp("#Game_w", msg , 7) ) {
			g_game_timeleft = -2;
		}
		else if ( !strncmp("#game_clan_s", msg , 12)  ){
			g_game_timeleft = -3;
		}
		break;
		   }
	case 2:{
		char * msg = (char*)mValue;
		if (!msg) break;
		if (g_game_timeleft == -2 ){
			g_game_timeleft = g_game_restarting = gpGlobals->time + atoi( msg );
			//    g_newround_time = g_game_timeleft + CVAR_GET_FLOAT("mp_freezetime");
		}
		else if ( g_game_timeleft == -3 )
			g_game_restarting = atoi( msg ) * 60.0f;
		break;
		   }
	case 3:{
		char * msg = (char*)mValue;
		if (!msg) break;
		if ( g_game_timeleft != -3 ) break;
		g_game_restarting += atoi( msg );
		g_game_timeleft = g_game_restarting	= gpGlobals->time + g_game_restarting;
		break;
		   }
	}
	
}

void Client_WeaponList(void* mValue)
{
  static int wpnList = 0;
  //static int wpnList2;
  static int iSlot;
  static const char* wpnName;
  switch (mState++) {
  case 0:
    wpnName = (char*)mValue;
    break;
  case 1:
    iSlot = *(int*)mValue;
    break;
  case 7:
    int iId = *(int*)mValue;
    if ( (iId < 0 || iId >= MAX_WEAPONS ) || (wpnList & (1<<iId)) )
      break;
    wpnList |= (1<<iId);
    g_weaponsData[iId].iId = iId;
    g_weaponsData[iId].ammoSlot = iSlot;
    g_weaponsData[iId].fullName.assign(wpnName);

  }
}

void Client_CurWeapon(void* mValue)
{
  static int iState;
  static int iId;
  switch (mState++){
  case 0:
    iState = *(int*)mValue;
    break;
  case 1:
    if (!iState) break;
    iId = *(int*)mValue;
    break;
  case 2:
  if (!mPlayer) return;
    if (!iState || (iId < 1 || iId >= MAX_WEAPONS ) ) break;
    mPlayer->weapons[iId].clip = *(int*)mValue;
    mPlayer->current = iId;
    mPlayer->lastHit = mPlayer->lastTrace;
  }
}

void Client_AmmoX(void* mValue)
{

  static int iAmmo;
  switch (mState++){
  case 0:
    iAmmo = *(int*)mValue;
    break;
  case 1:
  if (!mPlayer) return;
    for(int i=1;i<MAX_WEAPONS;++i)
    if (iAmmo == g_weaponsData[i].ammoSlot)
        mPlayer->weapons[i].ammo = *(int*)mValue;
  }
}

void Client_AmmoPickup(void* mValue)
{
  static int iSlot;
  switch (mState++){
  case 0:
    iSlot = *(int*)mValue;
    break;
  case 1:
  if (!mPlayer) return;
    for(int i=1;i<MAX_WEAPONS;++i)
    if (g_weaponsData[i].ammoSlot==iSlot)
        mPlayer->weapons[i].ammo += *(int*)mValue;
  }
}

void Client_ScoreInfo(void* mValue)
{
  static int index;
  static int deaths;
  switch (mState++){
  case 0:
    index = *(int*)mValue;
    break;
  case 2:
    deaths = *(int*)mValue;
    break;
  case 4:
	if ( index < 1 || index > gpGlobals->maxClients ) break;
    CPlayer*pPlayer = GET_PLAYER_POINTER_I( index );
    pPlayer->deaths = deaths;
    pPlayer->teamId = *(int*)mValue;
    if ( g_teamsIds.isNewTeam() )
      g_teamsIds.registerTeam( pPlayer->team.c_str() , pPlayer->teamId );
  }
}

void Client_DamageEnd(void* mValue)
{
	CPlayer* dead = mPlayer;

	if ( dead && dead->death_killer )
	{
		g_events.parserInit( CS_DEATHMSG , &gpGlobals->time , mPlayer = 0, mPlayerIndex = 0 );
		g_events.parseValue( dead->death_killer  );
		g_events.parseValue( dead->index  );
		g_events.parseValue( dead->death_headshot  );
		g_events.parseValue( dead->death_weapon.c_str()  );
		g_events.parseValue( dead->death_tk ? 1 : 0  );
		g_events.executeEvents();
		dead->death_killer = 0;
	}
}

void Client_DeathMsg(void* mValue)
{
	static CPlayer *killer;
	static CPlayer *victim;
	static int killer_id;
	static int victim_id;
	static int hs;
	
	switch (mState++){
	case 0:
		killer_id = *(int*)mValue;
		killer = (killer_id > 0 && killer_id < 33) ? 
			GET_PLAYER_POINTER_I(killer_id) : 0;
		break;
	case 1:
		victim_id = *(int*)mValue;
		victim = (victim_id > 0 && victim_id < 33) ? 
			GET_PLAYER_POINTER_I(victim_id) : 0;
		break;
	case 2:	
		hs = *(int*)mValue;
		break;
	case 3:
		
		if (  !killer || !victim ) break;

		victim->death_killer = killer_id;
		victim->death_weapon.assign((char*)mValue);
		victim->death_headshot = hs;
		victim->death_tk = (killer->teamId == victim->teamId);
	}
}
/*
void Client_SendAudio(void* mValue)
{

}

void Client_SendAudioEnd(void* mValue)
{


}

void Client_RoundTimeEnd(void* mValue)
{

}

void Client_RoundTime(void* mValue)
{

}


void Client_ResetHUD(void* mValue)
{

}
*/

