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

#include "amxxmodule.h"
#include "dodx.h"

static cell AMX_NATIVE_CALL get_weapon_name(AMX *amx, cell *params){ // from id to name 3 params id, name, len
	int id = params[1];
	if (id<0 || id>=DODMAX_WEAPONS){ 
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	return MF_SetAmxString(amx,params[2],weaponData[id].name,params[3]);
}

static cell AMX_NATIVE_CALL wpnlog_to_name(AMX *amx, cell *params){ // from log to name
	int iLen;
	char *log = MF_GetAmxString(amx,params[1],0,&iLen);
	int i;
	for ( i=0; i<DODMAX_WEAPONS; i++ ){
		if ( strcmp(log,weaponData[i].logname ) == 0 )
			return MF_SetAmxString(amx,params[2],weaponData[i].name,params[3]);
	}
	return 0;
}

static cell AMX_NATIVE_CALL wpnlog_to_id(AMX *amx, cell *params){ // from log to id
	int iLen;
	char *log = MF_GetAmxString(amx,params[1],0,&iLen);

	int i;
	for ( i=0; i<DODMAX_WEAPONS; i++ ){
		if ( strcmp(log,weaponData[i].logname) == 0 )
			return i;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_weapon_logname(AMX *amx, cell *params){ // from id to log
	int id = params[1];
	if (id<0 || id>=DODMAX_WEAPONS){ 
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	return MF_SetAmxString(amx,params[2],weaponData[id].logname,params[3]);
}

static cell AMX_NATIVE_CALL is_melee(AMX *amx, cell *params){
	int id = params[1];
	if (id<0 || id>=DODMAX_WEAPONS){ 
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	return weaponData[id].melee;
}

static cell AMX_NATIVE_CALL get_team_score(AMX *amx, cell *params){
	int index = params[1];
	switch ( index ){
	case 1:
		return AlliesScore;
		break;
	case 2:
		return AxisScore;
		break;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_user_score(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return -1; 
	}
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if (pPlayer->ingame)
		return pPlayer->savedScore; 
	return -1;
}

static cell AMX_NATIVE_CALL get_user_class(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if (pPlayer->ingame)
		return pPlayer->pEdict->v.playerclass;
	return 0;
}

static cell AMX_NATIVE_CALL user_kill(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame && pPlayer->IsAlive() ){
		pPlayer->killPlayer();
		return 1;
	}

	return 0;
}	

static cell AMX_NATIVE_CALL get_map_info(AMX *amx, cell *params){
	switch( params[1] ){
	case 0:
		return g_map.detect_allies_country;
		break;
	case 1:
		return g_map.detect_allies_paras;
		break;
	case 2:
		return g_map.detect_axis_paras;
		break;
	default:
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		break;
	}
	return -1;
}

static cell AMX_NATIVE_CALL get_user_pronestate(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if (pPlayer->ingame)
			return pPlayer->pEdict->v.iuser3;

	return 0;
}

static cell AMX_NATIVE_CALL get_user_weapon(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if (pPlayer->ingame){
		int wpn = pPlayer->current;
		cell *cpTemp = MF_GetAmxAddr(amx,params[2]);
		*cpTemp = pPlayer->weapons[wpn].clip;
		cpTemp = MF_GetAmxAddr(amx,params[3]);
		*cpTemp = pPlayer->weapons[wpn].ammo;
		return wpn;
	}

	return 0;
}

static cell AMX_NATIVE_CALL register_forward(AMX *amx, cell *params){ // forward 

#ifdef FORWARD_OLD_SYSTEM

	int iFunctionIndex;
	switch( params[1] ){
	case 0:
		if( MF_AmxFindPublic(amx, "client_damage", &iFunctionIndex) == AMX_ERR_NONE )
			g_damage_info.put( amx , iFunctionIndex );
		else
			MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
			return 0;
		break;
	case 1:
		if( MF_AmxFindPublic(amx, "client_death", &iFunctionIndex) == AMX_ERR_NONE )
			g_death_info.put( amx , iFunctionIndex );
		else
			MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
			return 0;
		break;
	default:
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
#endif
	return 1;
}

AMX_NATIVE_INFO base_Natives[] = {

  { "dod_get_wpnname", get_weapon_name },
  { "dod_get_wpnlogname", get_weapon_logname },
  { "dod_wpnlog_to_name", wpnlog_to_name },
  { "dod_wpnlog_to_id", wpnlog_to_id },
  { "dod_is_melee", is_melee },

  { "dod_get_team_score", get_team_score },
  { "dod_get_user_score", get_user_score },
  { "dod_get_user_class", get_user_class },
  { "dod_get_user_weapon", get_user_weapon },

  { "dod_get_map_info", get_map_info },
  { "dod_user_kill", user_kill },
  { "dod_get_pronestate", get_user_pronestate },

  {"register_statsfwd",register_forward },

  ///*******************
  { NULL, NULL } 
};