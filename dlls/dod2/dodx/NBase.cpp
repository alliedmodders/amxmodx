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
	case 2:
		if( MF_AmxFindPublic(amx, "client_score", &iFunctionIndex) == AMX_ERR_NONE )
			g_score_info.put( amx , iFunctionIndex );
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

static cell AMX_NATIVE_CALL register_cwpn(AMX *amx, cell *params){ // name,logname,melee=0 
	int i;
	bool bFree = false;
	for ( i=DODMAX_WEAPONS-DODMAX_CUSTOMWPNS;i<DODMAX_WEAPONS;i++){
		if ( !weaponData[i].needcheck ){
			bFree = true;
			break;
		}
	}

	if ( !bFree )
		return 0;

	int iLen;
	char *szName = MF_GetAmxString(amx, params[1], 0, &iLen);
	char *szLogName = MF_GetAmxString(amx, params[3], 0, &iLen);

	strcpy(weaponData[i].name,szName);
	strcpy(weaponData[i].logname,szLogName);
	weaponData[i].needcheck = true;
	weaponData[i].melee = params[2] ? true:false;
	return i;
}

static cell AMX_NATIVE_CALL cwpn_dmg(AMX *amx, cell *params){ // wid,att,vic,dmg,hp=0
	int weapon = params[1];
	if (  weapon < DODMAX_WEAPONS-DODMAX_CUSTOMWPNS ){ // only for custom weapons
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	int att = params[2];
	if (att<1||att>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	int vic = params[3];
	if (vic<1||vic>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	
	int dmg = params[4];
	if ( dmg<1 ){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	
	int aim = params[5];
	if ( aim < 0 || aim > 7 ){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	CPlayer* pAtt = GET_PLAYER_POINTER_I(att);
	CPlayer* pVic = GET_PLAYER_POINTER_I(vic);

	pVic->pEdict->v.dmg_inflictor = NULL;

	if ( pAtt->index != pVic->index )
		pAtt->saveHit( pVic , weapon , dmg, aim );

	if ( !pAtt ) pAtt = pVic;
	int TA = 0;
	if ( (pVic->pEdict->v.team == pAtt->pEdict->v.team ) && ( pVic != pAtt) )
		TA = 1;

#ifdef FORWARD_OLD_SYSTEM
	g_damage_info.exec( pAtt->index, pVic->index, dmg, weapon, aim, TA );
#else
	MF_ExecuteForward( iFDamage,pAtt->index, pVic->index, dmg, weapon, aim, TA );
#endif
	
	if ( pVic->IsAlive() )
		return 1;

	pAtt->saveKill(pVic,weapon,( aim == 1 ) ? 1:0 ,TA);

#ifdef FORWARD_OLD_SYSTEM
	g_death_info.exec( pAtt->index, pVic->index, weapon, aim, TA );
#else
	MF_ExecuteForward( iFDeath,pAtt->index, pVic->index, weapon, aim, TA );
#endif
	


	return 1;
}

static cell AMX_NATIVE_CALL cwpn_shot(AMX *amx, cell *params){ // player,wid
	int index = params[2];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	int weapon = params[1];
	if (  weapon < DODMAX_WEAPONS-DODMAX_CUSTOMWPNS ){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	pPlayer->saveShot(weapon);

	return 1;
}

static cell AMX_NATIVE_CALL get_maxweapons(AMX *amx, cell *params){
	return DODMAX_WEAPONS;
}

static cell AMX_NATIVE_CALL get_stats_size(AMX *amx, cell *params){
	return 9;
}

static cell AMX_NATIVE_CALL is_custom(AMX *amx, cell *params){
	int weapon = params[1];
	if (  weapon < DODMAX_WEAPONS-DODMAX_CUSTOMWPNS ){
		return 0;
	}
	return 1;
}

static cell AMX_NATIVE_CALL dod_get_user_team(AMX *amx, cell *params){ // player,wid
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	return pPlayer->pEdict->v.team;

}

static cell AMX_NATIVE_CALL get_user_team(AMX *amx, cell *params){ // player,wid
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	int iTeam = pPlayer->pEdict->v.team; 
	if ( params[3] ){ 
		char *szTeam = ""; 
		switch(iTeam){
		case 1: 
			szTeam = "Allies"; 
			break; 
		case 2: 
			szTeam = "Axis"; 
			break; 
		} 
		MF_SetAmxString(amx,params[2],szTeam,params[3]); 
	} 
	return iTeam; 
} 

AMX_NATIVE_INFO base_Natives[] = {

	{ "dod_wpnlog_to_name", wpnlog_to_name },
	{ "dod_wpnlog_to_id", wpnlog_to_id },

	{ "dod_get_team_score", get_team_score },
	{ "dod_get_user_score", get_user_score },
	{ "dod_get_user_class", get_user_class },
	{ "dod_get_user_weapon", get_user_weapon },

	{ "dod_get_map_info", get_map_info },
	{ "dod_user_kill", user_kill },
	{ "dod_get_pronestate", get_user_pronestate },

	{ "xmod_get_wpnname", get_weapon_name },
	{ "xmod_get_wpnlogname", get_weapon_logname },
	{ "xmod_is_melee_wpn", is_melee },
	{ "xmod_get_maxweapons", get_maxweapons },
	{ "xmod_get_stats_size", get_stats_size },
	{ "xmod_is_custom_wpn", is_custom },
  
	{ "register_statsfwd",register_forward },

	// Custom Weapon Support
	{ "custom_weapon_add", register_cwpn }, // name,melee,logname
	{ "custom_weapon_dmg", cwpn_dmg },
	{ "custom_weapon_shot", cwpn_shot },

	//****************************************

	{ "get_user_team", get_user_team },
	{ "get_weaponname", get_weapon_name },
	{ "get_user_weapon", get_user_weapon },
	{ "dod_get_user_team", dod_get_user_team },
	{ "dod_get_wpnname", get_weapon_name },
	{ "dod_get_wpnlogname", get_weapon_logname },
	{ "dod_is_melee", is_melee },

	///*******************
	{ NULL, NULL } 
};