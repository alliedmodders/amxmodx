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

#include "amxxmodule.h"
#include "tsx.h"

static cell AMX_NATIVE_CALL get_weapon_name(AMX *amx, cell *params){ // from id to name 3 params id, name, len
	int id = params[1];
	if (id<0 || id>=TSMAX_WEAPONS){ 
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	return MF_SetAmxString(amx,params[2],weaponData[id].name,params[3]);
}

static cell AMX_NATIVE_CALL wpnlog_to_name(AMX *amx, cell *params){ // from log to name
	int iLen;
	char *log = MF_GetAmxString(amx,params[1],0,&iLen);
	int i;
	for ( i=1; i<TSMAX_WEAPONS; i++ ){
		if ( strcmp(log,weaponData[i].logname ) == 0 )
			return MF_SetAmxString(amx,params[2],weaponData[i].name,params[3]);
	}
	return 0;
}

static cell AMX_NATIVE_CALL wpnlog_to_id(AMX *amx, cell *params){ // from log to id
	int iLen;
	char *log = MF_GetAmxString(amx,params[1],0,&iLen);

	int i;
	for ( i=1; i<TSMAX_WEAPONS; i++ ){
		if ( strcmp(log,weaponData[i].logname) == 0 )
			return i;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_weapon_logname(AMX *amx, cell *params){ // from id to log
	int id = params[1];
	if (id<0 || id>=TSMAX_WEAPONS){ 
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	return MF_SetAmxString(amx,params[2],weaponData[id].logname,params[3]);
}

static cell AMX_NATIVE_CALL is_melee(AMX *amx, cell *params){
	int id = params[1];
	if (id<1 || id>=TSMAX_WEAPONS){ 
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	if ( weaponData[id].melee )
		return 1;
	return 0;
}

static cell AMX_NATIVE_CALL get_user_weapon(AMX *amx, cell *params){
	int id = params[1];
	if ( id<1 || id>gpGlobals->maxClients ){ 
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if ( pPlayer->ingame ){
		int wpn = pPlayer->current;
		cell *cpTemp = MF_GetAmxAddr(amx,params[2]);
		*cpTemp = pPlayer->weapons[wpn].clip;
		cpTemp = MF_GetAmxAddr(amx,params[3]);
		*cpTemp = pPlayer->weapons[wpn].ammo;
		cpTemp = MF_GetAmxAddr(amx,params[4]);
		*cpTemp = pPlayer->weapons[wpn].mode;
		cpTemp = MF_GetAmxAddr(amx,params[5]);
		*cpTemp = pPlayer->weapons[wpn].attach;
		return wpn;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_user_cash(AMX *amx, cell *params){
	int id = params[1];
	if ( id<1 || id>gpGlobals->maxClients ){ 
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if ( pPlayer->ingame ){
		return pPlayer->money;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_user_space(AMX *amx, cell *params){
	int id = params[1];
	if ( id<1 || id>gpGlobals->maxClients ){ 
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if ( pPlayer->ingame ){
		return pPlayer->space;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_user_pwup(AMX *amx, cell *params){
	int id = params[1];
	if ( id<1 || id>gpGlobals->maxClients ){ 
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if ( pPlayer->ingame ){
		cell *cpTemp = MF_GetAmxAddr(amx,params[2]);
		*cpTemp = pPlayer->PwUpValue;
		return pPlayer->PwUp;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_user_items(AMX *amx, cell *params){
	int id = params[1];
	if ( id<1 || id>gpGlobals->maxClients ){ 
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if ( pPlayer->ingame ){
		return pPlayer->items;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_killingStreak(AMX *amx, cell *params){
	int id = params[1];
	if ( id<1 || id>gpGlobals->maxClients ){ 
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if ( pPlayer->ingame ){
		return pPlayer->killingSpree;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_lastFrag(AMX *amx, cell *params){
	int id = params[1];
	if ( id<1 || id>gpGlobals->maxClients ){ 
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if ( pPlayer->ingame ){
		return pPlayer->lastFrag;
	}
	return 0;
}

static cell AMX_NATIVE_CALL give_weapon(AMX *amx, cell *params){ // index,weapon,clips,extra
	int id = params[1];
	if ( id<1 || id>gpGlobals->maxClients ){ 
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if ( !pPlayer->ingame || !pPlayer->IsAlive() ){
		return 0;
	}

	// can he carry this weapon check ?

	string_t item = MAKE_STRING("ts_groundweapon"); 
	edict_t	*pent = CREATE_NAMED_ENTITY( item );
	if ( FNullEnt( pent ) ){
		return 0;
	}

	KeyValueData pkvd;
	char szTemp[16];

	sprintf(szTemp,"%d",(int)params[2]);

	pkvd.szClassName = (char *)STRING(pent->v.classname);
	pkvd.szKeyName = "tsweaponid"; // weapon
	pkvd.szValue = szTemp;
	pkvd.fHandled = false;
	MDLL_KeyValue(pent, &pkvd);

	pkvd.szClassName = (char *)STRING(pent->v.classname);
	pkvd.szKeyName = "wduration"; // duration
	pkvd.szValue = "180";
	pkvd.fHandled = false;
	MDLL_KeyValue(pent, &pkvd);

	sprintf(szTemp,"%d",(int)params[3]); 

	pkvd.szClassName = (char *)STRING(pent->v.classname);
	pkvd.szKeyName = "wextraclip"; // clips
	pkvd.szValue = szTemp;
	pkvd.fHandled = false;
	MDLL_KeyValue(pent, &pkvd);

	sprintf(szTemp,"%d",(int)params[4]);

	pkvd.szClassName = (char *)STRING(pent->v.classname);
	pkvd.szKeyName = "spawnflags"; // attachements :flashlight,lasersight,scope..
	pkvd.szValue = szTemp;
	pkvd.fHandled = false;
	MDLL_KeyValue(pent, &pkvd);

/*
	pkvd.szClassName = "ts_groundweapon";
	pkvd.szKeyName = "message";
	pkvd.szValue = "";
	pMDLL_KeyValue(pEntity, &pkvd);
*/

	MDLL_Spawn(pent);

	pent->v.origin = pPlayer->pEdict->v.origin;

	MDLL_Use(pent, pPlayer->pEdict);

	REMOVE_ENTITY(pent);

	return 1;

}

static cell AMX_NATIVE_CALL create_pwup(AMX *amx, cell *params){ // pwup ,origin[3]

	string_t item = MAKE_STRING("ts_powerup"); 
	edict_t	*pent = CREATE_NAMED_ENTITY( item );
	if ( FNullEnt( pent ) ){
		return 0;
	}

	KeyValueData pkvd;
	char szTemp[16];

	sprintf(szTemp,"%d",(int)params[1]);

	pkvd.szClassName = (char *)STRING(pent->v.classname);
	pkvd.szKeyName = "pwuptype"; // type
	pkvd.szValue = szTemp;
	pkvd.fHandled = false;
	MDLL_KeyValue(pent, &pkvd);

	pkvd.szClassName = (char *)STRING(pent->v.classname);
	pkvd.szKeyName = "pwupduration"; // duration
	pkvd.szValue = "60";
	pkvd.fHandled = false;
	MDLL_KeyValue(pent, &pkvd);

/*
	pkvd.szClassName = (char *)STRING(pent->v.classname);
	pkvd.szKeyName = "message";
	pkvd.szValue = "";
	pMDLL_KeyValue(pEntity, &pkvd);
*/
	cell *vInput = MF_GetAmxAddr(amx,params[2]);

	float fNewX = *(float *)((void *)&vInput[0]);
	float fNewY = *(float *)((void *)&vInput[1]);
	float fNewZ = *(float *)((void *)&vInput[2]);

	vec3_t vNewValue = vec3_t(fNewX, fNewY, fNewZ);

	MDLL_Spawn(pent);
	pent->v.origin = vNewValue;

	return ENTINDEX(pent);
}

// create_pwup -> !wait! -> give_pwup
static cell AMX_NATIVE_CALL give_pwup(AMX *amx, cell *params){ // index,pwupentindex

	edict_t* pent = INDEXENT(params[2]);
	if ( FNullEnt( pent ) || strcmp("ts_powerup",STRING(pent->v.classname))!=0 ){
		return 0;
	}
	
	int id = params[1];
	if ( id<1 || id>gpGlobals->maxClients ){ 
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if ( !pPlayer->ingame || !pPlayer->IsAlive() ){
		REMOVE_ENTITY(pent);
		return 0;
	}

	pent->v.origin = pPlayer->pEdict->v.origin;

	MDLL_Touch(pent, pPlayer->pEdict);

	REMOVE_ENTITY(pent);

	return 1;

}

static cell AMX_NATIVE_CALL ts_setup(AMX *amx, cell *params){ // index,pwupentindex
	gKnifeOffset = params[1];
	return 1;

}

AMX_NATIVE_INFO base_Natives[] = {
	{ "TS_GetWpnName", get_weapon_name },
	{ "TS_GetWpnLogName", get_weapon_logname },
	{ "TS_WpnLogToName", wpnlog_to_name },
	{ "TS_WpnLogToId", wpnlog_to_id },
	{ "TS_IsMelee", is_melee },
	{ "TS_GetUserWpn", get_user_weapon },
	{ "TS_GetUserCash", get_user_cash },
	{ "TS_GetUserSpace", get_user_space },
	{ "TS_GetUserPwUp",get_user_pwup },
	{ "TS_GetUserItems",get_user_items },
	{ "TS_GetKillingStreak",get_killingStreak },
	{ "TS_GetUserLastFrag",get_lastFrag },

	{ "TS_GiveWeapon",give_weapon },
	{ "TS_CreatePwUp",create_pwup },
	{ "TS_GivePwUp",give_pwup },

	{ "TS_SetPDdata",ts_setup },

	//"*******************"
	{ NULL, NULL } 
};