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
// TSX Module
//

#include "amxxmodule.h"
#include "tsx.h"

static cell AMX_NATIVE_CALL get_weapon_name(AMX *amx, cell *params)
{ // from id to name 3 params id, name, len
	int id = params[1];
	if (id<0 || id>=TSMAX_WEAPONS)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Weapon %d is not valid", id);
		return 0;
	}
	return MF_SetAmxString(amx,params[2],weaponData[id].name,params[3]);
}

static cell AMX_NATIVE_CALL wpnlog_to_name(AMX *amx, cell *params)
{ // from log to name
	int iLen;
	char *log = MF_GetAmxString(amx,params[1],0,&iLen);
	int i;
	for ( i=1; i<TSMAX_WEAPONS; i++ ){
		if ( strcmp(log,weaponData[i].logname ) == 0 )
			return MF_SetAmxString(amx,params[2],weaponData[i].name,params[3]);
	}
	return 0;
}

static cell AMX_NATIVE_CALL wpnlog_to_id(AMX *amx, cell *params)
{ // from log to id
	int iLen;
	char *log = MF_GetAmxString(amx,params[1],0,&iLen);

	int i;
	for (i=1; i<TSMAX_WEAPONS; i++ )
	{
		if ( strcmp(log,weaponData[i].logname) == 0 )
			return i;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_weapon_logname(AMX *amx, cell *params)
{ // from id to log
	int id = params[1];
	if (id<0 || id>=TSMAX_WEAPONS)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Weapon %d is not valid", id);
		return 0;
	}
	return MF_SetAmxString(amx,params[2],weaponData[id].logname,params[3]);
}

static cell AMX_NATIVE_CALL is_melee(AMX *amx, cell *params)
{
	int id = params[1];
	if (id<1 || id>=TSMAX_WEAPONS)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Weapon %d is not valid", id);
		return 0;
	}
	if ( weaponData[id].melee )
		return 1;
	return 0;
}

static cell AMX_NATIVE_CALL ts_get_user_weapon(AMX *amx, cell *params){
	int id = params[1];
	if (id<1 || id>gpGlobals->maxClients)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not valid", id);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if (pPlayer->ingame)
	{
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

static cell AMX_NATIVE_CALL get_user_weapon(AMX *amx, cell *params){
	int id = params[1];
	if (id<1 || id>gpGlobals->maxClients)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not valid", id);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if (pPlayer->ingame)
	{
		int wpn = pPlayer->current;
		cell *cpTemp = MF_GetAmxAddr(amx,params[2]);
		*cpTemp = pPlayer->weapons[wpn].clip;
		cpTemp = MF_GetAmxAddr(amx,params[3]);
		*cpTemp = pPlayer->weapons[wpn].ammo;
		return wpn;
	}
	return 0;
}

#define BEGIN_USER_FUNC(__name) \
	static cell AMX_NATIVE_CALL __name(AMX *amx, cell *params) { \
	int id = params[1]; \
	if (id<1 || id>gpGlobals->maxClients) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not valid", id); \
		return 0; } \
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id); \
	if (!pPlayer->ingame) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not ingame", id); \
	}
#define END_USER_FUNC() \
	}

BEGIN_USER_FUNC(get_user_slots)
	return pPlayer->GetSlots();
END_USER_FUNC()

BEGIN_USER_FUNC(set_user_slots)
	pPlayer->SetSlots(params[2]);
	return 1;
END_USER_FUNC()

BEGIN_USER_FUNC(get_user_state)
	return pPlayer->state;
END_USER_FUNC()

BEGIN_USER_FUNC(get_user_message)
	int val = pPlayer->GetOffset(TSX_MSG_OFFSET);
	return (val & 15);
END_USER_FUNC()

BEGIN_USER_FUNC(set_user_message)
	int message = params[2];
	if (message < 1 || message > 16)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid message id: %d", message);
		return 0;
	}
	int val = pPlayer->GetOffset(TSX_MSG_OFFSET);
	pPlayer->SetOffset(TSX_MSG_OFFSET, (val & ~15)^message);
	return 1;
END_USER_FUNC()

BEGIN_USER_FUNC(set_bullettrail)
	int bullettrail = params[2] * 256;
	pPlayer->SetOffset(TSX_BTRAIL_OFFSET, bullettrail);
	return 1;
END_USER_FUNC()

BEGIN_USER_FUNC(set_fake_slowmo)
	float time = amx_ctof(params[2]);
	pPlayer->SetOffset(TSX_SLOMO1_OFFSET, TSPWUP_SLOWMO);
	float prev = pPlayer->GetTime();
	pPlayer->SetOffsetF(TSX_SLOMO2_OFFSET, prev+time);
	return 1;
END_USER_FUNC()

BEGIN_USER_FUNC(set_fake_slowpause)
	float time = amx_ctof(params[2]);
	pPlayer->SetOffset(TSX_SLOMO1_OFFSET, TSPWUP_SLOWPAUSE);
	float prev = pPlayer->GetTime();
	pPlayer->SetOffsetF(TSX_SLOMO2_OFFSET, prev+time);
	return 1;
END_USER_FUNC()

BEGIN_USER_FUNC(is_in_slowmo)
	if (pPlayer->GetOffsetF(TSX_ISSLO_OFFSET))
		return amx_ftoc(pPlayer->GetOffsetF(TSX_SLOMO2_OFFSET));
	return 0;
END_USER_FUNC()

BEGIN_USER_FUNC(set_speed)
	pPlayer->SetOffsetF(TSX_ISSLO_OFFSET, amx_ctof(params[2]));
	pPlayer->SetOffsetF(TSX_SPEED2_OFFSET, amx_ctof(params[3]));
	pPlayer->SetOffsetF(TSX_SPEED1_OFFSET, amx_ctof(params[3]));
	pPlayer->SetOffsetF(TSX_SLOMO2_OFFSET, amx_ctof(params[4]));
	return 1;
END_USER_FUNC()

BEGIN_USER_FUNC(set_physics_speed)
	pPlayer->SetOffsetF(TSX_PHYSICS_OFFSET, amx_ctof(params[2]));
	return 1;
END_USER_FUNC()

BEGIN_USER_FUNC(is_running_powerup)
	return pPlayer->GetOffset(TSX_SLOMO1_OFFSET);
END_USER_FUNC()

BEGIN_USER_FUNC(force_powerup_run)
	pPlayer->SetOffset(TSX_SLOMO1_OFFSET, params[2]);
	return 1;
END_USER_FUNC()

BEGIN_USER_FUNC(has_superjump)
	int val3 = pPlayer->GetOffset(TSX_MSG_OFFSET);

	if (val3 & 0x01000000)
		return 1;

	return 0;
END_USER_FUNC()

BEGIN_USER_FUNC(has_fupowerup)
	int val3 = pPlayer->GetOffset(TSX_MSG_OFFSET);

	if (val3 & 65536)
		return 1;

	return 0;
END_USER_FUNC()

static cell AMX_NATIVE_CALL set_user_cash(AMX *amx, cell *params)
{
	int id = params[1];
	if (id<1 || id>gpGlobals->maxClients)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not valid", id);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if (pPlayer->ingame)
	{
		pPlayer->SetMoney(params[2]);
		pPlayer->money = params[2];
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_user_cash(AMX *amx, cell *params)
{
	int id = params[1];
	if (id<1 || id>gpGlobals->maxClients)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not valid", id);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if (pPlayer->ingame)
	{
		return pPlayer->money;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_user_space(AMX *amx, cell *params){
	int id = params[1];
	if (id<1 || id>gpGlobals->maxClients)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not valid", id);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if (pPlayer->ingame)
	{
		return pPlayer->space;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_user_pwup(AMX *amx, cell *params){
	int id = params[1];
	if (id<1 || id>gpGlobals->maxClients)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not valid", id);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if (pPlayer->ingame)
	{
		cell *cpTemp = MF_GetAmxAddr(amx,params[2]);
		*cpTemp = pPlayer->PwUpValue;
		return pPlayer->PwUp;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_user_items(AMX *amx, cell *params)
{
	int id = params[1];
	if (id<1 || id>gpGlobals->maxClients)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not valid", id);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if (pPlayer->ingame)
	{
		return pPlayer->items;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_killingStreak(AMX *amx, cell *params)
{
	int id = params[1];
	if (id<1 || id>gpGlobals->maxClients)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not valid", id);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if (pPlayer->ingame)
	{
		return pPlayer->killingSpree;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_lastFrag(AMX *amx, cell *params)
{
	int id = params[1];
	if (id<1 || id>gpGlobals->maxClients)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not valid", id);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if (pPlayer->ingame)
	{
		return pPlayer->lastFrag;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_killflags(AMX *amx, cell *params)
{
	int id = params[1];
	if (id<1 || id>gpGlobals->maxClients)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not valid", id);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if ( pPlayer->ingame ){
		return pPlayer->killFlags;
	}
	return 0;
}

static cell AMX_NATIVE_CALL give_weapon(AMX *amx, cell *params)
{ // index,weapon,clips,extra
	int id = params[1];
	if (id<1 || id>gpGlobals->maxClients)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not valid", id);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if (!pPlayer->ingame || !pPlayer->IsAlive())
	{
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
static cell AMX_NATIVE_CALL give_pwup(AMX *amx, cell *params)
{ // index,pwupentindex
	edict_t* pent = INDEXENT(params[2]);
	if ( FNullEnt( pent ) || strcmp("ts_powerup",STRING(pent->v.classname))!=0 ){
		return 0;
	}
	
	int id = params[1];
	if (id<1 || id>gpGlobals->maxClients)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid player %d", id);
		return 0;
	}
	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
	if (!pPlayer->ingame || !pPlayer->IsAlive())
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not ingame.", id);
		return 0;
	}

	pent->v.origin = pPlayer->pEdict->v.origin;

	MDLL_Touch(pent, pPlayer->pEdict);

	REMOVE_ENTITY(pent);

	return 1;

}

static cell AMX_NATIVE_CALL ts_setup(AMX *amx, cell *params)
{ // index,pwupentindex
	gKnifeOffset = params[1];
	return 1;
}

static cell AMX_NATIVE_CALL register_forward(AMX *amx, cell *params)
{ // forward 
	return 1;
}

static cell AMX_NATIVE_CALL get_maxweapons(AMX *amx, cell *params)
{
	return TSMAX_WEAPONS;
}

static cell AMX_NATIVE_CALL get_stats_size(AMX *amx, cell *params)
{
	return 8;
}

static cell AMX_NATIVE_CALL is_custom(AMX *amx, cell *params)
{
	int weapon = params[1];
	if (weapon < TSMAX_WEAPONS-TSMAX_CUSTOMWPNS)
	{
		return 0;
	}
	return 1;
}

AMX_NATIVE_INFO base_Natives[] = {

	{ "xmod_get_wpnname", get_weapon_name },
	{ "xmod_get_wpnlogname", get_weapon_logname },
	{ "xmod_is_melee_wpn", is_melee },
	{ "xmod_get_maxweapons", get_maxweapons },
	{ "xmod_get_stats_size", get_stats_size },
	{ "xmod_is_custom_wpn", is_custom },
	
	{ "ts_wpnlogtoname", wpnlog_to_name },
	{ "ts_wpnlogtoid", wpnlog_to_id },
	
	{ "ts_getuserwpn", ts_get_user_weapon },
	{ "ts_getusercash", get_user_cash },
	{ "ts_setusercash", set_user_cash },
	{ "ts_getuserspace", get_user_space },
	{ "ts_getuserpwup", get_user_pwup },
	{ "ts_getuseritems", get_user_items },
	{ "ts_getkillingstreak", get_killingStreak },
	{ "ts_getuserlastfrag", get_lastFrag },
	{ "ts_getuserkillflags", get_killflags },
	{ "ts_getuserslots", get_user_slots },
	{ "ts_setuserslots", set_user_slots },

	{ "ts_getuserstate", get_user_state },

	{ "ts_giveweapon",give_weapon },
	{ "ts_createpwup",create_pwup },
	{ "ts_givepwup",give_pwup },

	{ "ts_has_superjump", has_superjump },
	{ "ts_has_fupowerup", has_fupowerup },

	{ "ts_set_message", set_user_message },
	{ "ts_get_message", get_user_message },

	{ "ts_setpddata",ts_setup },

	{ "register_statsfwd", register_forward },

	{ "ts_set_bullettrail",set_bullettrail },
	{ "ts_set_fakeslowmo",set_fake_slowmo },
	{ "ts_set_fakeslowpause",set_fake_slowpause },
	{ "ts_is_in_slowmo",is_in_slowmo },
	{ "ts_set_speed",set_speed },
	{ "ts_set_physics_speed",set_physics_speed },
	{ "ts_is_running_powerup",is_running_powerup},
	{ "ts_force_run_powerup",force_powerup_run},

	//****************************************
	{ "get_weaponname", get_weapon_name },
	{ "get_user_weapon", get_user_weapon },

	//"*******************"
	{ NULL, NULL } 
};
