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
// TFCX Module
//

#include "amxxmodule.h"
#include "tfcx.h"

extern int g_AlliesFlags[4];

// Vexd start

// Set A TFC Player's model. This works differently then CS.
static cell AMX_NATIVE_CALL TFC_SetModel(AMX *amx, cell *params) { 
	int iIndex = params[1];
	// Make sure its a player.
	CHECK_PLAYER(iIndex);

	int iLen;

	char *szSkin = MF_GetAmxString(amx, params[3],0, &iLen);
	char *szModel = MF_GetAmxString(amx, params[2],0, &iLen);

	// Get Player's edict pointer
	edict_t* pPlayer = MF_GetPlayerEdict(iIndex);

	// Set key on client, replacement_model is for the model we want.
	KeyValueData pkvd;
	pkvd.szClassName = "player";
	pkvd.szKeyName = "replacement_model";
	pkvd.szValue = szModel;
	pkvd.fHandled = FALSE;
	(gpGamedllFuncs->dllapi_table->pfnKeyValue)(pPlayer, &pkvd);

	// Set key on client, replacement_model_skin is for what skin we want.
	KeyValueData pkvd2;
	pkvd2.szClassName = "player";
	pkvd2.szKeyName = "replacement_model_skin";
	pkvd2.szValue = szSkin;
	pkvd2.fHandled = FALSE;
	(gpGamedllFuncs->dllapi_table->pfnKeyValue)(pPlayer, &pkvd2);

	// and, like CS, set model in the infobuffer.
	(g_engfuncs.pfnSetClientKeyValue)(iIndex, (g_engfuncs.pfnGetInfoKeyBuffer)(pPlayer), "model", szModel);

	return 1;
}

// Clear's a TFC player's model... Almost the same as setting the model.
// HACKHACK: Is there a better way of doing this? what if the player is random?
static cell AMX_NATIVE_CALL TFC_ClearModel(AMX *amx, cell *params) { 
	int iIndex = params[1];
  
	CHECK_PLAYER(iIndex)

	edict_t* pPlayer = MF_GetPlayerEdict(iIndex);
	
	if (pPlayer->pvPrivateData == NULL)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Player has no private data, cannot clear model!");
		return 0;
	}

	// PD_REPLACE_MODEL is the string_t for the replacement model
	// setting it to 0 will reset the model properly
	*( (int*)pPlayer->pvPrivateData + PD_REPLACE_MODEL) = 0;

	// PD_REPLACE_SKIN is the integer setting for the skin, by default
	// it is 0.
	*( (int*)pPlayer->pvPrivateData + PD_REPLACE_SKIN) = 0;

	return 1;
	
	/*
	// the old, buggy method here
	int iIndex = params[1];
  
	CHECK_PLAYER(iIndex)

	edict_t* pPlayer = INDEXENT(iIndex);
	
	char szModel[32];
	memset(szModel, 0x0, strlen(szModel));

	switch(pPlayer->v.playerclass) {
		case 1:	//Scout
			memcpy(szModel, "Scout", strlen("Scout"));
			break;
		case 2:	//Sniper
			memcpy(szModel, "Sniper", strlen("Sniper"));
			break;
		case 3:	//Soldier
			memcpy(szModel, "Soldier", strlen("Soldier"));
			break;
		case 4:	//demoman
			memcpy(szModel, "Demo", strlen("Demo"));
			//(char *)szModel = "Demo";
			break;
		case 5:	//Medic
			memcpy(szModel, "Medic", strlen("Medic"));
			break;
		case 6:	//HWGuy
			memcpy(szModel, "HvyWeapon", strlen("HvyWeapon"));
			break;
		case 7:	//Pyro
			memcpy(szModel, "Pyro", strlen("Pyro"));
			break;
		case 8:	//Spy
			memcpy(szModel, "Spy", strlen("Spy"));
			break;
		case 9:	//Engineer
			memcpy(szModel, "Engineer", strlen("Engineer"));
			break;
		case 11: //Hunted
			memcpy(szModel, "Civilian", strlen("Civilian"));
			break;
		default:
			break;
	}

	KeyValueData pkvd;
	pkvd.szClassName = "player";
	pkvd.szKeyName = "replacement_model";
	pkvd.szValue = szModel;
	pkvd.fHandled = FALSE;
	(gpGamedllFuncs->dllapi_table->pfnKeyValue)(pPlayer, &pkvd);

	KeyValueData pkvd2;
	pkvd2.szClassName = "player";
	pkvd2.szKeyName = "replacement_model_skin";
	pkvd2.szValue = "0";
	pkvd2.fHandled = FALSE;
	(gpGamedllFuncs->dllapi_table->pfnKeyValue)(pPlayer, &pkvd2);

	(g_engfuncs.pfnSetClientKeyValue)(iIndex, (g_engfuncs.pfnGetInfoKeyBuffer)(pPlayer), "model", szModel);
	
	return 1;
	*/
}
// Vexd end :)

// AssKicR start
static cell AMX_NATIVE_CALL TFC_SetBAmmo(AMX *amx, cell *params) { 
	int iIndex = params[1];
  
	CHECK_PLAYER(iIndex);
	int iValue = params[3];
		
	if (iValue < 0 )  {
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid ammo amount %d", iValue);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(iIndex);

	switch(params[2]){ // ammo
	case TFC_AMMO_SHELLS:
		*( (int*)pPlayer->pEdict->pvPrivateData + pdAmmo[TFC_AMMO_SHELLS]) = iValue;
		break;
	case TFC_AMMO_BULLETS:
		*( (int*)pPlayer->pEdict->pvPrivateData + pdAmmo[TFC_AMMO_BULLETS]) = iValue;
		break;
	case TFC_AMMO_CELLS:
		*( (int*)pPlayer->pEdict->pvPrivateData + pdAmmo[TFC_AMMO_CELLS]) = iValue;
		break;
	case TFC_AMMO_ROCKETS:
		*( (int*)pPlayer->pEdict->pvPrivateData + pdAmmo[TFC_AMMO_ROCKETS]) = iValue;
		break;
	case TFC_AMMO_NADE1:
		*( (int*)pPlayer->pEdict->pvPrivateData + pdAmmo[TFC_AMMO_NADE1]) = iValue;
		break;
	case TFC_AMMO_NADE2:
		*( (int*)pPlayer->pEdict->pvPrivateData + pdAmmo[TFC_AMMO_NADE2]) = iValue;
		break;
	default : 
		return 0;
		break;
	}
	return 1;
}

static cell AMX_NATIVE_CALL TFC_GetBAmmo(AMX *amx, cell *params) { 
	int iIndex = params[1];
  
	CHECK_PLAYER(iIndex);

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(iIndex);
	
	switch(params[2]){ // ammo
	case TFC_AMMO_SHELLS:
		return *( (int*)pPlayer->pEdict->pvPrivateData + pdAmmo[TFC_AMMO_SHELLS]);
		break;
	case TFC_AMMO_BULLETS:
		return *( (int*)pPlayer->pEdict->pvPrivateData + pdAmmo[TFC_AMMO_BULLETS]);
		break;
	case TFC_AMMO_CELLS:
		return *( (int*)pPlayer->pEdict->pvPrivateData + pdAmmo[TFC_AMMO_CELLS]);
		break;
	case TFC_AMMO_ROCKETS:
		return *( (int*)pPlayer->pEdict->pvPrivateData + pdAmmo[TFC_AMMO_ROCKETS]);
		break;
	case TFC_AMMO_NADE1:
		return *( (int*)pPlayer->pEdict->pvPrivateData + pdAmmo[TFC_AMMO_NADE1]);
		break;
	case TFC_AMMO_NADE2:
		return *( (int*)pPlayer->pEdict->pvPrivateData + pdAmmo[TFC_AMMO_NADE2]);
		break;
	default : 
		return 0;
		break;
	}
	return 1;
}

static cell AMX_NATIVE_CALL TFC_GetWeaponBAmmo(AMX *amx, cell *params) { 
	int iIndex = params[1];
  
	CHECK_PLAYER(iIndex);

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(iIndex);

	switch(params[2]){ // weapon
	case TFC_WPN_SNIPERRIFLE:
	case TFC_WPN_AUTORIFLE:
	case TFC_WPN_SHOTGUN:
	case TFC_WPN_SUPERSHOTGUN:
	case TFC_WPN_TRANQ:
	case TFC_WPN_AC:
		return *( (int*)pPlayer->pEdict->pvPrivateData + PD_AMMO_SHELLS);
		break;
	case TFC_WPN_NG:
	case TFC_WPN_SUPERNG:
	case TFC_WPN_RAILGUN:
		return *( (int*)pPlayer->pEdict->pvPrivateData + PD_AMMO_BULLETS);
		break;
	case TFC_WPN_FLAMETHROWER:
		return *( (int*)pPlayer->pEdict->pvPrivateData + PD_AMMO_CELLS);
		break;
	case TFC_WPN_GL:
	case TFC_WPN_RPG:
	case TFC_WPN_IC:
	case TFC_WPN_PL:
		return *( (int*)pPlayer->pEdict->pvPrivateData + PD_AMMO_ROCKETS);
		break;
	case TFC_WPN_CALTROP:
	case TFC_WPN_NORMALGRENADE:
		return *( (int*)pPlayer->pEdict->pvPrivateData + PD_AMMO_NADE1);
		break;
	case TFC_WPN_CONCUSSIONGRENADE: 
	case TFC_WPN_NAILGRENADE: 
	case TFC_WPN_MIRVGRENADE:
	case TFC_WPN_NAPALMGRENADE:
	case TFC_WPN_GASGRENADE:
	case TFC_WPN_EMPGRENADE:
		return *( (int*)pPlayer->pEdict->pvPrivateData + PD_AMMO_NADE2);


		break;
	default : 
		return 0;
		break;
	}
	return 1;

}

static cell AMX_NATIVE_CALL TFC_SetWeaponBAmmo(AMX *amx, cell *params) { 
	int iIndex = params[1];
  
	CHECK_PLAYER(iIndex);
	
	int iValue = params[3];
		
	if (iValue < 0 )  {
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid ammo amount %d", iValue);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(iIndex);

	switch(params[2]){ // weapon
	case TFC_WPN_SNIPERRIFLE:
	case TFC_WPN_AUTORIFLE:
	case TFC_WPN_SHOTGUN:
	case TFC_WPN_SUPERSHOTGUN:
	case TFC_WPN_TRANQ:
	case TFC_WPN_AC:
		*( (int*)pPlayer->pEdict->pvPrivateData + PD_AMMO_SHELLS) = iValue;
		break;
	case TFC_WPN_NG:
	case TFC_WPN_SUPERNG:
	case TFC_WPN_RAILGUN:
		*( (int*)pPlayer->pEdict->pvPrivateData + PD_AMMO_BULLETS) = iValue;
		break;
	case TFC_WPN_FLAMETHROWER:
		*( (int*)pPlayer->pEdict->pvPrivateData + PD_AMMO_CELLS) = iValue;
		break;
	case TFC_WPN_GL:
	case TFC_WPN_RPG:
	case TFC_WPN_IC:
	case TFC_WPN_PL:
		*( (int*)pPlayer->pEdict->pvPrivateData + PD_AMMO_ROCKETS) = iValue;
		break;
	case TFC_WPN_CALTROP:
	case TFC_WPN_NORMALGRENADE:
		*( (int*)pPlayer->pEdict->pvPrivateData + PD_AMMO_NADE1) = iValue;
		break;
	case TFC_WPN_CONCUSSIONGRENADE: 
	case TFC_WPN_NAILGRENADE: 
	case TFC_WPN_MIRVGRENADE:
	case TFC_WPN_NAPALMGRENADE:
	case TFC_WPN_GASGRENADE:
	case TFC_WPN_EMPGRENADE:
		*( (int*)pPlayer->pEdict->pvPrivateData + PD_AMMO_NADE2) = iValue;
		break;
	default : 
		return 0;
		break;
	}
	return 1;
}
// AssKicR end :)

static cell AMX_NATIVE_CALL TFC_GetWeaponAmmo(AMX *amx, cell *params)
{
	int index = params[1];

	CHECK_NONPLAYER(index);
	edict_t *weapon = INDEXENT(index);

	return *((int *)weapon->pvPrivateData + PD_WEAPON_AMMO);
}

static cell AMX_NATIVE_CALL TFC_SetWeaponAmmo(AMX *amx, cell *params)
{
	int index = params[1];

	CHECK_NONPLAYER(index);
	edict_t *weapon = INDEXENT(index);

	*((int *)weapon->pvPrivateData + PD_WEAPON_AMMO) = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL TFC_GetUserGoalItem(AMX *amx, cell *params)
{
	int index = params[1];
	
	CHECK_PLAYER(index);

	edict_t *pPlayer = MF_GetPlayerEdict(index);
	cell *team = MF_GetAmxAddr(amx, params[2]);

	*team = *((int *)pPlayer->pvPrivateData + PD_GOALITEM_TEAM);

	return *((int *)pPlayer->pvPrivateData + PD_HAS_GOALITEM) & CARRYING_GOALITEM;
}

static cell AMX_NATIVE_CALL TFC_GetWpnName(AMX *amx, cell *params) { 
	int iIndex = params[1];
	if ( iIndex < 1 || iIndex > TFCMAX_WEAPONS ){
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon id %d", iIndex);
		return 0;
	}
	return MF_SetAmxString(amx,params[2],weaponData[iIndex].name,params[3]);
}

static cell AMX_NATIVE_CALL TFC_GetWpnLogName(AMX *amx, cell *params) { 
	int iIndex = params[1];
	if ( iIndex < 1 || iIndex > TFCMAX_WEAPONS ){
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon id %d", iIndex);
		return 0;
	}
	return MF_SetAmxString(amx,params[2],weaponData[iIndex].logName,params[3]);
}

static cell AMX_NATIVE_CALL TFC_SetPDdata(AMX *amx, cell *params) { 

	pdTimerOwner  = params[1];
	pdSentryGunOwner  = params[2];

	pdAmmo[TFC_AMMO_SHELLS] = params[3];
	pdAmmo[TFC_AMMO_BULLETS] = params[4];
	pdAmmo[TFC_AMMO_CELLS] = params[5];
	pdAmmo[TFC_AMMO_ROCKETS] = params[6]; 
	pdAmmo[TFC_AMMO_NADE1] = params[7];
	pdAmmo[TFC_AMMO_NADE2] = params[8];

	return 1;

}

static cell AMX_NATIVE_CALL TFC_IsMelee(AMX *amx, cell *params){ // player,wid
	int weapon = params[1];
	if (  weapon < 1 || weapon >= TFCMAX_WEAPONS  ){
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon id %d", weapon);
		return 0;
	}
	return weaponData[weapon].melee;
}

static cell AMX_NATIVE_CALL TFC_UserKill(AMX *amx, cell *params){ // player,wid
	int index = params[1];
	CHECK_PLAYER(index);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	pPlayer->killPlayer();

	return 1;
}

static cell AMX_NATIVE_CALL get_maxweapons(AMX *amx, cell *params){
	return TFCMAX_WEAPONS;
}

static cell AMX_NATIVE_CALL get_stats_size(AMX *amx, cell *params){
	return 8;
}

static cell AMX_NATIVE_CALL is_custom(AMX *amx, cell *params){
	int weapon = params[1];
	if (  weapon < TFCMAX_WEAPONS-TFCMAX_CUSTOMWPNS ){
		return 0;
	}
	return 1;
}

static cell AMX_NATIVE_CALL register_forward(AMX *amx, cell *params)
{
	return 1;
}

static cell AMX_NATIVE_CALL TFC_IsFeigning(AMX *amx, cell *params)
{
	int index = params[1];
	CHECK_PLAYER(index);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	
	return (pPlayer->pEdict->v.playerclass == TFC_PC_SPY && pPlayer->pEdict->v.deadflag == 5);
};
cvar_t *mp_teamplay=NULL;
static cell AMX_NATIVE_CALL TFC_IsTeamAlly(AMX *amx, cell *params)
{
	if (mp_teamplay==NULL)
	{
		mp_teamplay=CVAR_GET_POINTER("mp_teamplay");
	}

	if (mp_teamplay && mp_teamplay->value != 0.0)
	{
		return 0;
	}
	int TeamA=params[1];
	int TeamB=params[2];
	if (TeamA==TeamB) // same team, yes these are allies
	{
		return 1;
	}

	if (TeamA==0 || TeamB==0) // spectators
	{
		return 0;
	}



	if (TeamA < 1 || TeamA > 4) // out of bounds?
	{
		MF_LogError(amx,AMX_ERR_NATIVE,"Team A is out of bounds (got %d, expected 0 through 4)",TeamA);
		return 0;
	}
	if (TeamB < 1 || TeamB > 4) // out of bounds?
	{
		MF_LogError(amx,AMX_ERR_NATIVE,"Team B is out of bounds (got %d, expected 0 through 4)",TeamA);
		return 0;
	}

	if (g_AlliesFlags[--TeamA] & (1<<(--TeamB)))
	{
		return 1;
	}


	return 0;
};

// Native list.
AMX_NATIVE_INFO base_Natives[] = {
	{"tfc_setmodel", TFC_SetModel},
	{"tfc_clearmodel", TFC_ClearModel},
	{"tfc_setbammo", TFC_SetBAmmo},
	{"tfc_getbammo", TFC_GetBAmmo},
	{"tfc_getweaponbammo", TFC_GetWeaponBAmmo},
	{"tfc_setweaponbammo", TFC_SetWeaponBAmmo},
	{"tfc_getweaponammo", TFC_GetWeaponAmmo},
	{"tfc_setweaponammo", TFC_SetWeaponAmmo},

	{"tfc_is_user_feigning", TFC_IsFeigning},

	{"tfc_is_team_ally", TFC_IsTeamAlly},

	{"tfc_get_user_goalitem", TFC_GetUserGoalItem},

	{"xmod_get_wpnname", TFC_GetWpnName},
	{"xmod_get_wpnlogname", TFC_GetWpnLogName},
	{"xmod_is_melee_wpn", TFC_IsMelee},
	{"xmod_get_maxweapons", get_maxweapons},
	{"xmod_get_stats_size", get_stats_size},
	{"xmod_is_custom_wpn", is_custom},

	{"tfc_userkill" , TFC_UserKill},
	
	{"tfc_setpddata", TFC_SetPDdata },

	{"register_statsfwd",register_forward },

	//*****************************************

	{"get_weaponname", TFC_GetWpnName},

	//******************* 19 :)
	{NULL,			NULL} 
};
