/*
 * TFCX 
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
#include "tfcx.h"

// Vexd start

// Set A TFC Player's model. This works differently then CS.
static cell AMX_NATIVE_CALL TFC_SetModel(AMX *amx, cell *params) { 
	int iIndex = params[1];
	// Make sure its a player.
	if (iIndex < 1 || iIndex > gpGlobals->maxClients) {
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	int iLen;

	char *szSkin = MF_GetAmxString(amx, params[3],0, &iLen);
	char *szModel = MF_GetAmxString(amx, params[2],0, &iLen);

	// Get Player's edict pointer
	edict_t* pPlayer = INDEXENT(iIndex);

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
  
	if (iIndex < 1 || iIndex > gpGlobals->maxClients) {
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

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
}
// Vexd end :)

// AssKicR start
static cell AMX_NATIVE_CALL TFC_SetBAmmo(AMX *amx, cell *params) { 
	int iIndex = params[1];
  
	if (iIndex < 1 || iIndex > gpGlobals->maxClients) {
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	int iValue = params[3];
		
	if (iValue < 0 )  {
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
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
  
	if (iIndex < 1 || iIndex > gpGlobals->maxClients) {
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

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
  
	if (iIndex < 1 || iIndex > gpGlobals->maxClients) {
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
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
  
	if (iIndex < 1 || iIndex > gpGlobals->maxClients) {
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	
	int iValue = params[3];
		
	if (iValue < 0 )  {
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
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

static cell AMX_NATIVE_CALL TFC_GetWpnName(AMX *amx, cell *params) { 
	int iIndex = params[1];
	if ( iIndex < 1 || iIndex > TFCMAX_WEAPONS ){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	return MF_SetAmxString(amx,params[2],weaponData[iIndex].name,params[3]);
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


static cell AMX_NATIVE_CALL TFC_register_cwpn(AMX *amx, cell *params){ // name,logname,melee=0 
	int i;
	bool bFree = false;
	for ( i=TFCMAX_WEAPONS-TFCMAX_CUSTOMWPNS;i<TFCMAX_WEAPONS;i++){
		if ( !weaponData[i].ammoSlot ){
			bFree = true;
			break;
		}
	}

	if ( !bFree )
		return 0;

	int iLen;
	char *szName = MF_GetAmxString(amx, params[1], 0, &iLen);

	strcpy(weaponData[i].name,szName);
	weaponData[i].ammoSlot = true;
	weaponData[i].melee = params[2] ? true:false;
	return i;
}

static cell AMX_NATIVE_CALL TFC_cwpn_dmg(AMX *amx, cell *params){ // wid,att,vic,dmg,hp=0
	int weapon = params[1];
	if (  weapon < TFCMAX_WEAPONS-TFCMAX_CUSTOMWPNS ){ // only for custom weapons
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
	pAtt->saveHit( pVic , weapon , dmg, aim );

	if ( !pAtt ) pAtt = pVic;
	int TA = 0;
	if ( (pVic->pEdict->v.team == pAtt->pEdict->v.team ) && ( pVic != pAtt) )
		TA = 1;
	MF_ExecuteForward ( iFDamage, pAtt->index, pVic->index, dmg, weapon, aim, TA );
	
	if ( pVic->IsAlive() )
		return 1;

	pAtt->saveKill(pVic,weapon,( aim == 1 ) ? 1:0 ,TA);
	MF_ExecuteForward ( iFDeath, pAtt->index, pVic->index, weapon, aim, TA );

	return 1;
}

static cell AMX_NATIVE_CALL TFC_cwpn_shot(AMX *amx, cell *params){ // player,wid
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	int weapon = params[2];
	if (  weapon < TFCMAX_WEAPONS-TFCMAX_CUSTOMWPNS ){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	pPlayer->saveShot(weapon);

	return 1;
}

static cell AMX_NATIVE_CALL TFC_IsMelee(AMX *amx, cell *params){ // player,wid
	int weapon = params[1];
	if (  weapon < 1 || weapon >= TFCMAX_WEAPONS  ){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	return weaponData[weapon].melee;
}

static cell AMX_NATIVE_CALL TFC_UserKill(AMX *amx, cell *params){ // player,wid
	int index = params[1];
	if (  index < 1 || index >gpGlobals->maxClients   ){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	pPlayer->killPlayer();

	return 1;
}

// Native list.
AMX_NATIVE_INFO base_Natives[] = {
	{"TFC_SetModel", TFC_SetModel},
	{"TFC_ClearModel", TFC_ClearModel},
	{"TFC_SetBAmmo", TFC_SetBAmmo},
	{"TFC_GetBAmmo", TFC_GetBAmmo},
	{"TFC_GetWeaponBAmmo", TFC_GetWeaponBAmmo},
	{"TFC_SetWeaponBAmmo", TFC_SetWeaponBAmmo},
	{"TFC_GetWpnName", TFC_GetWpnName},

	{"TFC_IsMelee", TFC_IsMelee},
	{"TFC_UserKill" , TFC_UserKill},
	
	// Custom Weapon Support
	{ "TFC_reg_custom_wpn", TFC_register_cwpn },
	{ "TFC_custom_wpn_dmg", TFC_cwpn_dmg },
	{ "TFC_custom_wpn_shot", TFC_cwpn_shot },

	{"TFC_SetPDdata", TFC_SetPDdata },
	//******************* 19 :)
	{NULL,			NULL} 
};
