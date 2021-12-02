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

static cell AMX_NATIVE_CALL get_user_astats(AMX *amx, cell *params) /* 6 param */
{
	int index = params[1];
	CHECK_PLAYERRANGE(index);
	int attacker = params[2];
	CHECK_PLAYERRANGE(attacker);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if (pPlayer->attackers[attacker].hits){
		cell *cpStats = MF_GetAmxAddr(amx,params[3]);
		cell *cpBodyHits = MF_GetAmxAddr(amx,params[4]);
		CPlayer::PlayerWeapon* stats = &pPlayer->attackers[attacker];
		cpStats[0] = stats->kills;
		cpStats[1] = stats->deaths;
		cpStats[2] = stats->hs;
		cpStats[3] = stats->tks;
		cpStats[4] = stats->shots;
		cpStats[5] = stats->hits;
		cpStats[6] = stats->damage;
		for(int i = 1; i < 8; ++i)
			cpBodyHits[i] = stats->bodyHits[i];
		if (params[6] && attacker && stats->name )
			MF_SetAmxString(amx,params[5],stats->name,params[6]);
		return 1;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_user_vstats(AMX *amx, cell *params) /* 6 param */
{
	int index = params[1];
	CHECK_PLAYERRANGE(index);
	int victim = params[2];
	CHECK_PLAYERRANGE(victim);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if (pPlayer->victims[victim].hits){
		cell *cpStats = MF_GetAmxAddr(amx,params[3]);
		cell *cpBodyHits = MF_GetAmxAddr(amx,params[4]);
		CPlayer::PlayerWeapon* stats = &pPlayer->victims[victim];
		cpStats[0] = stats->kills;
		cpStats[1] = stats->deaths;
		cpStats[2] = stats->hs;
		cpStats[3] = stats->tks;
		cpStats[4] = stats->shots;
		cpStats[5] = stats->hits;
		cpStats[6] = stats->damage;
		for(int i = 1; i < 8; ++i)
			cpBodyHits[i] = stats->bodyHits[i];
		if (params[6] && victim && stats->name)
			MF_SetAmxString(amx,params[5],stats->name,params[6]);
		return 1;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_user_wrstats(AMX *amx, cell *params) /* 4 param */ // DEC-Weapon (round) stats (end)
{
	int index = params[1];
	CHECK_PLAYERRANGE(index);
	int weapon = params[2];
	if (weapon<0||weapon>=TFCMAX_WEAPONS){
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon id %d", weapon);
		return 0;
	}
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if (pPlayer->weaponsRnd[weapon].shots){
		cell *cpStats = MF_GetAmxAddr(amx,params[3]);
		cell *cpBodyHits = MF_GetAmxAddr(amx,params[4]);
		Stats* stats = &pPlayer->weaponsRnd[weapon];
		cpStats[0] = stats->kills;
		cpStats[1] = stats->deaths;
		cpStats[2] = stats->hs;
		cpStats[3] = stats->tks;
		cpStats[4] = stats->shots;
		cpStats[5] = stats->hits;
		cpStats[6] = stats->damage;
		for(int i = 1; i < 8; ++i)
			cpBodyHits[i] = stats->bodyHits[i];
		return 1;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_user_wstats(AMX *amx, cell *params) /* 4 param */
{
	int index = params[1];
	CHECK_PLAYERRANGE(index);
	int weapon = params[2];
	if (weapon<0||weapon>=TFCMAX_WEAPONS){
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon id %d", weapon);
		return 0;
	}
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if (pPlayer->weapons[weapon].shots){
		cell *cpStats = MF_GetAmxAddr(amx,params[3]);
		cell *cpBodyHits = MF_GetAmxAddr(amx,params[4]);
		CPlayer::PlayerWeapon* stats = &pPlayer->weapons[weapon];
		cpStats[0] = stats->kills;
		cpStats[1] = stats->deaths;
		cpStats[2] = stats->hs;
		cpStats[3] = stats->tks;
		cpStats[4] = stats->shots;
		cpStats[5] = stats->hits;
		cpStats[6] = stats->damage;
		for(int i = 1; i < 8; ++i)
			cpBodyHits[i] = stats->bodyHits[i];
		return 1;
	}
	return 0;
}

static cell AMX_NATIVE_CALL reset_user_wstats(AMX *amx, cell *params) /* 6 param */
{
	int index = params[1];
	CHECK_PLAYERRANGE(index);
	GET_PLAYER_POINTER_I(index)->restartStats();
	return 1;
}

static cell AMX_NATIVE_CALL get_user_stats(AMX *amx, cell *params) /* 3 param */
{
	int index = params[1];
	CHECK_PLAYERRANGE(index);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if ( pPlayer->rank ){
		cell *cpStats = MF_GetAmxAddr(amx,params[2]);
		cell *cpBodyHits = MF_GetAmxAddr(amx,params[3]);
		cpStats[0] = pPlayer->rank->kills;
		cpStats[1] = pPlayer->rank->deaths;
		cpStats[2] = pPlayer->rank->hs;
		cpStats[3] = pPlayer->rank->tks;
		cpStats[4] = pPlayer->rank->shots;
		cpStats[5] = pPlayer->rank->hits;
		cpStats[6] = pPlayer->rank->damage;
		cpStats[7] = pPlayer->rank->getPosition();
		for(int i = 1; i < 8; ++i)
			cpBodyHits[i] = pPlayer->rank->bodyHits[i];
		return pPlayer->rank->getPosition();
	}
	return 0;
	
}

static cell AMX_NATIVE_CALL get_user_rstats(AMX *amx, cell *params) /* 3 param */
{
	int index = params[1];
	CHECK_PLAYERRANGE(index);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if (pPlayer->rank){
		cell *cpStats = MF_GetAmxAddr(amx,params[2]);
		cell *cpBodyHits = MF_GetAmxAddr(amx,params[3]);
		cpStats[0] = pPlayer->life.kills;
		cpStats[1] = pPlayer->life.deaths;
		cpStats[2] = pPlayer->life.hs;
		cpStats[3] = pPlayer->life.tks;
		cpStats[4] = pPlayer->life.shots;
		cpStats[5] = pPlayer->life.hits;
		cpStats[6] = pPlayer->life.damage;
		for(int i = 1; i < 8; ++i)
			cpBodyHits[i] = pPlayer->life.bodyHits[i];
		return 1;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_stats(AMX *amx, cell *params) /* 3 param */
{
	
	int index = params[1] + 1;

	for(RankSystem::iterator a = g_rank.front(); a ;--a){
		if ((*a).getPosition() == index)  {
			cell *cpStats = MF_GetAmxAddr(amx,params[2]);
			cell *cpBodyHits = MF_GetAmxAddr(amx,params[3]);
			cpStats[0] = (*a).kills;
			cpStats[1] = (*a).deaths;
			cpStats[2] = (*a).hs;
			cpStats[3] = (*a).tks;
			cpStats[4] = (*a).shots;
			cpStats[5] = (*a).hits;
			cpStats[6] = (*a).damage;
			cpStats[7] = (*a).getPosition();
			MF_SetAmxString(amx,params[4],(*a).getName(),params[5]);
			for(int i = 1; i < 8; ++i)
				cpBodyHits[i] = (*a).bodyHits[i];
			return --a ? index : 0;
		}	
	}
	
	return 0;
}

static cell AMX_NATIVE_CALL get_statsnum(AMX *amx, cell *params)
{
	return g_rank.getRankNum();
}


static cell AMX_NATIVE_CALL register_cwpn(AMX *amx, cell *params){ // name,logname,melee=0 
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

static cell AMX_NATIVE_CALL cwpn_dmg(AMX *amx, cell *params){ // wid,att,vic,dmg,hp=0
	int weapon = params[1];
	if (  weapon < TFCMAX_WEAPONS-TFCMAX_CUSTOMWPNS ){ // only for custom weapons
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid custom weapon id %d", weapon);
		return 0;
	}

	int att = params[2];
	CHECK_PLAYERRANGE(att);

	int vic = params[3];
	CHECK_PLAYERRANGE(vic);
	
	int dmg = params[4];
	if ( dmg<1 ){
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid damage amount %d", dmg);
		return 0;
	}
	
	int aim = params[5];
	if ( aim < 0 || aim > 7 ){
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid aim %d", aim);
		return 0;
	}

	CPlayer* pAtt = GET_PLAYER_POINTER_I(att);
	CPlayer* pVic = GET_PLAYER_POINTER_I(vic);

	if ( !pAtt ) pAtt = pVic;
	pVic->pEdict->v.dmg_inflictor = NULL;
	pAtt->saveHit( pVic , weapon , dmg, aim );

	int TA = 0;
	if ( (pVic->pEdict->v.team == pAtt->pEdict->v.team ) && ( pVic != pAtt) )
		TA = 1;
	MF_ExecuteForward(g_damage_info, pAtt->index, pVic->index, dmg, weapon, aim, TA);
	
	if ( pVic->IsAlive() )
		return 1;

	pAtt->saveKill(pVic,weapon,( aim == 1 ) ? 1:0 ,TA);
	MF_ExecuteForward(g_death_info, pAtt->index, pVic->index, weapon, aim, TA);

	return 1;
}

static cell AMX_NATIVE_CALL cwpn_shot(AMX *amx, cell *params){ // player,wid
	int index = params[2];
	CHECK_PLAYERRANGE(index);

	int weapon = params[1];
	if (  weapon < TFCMAX_WEAPONS-TFCMAX_CUSTOMWPNS ){
		MF_LogError(amx,AMX_ERR_NATIVE, "Invalid custom weapon id %d", weapon);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	pPlayer->saveShot(weapon);

	return 1;
}

AMX_NATIVE_INFO stats_Natives[] = {
	{ "get_stats",      get_stats},
	{ "get_statsnum",   get_statsnum},
	{ "get_user_astats",  get_user_astats },
	{ "get_user_rstats",  get_user_rstats },
	{ "get_user_lstats",  get_user_rstats }, // for backward compatibility
	{ "get_user_stats",   get_user_stats },
	{ "get_user_vstats",  get_user_vstats },
	{ "get_user_wrstats",  get_user_wrstats},             // DEC-Weapon(Round) Stats
	{ "get_user_wstats",  get_user_wstats},
	{ "reset_user_wstats",  reset_user_wstats },

	// Custom Weapon Support
	{ "custom_weapon_add", register_cwpn },
	{ "custom_weapon_dmg", cwpn_dmg },
	{ "custom_weapon_shot", cwpn_shot },

	{ NULL, NULL }
};

