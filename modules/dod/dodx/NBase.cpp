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
// DODX Module
//

#include "amxxmodule.h"
#include "dodx.h"

#define WEAPONLIST 71

/* Weapon names aren't send in WeaponList message in DoD */
weaponlist_s weaponlist[] = 
{ 
	{ 0,     0,	  0,	false}, // 0,
	{ -1,    0,	 -1,	true }, // DODW_AMERKNIFE = 1,
	{ -1,    0,	 -1,	true }, // DODW_GERKNIFE,
	{  4,   64,	  7,	true }, // DODW_COLT,
	{  4,   64,	  8,	true }, // DODW_LUGER,
	{  3,  128,	  8,	true }, // DODW_GARAND,
	{  3,  128,	  5,	true }, // DODW_SCOPED_KAR,
	{  1,  128,	 30,	true }, // DODW_THOMPSON,
	{  6,  128,	 30,	true }, // DODW_STG44,
	{  5,  128,	  5,	true }, // DODW_SPRINGFIELD,
	{  3,  128,	  5,	true }, // DODW_KAR,
	{  6,  128,	 20,	true }, // DODW_BAR,
	{  1,  130,	 30,	true }, // DODW_MP40,
	{  9,   24,	 -1,	true }, // DODW_HANDGRENADE,
	{ 11,   24,	 -1,	true }, // DODW_STICKGRENADE,
	{ 12,   24,	 -1,	true }, // DODW_STICKGRENADE_EX,
	{ 10,   24,	 -1,	true }, // DODW_HANDGRENADE_EX,
	{  7, 2178,	250,	true }, // DODW_MG42,
	{  8,  130,	150,	true }, // DODW_30_CAL,
	{ -1,    0,	 -1,	true }, // DODW_SPADE,
	{  2,  128,	 15,	true }, // DODW_M1_CARBINE,
	{  2,  130,	 75,	true }, // DODW_MG34,
	{  1,  128,	 30,	true }, // DODW_GREASEGUN,
	{  6,  128,	 20,	true }, // DODW_FG42,
	{  2,  128,	 10,	true }, // DODW_K43,
	{  3,  128,	 10,	true }, // DODW_ENFIELD,
	{  1,  128,	 30,	true }, // DODW_STEN,
	{  6,  128,	 30,	true }, // DODW_BREN,
	{  4,   64,	  6,	true }, // DODW_WEBLEY,
	{ 13,  642,	  1,	true }, // DODW_BAZOOKA,
	{ 13,  642,	  1,	true }, // DODW_PANZERSCHRECK,
	{ 13,  642,	  1,	true }, // DODW_PIAT,
	{  3,  128,	 20,	true }, // DODW_SCOPED_FG42, UNSURE ABOUT THIS ONE
	{  2,  128,	 15,	true }, // DODW_FOLDING_CARBINE,
	{  0,    0,	  0,	false}, // DODW_KAR_BAYONET,
	{  3,  128,	 10,	true }, // DODW_SCOPED_ENFIELD, UNSURE ABOUT THIS ONE
	{  9,   24,	 -1,	true }, // DODW_MILLS_BOMB,
	{ -1,    0,	 -1,	true }, // DODW_BRITKNIFE,
	{ 38,    0,	  0,	false}, // DODW_GARAND_BUTT,
	{ 39,    0,	  0,	false}, // DODW_ENFIELD_BAYONET,
	{ 40,    0,	  0,	false}, // DODW_MORTAR,
	{ 41,    0,	  0,	false}, // DODW_K43_BUTT,
};

// from id to name 3 params id, name, len
static cell AMX_NATIVE_CALL get_weapon_name(AMX *amx, cell *params)
{ 
	int id = params[1];

	if(id < 0 || id >= DODMAX_WEAPONS)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon id %d", id);
		return 0;
	}

	return MF_SetAmxString(amx,params[2],weaponData[id].name,params[3]);
}

// from log to name
static cell AMX_NATIVE_CALL wpnlog_to_name(AMX *amx, cell *params)
{ 
	int iLen;
	char *log = MF_GetAmxString(amx,params[1],0,&iLen);

	for(int i = 0; i < DODMAX_WEAPONS; i++)
	{
		if(strcmp(log,weaponData[i].logname ) == 0)
			return MF_SetAmxString(amx,params[2],weaponData[i].name,params[3]);
	}
	return 0;
}

// from log to id
static cell AMX_NATIVE_CALL wpnlog_to_id(AMX *amx, cell *params)
{ 
	int iLen;
	char *log = MF_GetAmxString(amx, params[1], 0, &iLen);

	for(int i = 0; i < DODMAX_WEAPONS; i++)
	{
		if(strcmp(log,weaponData[i].logname) == 0)
			return i;
	}
	return 0;
}

// from id to log
static cell AMX_NATIVE_CALL get_weapon_logname(AMX *amx, cell *params)
{ 
	int id = params[1];

	if (id<0 || id>=DODMAX_WEAPONS)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon id %d", id);
		return 0;
	}

	return MF_SetAmxString(amx,params[2],weaponData[id].logname,params[3]);
}

static cell AMX_NATIVE_CALL is_melee(AMX *amx, cell *params)
{
	int id = params[1];

	if(id < 0 || id >= DODMAX_WEAPONS)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon id %d", id);
		return 0;
	}

	return weaponData[id].melee;
}

static cell AMX_NATIVE_CALL get_team_score(AMX *amx, cell *params)
{
	int index = params[1];

	switch ( index )
	{
	case 1:
		return AlliesScore;
		break;

	case 2:
		return AxisScore;
		break;
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_user_score(AMX *amx, cell *params)
{
	int index = params[1];
	CHECK_PLAYER(index);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame)
		return (cell)pPlayer->savedScore;

	return -1;
}

static cell AMX_NATIVE_CALL get_user_class(AMX *amx, cell *params)
{
	int index = params[1];
	CHECK_PLAYER(index);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame)
		return pPlayer->pEdict->v.playerclass;

	return 0;
}

static cell AMX_NATIVE_CALL user_kill(AMX *amx, cell *params)
{
	int index = params[1];
	CHECK_PLAYER(index);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if(pPlayer->ingame && pPlayer->IsAlive())
	{
		pPlayer->killPlayer();
		return 1;
	}

	return 0;
}	

static cell AMX_NATIVE_CALL get_map_info(AMX *amx, cell *params)
{
	switch(params[1])
	{
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
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid map info id %d", params[1]);
		break;
	}
	return -1;
}

static cell AMX_NATIVE_CALL get_user_pronestate(AMX *amx, cell *params)
{
	int index = params[1];
	CHECK_PLAYER(index);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame)
			return pPlayer->pEdict->v.iuser3;

	return 0;
}

static cell AMX_NATIVE_CALL get_user_weapon(AMX *amx, cell *params)
{
	int index = params[1];
	CHECK_PLAYER(index);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

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

/* We want to get just the weapon of whichever type that the player is on him */
static cell AMX_NATIVE_CALL dod_weapon_type(AMX *amx, cell *params) /* 2 params */
{
	int index = params[1];
	int type = params[2];

	CHECK_PLAYER(index);

	if(type < DODWT_PRIMARY || type > DODWT_OTHER)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon type id %d", type);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if(pPlayer->ingame)
	{
		int weaponsbit = pPlayer->pEdict->v.weapons & ~(1<<31); // don't count last element

		for(int x = 1; x < DODMAX_WEAPONS; ++x)
		{
			if((weaponsbit&(1<<x)) > 0)
			{
				if(weaponData[x].type == type)
					return x;
			}
		}
	}

	return 0;
}

// forward 
static cell AMX_NATIVE_CALL register_forward(AMX *amx, cell *params)
{ 

	#ifdef FORWARD_OLD_SYSTEM
		int iFunctionIndex;
		int err;
		switch( params[1] )
		{
		case 0:
			if((err = MF_AmxFindPublic(amx, "client_damage", &iFunctionIndex)) == AMX_ERR_NONE)
				g_damage_info.put( amx , iFunctionIndex );

			else
				MF_LogError(amx, err, "client_damage not found");
				return 0;
			break;

		case 1:
			if((err = MF_AmxFindPublic(amx, "client_death", &iFunctionIndex)) == AMX_ERR_NONE)
				g_death_info.put( amx , iFunctionIndex );

			else
				MF_LogError(amx, err, "client_Death not found");
				return 0;
			break;

		case 2:
			if((err = MF_AmxFindPublic(amx, "client_score", &iFunctionIndex)) == AMX_ERR_NONE)
				g_score_info.put( amx , iFunctionIndex );

			else
				MF_LogError(amx, err, "client_score not found");
				return 0;
			break;

		default:
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid forward id %d", params[2]);
			return 0;
		}
	#endif

	return 1;
}

// name,logname,melee=0 
static cell AMX_NATIVE_CALL register_cwpn(AMX *amx, cell *params)
{ 
	int i;
	bool bFree = false;

	for(i = DODMAX_WEAPONS - DODMAX_CUSTOMWPNS; i < DODMAX_WEAPONS; i++)
	{
		if(!weaponData[i].needcheck)
		{
			bFree = true;
			break;
		}
	}

	if(!bFree)
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

// wid,att,vic,dmg,hp=0
static cell AMX_NATIVE_CALL cwpn_dmg(AMX *amx, cell *params)
{ 
	int weapon = params[1];

	// only for custom weapons
	if(weapon < DODMAX_WEAPONS-DODMAX_CUSTOMWPNS)
	{ 
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid custom weapon id %d", weapon);
		return 0;
	}

	int att = params[2];
	CHECK_PLAYER(params[2]);

	int vic = params[3];
	CHECK_PLAYER(params[3]);
	
	int dmg = params[4];
	if(dmg<1)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid damage %d", dmg);
		return 0;
	}
	
	int aim = params[5];
	if(aim < 0 || aim > 7)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid aim %d", aim);
		return 0;
	}

	CPlayer* pAtt = GET_PLAYER_POINTER_I(att);
	CPlayer* pVic = GET_PLAYER_POINTER_I(vic);

	pVic->pEdict->v.dmg_inflictor = NULL;

	if(!pAtt) 
		pAtt = pVic;

	if(pAtt->index != pVic->index)
		pAtt->saveHit(pVic , weapon , dmg, aim);

	int TA = 0;

	if((pVic->pEdict->v.team == pAtt->pEdict->v.team) && (pVic != pAtt))
		TA = 1;

	MF_ExecuteForward(iFDamage,pAtt->index, pVic->index, dmg, weapon, aim, TA);

	if(pVic->IsAlive())
		return 1;

	pAtt->saveKill(pVic,weapon,( aim == 1 ) ? 1:0 ,TA);

	MF_ExecuteForward(iFDeath,pAtt->index, pVic->index, weapon, aim, TA);

	return 1;
}

// player,wid
static cell AMX_NATIVE_CALL cwpn_shot(AMX *amx, cell *params)
{ 
	int index = params[2];

	CHECK_PLAYER(index);

	int weapon = params[1];
	if(weapon < DODMAX_WEAPONS-DODMAX_CUSTOMWPNS)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid custom weapon id %d", weapon);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	pPlayer->saveShot(weapon);

	return 1;
}

static cell AMX_NATIVE_CALL get_maxweapons(AMX *amx, cell *params)
{
	return DODMAX_WEAPONS;
}

static cell AMX_NATIVE_CALL get_stats_size(AMX *amx, cell *params)
{
	return 9;
}

static cell AMX_NATIVE_CALL is_custom(AMX *amx, cell *params)
{
	int weapon = params[1];

	if(weapon < DODMAX_WEAPONS-DODMAX_CUSTOMWPNS)
	{
		return 0;
	}
	return 1;
}

// player,wid
static cell AMX_NATIVE_CALL dod_get_user_team(AMX *amx, cell *params)
{ 
	int index = params[1];
	CHECK_PLAYER(index);

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	return pPlayer->pEdict->v.team;

}

// player,wid
static cell AMX_NATIVE_CALL get_user_team(AMX *amx, cell *params)
{ 
	int index = params[1];
	CHECK_PLAYER(index);

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	int iTeam = pPlayer->pEdict->v.team; 

	if ( params[3] )
	{ 
		const char *szTeam = ""; 
		switch(iTeam)
		{
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

static cell AMX_NATIVE_CALL dod_set_model(AMX *amx, cell *params) // player,model
{
	int index = params[1];
	CHECK_PLAYER(index);

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if(!pPlayer->ingame)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid Player, Not on Server");
		return 0;
	}

	int length;
	pPlayer->initModel((char*)STRING(ALLOC_STRING(MF_GetAmxString(amx, params[2], 1, &length))));

	return true;
}

static cell AMX_NATIVE_CALL dod_set_body(AMX *amx, cell *params) // player,bodynumber
{
	int index = params[1];
	CHECK_PLAYER(index);

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if(!pPlayer->ingame)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid Player, Not on Server");
		return 0;
	}

	pPlayer->setBody(params[2]);

	return true;
}

static cell AMX_NATIVE_CALL dod_clear_model(AMX *amx, cell *params) // player
{
	int index = params[1];
	CHECK_PLAYER(index);

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if(!pPlayer->ingame)
		return false;

	pPlayer->clearModel();

	return true;
}

/* 
0 [Byte]	1	// Weapons Groupings
1 [Byte]	210	// Total Rounds Allowed
2 [Byte]	-1	// Undefined Not Used
3 [Byte]	-1	// Undefined Not Used
4 [Byte]	2	// Weapon Slot
5 [Byte]	0	// Bucket ( Position Under Weapon Slot )
6 [Short]	7	// Weapon Number / Bit Field for the weapon
7 [Byte]	128	// Bit Field for the Ammo or Ammo Type
8 [Byte]	30	// Rounds Per Mag

id, wpnID, slot, position, totalrds
*/
static cell AMX_NATIVE_CALL dod_weaponlist(AMX *amx, cell *params) // player
{
	if(!weaponlist[params[1]].changeable)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "This Weapon Cannot be Changed");
		return 0;
	}

	int id = params[1];
	int wpnID = params[2];
	int slot = params[3];
	int position = params[4];
	int totalrds = params[5];

	UTIL_LogPrintf("ID (%d) WpnID (%d) Slot (%d) Pos (%d) Rounds (%d)", id, wpnID, slot, position, totalrds);

	CHECK_PLAYER(id);

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(id);
	if(!pPlayer->ingame)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid Player, Not on Server");
		return 0;
	}
	
	MESSAGE_BEGIN(MSG_ONE, GET_USER_MSG_ID(PLID, "WeaponList", NULL), NULL, pPlayer->pEdict);
	WRITE_BYTE(weaponlist[wpnID].grp);
		WRITE_BYTE(totalrds);
		WRITE_BYTE(-1);
		WRITE_BYTE(-1);
		WRITE_BYTE(slot - 1);
		WRITE_BYTE(position);
		WRITE_SHORT(wpnID);
		WRITE_BYTE(weaponlist[wpnID].bitfield);

		// Is it grenades
		if(wpnID == 13 || wpnID == 14 || wpnID == 15 || wpnID == 16 || wpnID == 36)
			WRITE_BYTE(-1);
		else if(wpnID == 29 || wpnID == 30 || wpnID == 31)
			WRITE_BYTE(1);
		else
			WRITE_BYTE(weaponlist[wpnID].clip);
	MESSAGE_END();

	return 1;
}



AMX_NATIVE_INFO base_Natives[] = 
{
	{ "dod_wpnlog_to_name", wpnlog_to_name },
	{ "dod_wpnlog_to_id", wpnlog_to_id },

	{ "dod_get_team_score", get_team_score },
	{ "dod_get_user_score", get_user_score },
	{ "dod_get_user_class", get_user_class },
	{ "dod_get_user_weapon", get_user_weapon },
	
	{ "dod_weapon_type", dod_weapon_type },

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

	{"dod_set_model",		dod_set_model},
	{"dod_set_body_number",	dod_set_body},
	{"dod_clear_model",		dod_clear_model},
	{"dod_set_weaponlist",	dod_weaponlist},

	///*******************
	{ NULL, NULL } 
};
