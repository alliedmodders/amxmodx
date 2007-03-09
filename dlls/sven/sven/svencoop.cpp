/* AMX Mod X
   *   Sven Co-op Module
   *
   * by the AMX Mod X Development Team
   *
   * This file is part of AMX Mod X.
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

#include "svencoop.h"

//
// GLOBALS
//

const char g_weapon_names[24][24] =
{
	"",
	"weapon_crowbar",
	"weapon_9mmhandgun",
	"weapon_357",
	"weapon_9mmAR",
	"",
	"weapon_crossbow",
	"weapon_shotgun",
	"weapon_rpg",
	"weapon_gauss",
	"weapon_egon",
	"weapon_hornetgun",
	"weapon_handgrenade",
	"weapon_tripmine",
	"weapon_satchel",
	"weapon_snark",
	"weapon_uziakimbo",
	"weapon_uzi",
	"weapon_medkit",
	"weapon_crowbar_electric",
	"weapon_pipewrench",
	"weapon_minigun",
	"weapon_grapple",
	"weapon_sniperrifle"
};

const int g_ammo_offsets[25] =
{
	-1,						// NONE = 0
	0,						// SCW_CROWBAR = 1
	OFFSET_9MM_AMMO,		// SCW_9MMHANDGUN = 2
	OFFSET_357_AMMO,		// SCW_357 = 3
	OFFSET_9MM_AMMO,		// SCW_9MMAR = 4
	-1,						// NONE = 5
	OFFSET_CROSSBOW_AMMO,	// SCW_CROSSBOW = 6
	OFFSET_SHOTGUN_AMMO,	// SCW_SHOTGUN = 7
	OFFSET_RPG_AMMO,		// SCW_RPG = 8
	OFFSET_ENERGY_AMMO,		// SCW_GAUSS = 9
	OFFSET_ENERGY_AMMO,		// SCW_EGON = 10
	OFFSET_HORNETGUN_AMMO,	// SCW_HORNETGUN = 11
	OFFSET_HANDGRENADE_AMMO,// SCW_HANDGRENADE = 12
	OFFSET_TRIPMINE_AMMO,	// SCW_TRIPMINE = 13
	OFFSET_SATCHEL_AMMO,	// SCW_SATCHEL = 14
	OFFSET_SNARK_AMMO,		// SCW_SNARK = 15
	OFFSET_9MM_AMMO,		// SCW_UZIAKIMBO = 16
	OFFSET_9MM_AMMO,		// SCW_UZI = 17
	OFFSET_MEDKIT_AMMO,		// SCW_MEDKIT = 18
	0,						// SCW_CROWBAR_ELECTRIC = 19
	0,						// SCW_PIPEWRENCH = 20
	OFFSET_MINIGUN_AMMO,	// SCW_MINIGUN = 21
	0,						// SCW_GRAPPLE = 22
	OFFSET_SNIPERRIFLE_AMMO,// SCW_SNIPERRIFLE = 23
	OFFSET_ARGRENADE_AMMO	// SCW_ARGRENADE = 24 
};

//
// MONSTER NATIVES
//

static cell AMX_NATIVE_CALL sc_get_frags(AMX *amx, cell *params) // sc_get_frags(index); = 1 arguments
{
	// Gets a monster's or player's frags
	// params[1] = monster/player index

	// not CHECK_MONSTER because this works for players
	CHECK_ENTITY(params[1]); 
	edict_t *pEdict = GETEDICT(params[1]);

	if(!UTIL_IsPlayer(pEdict) && !UTIL_IsMonster(pEdict))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a player or monster_* entity", params[1], STRING(pEdict->v.classname));
		return 0;
	}

	return amx_ftoc(*((float *)pEdict->pvPrivateData + OFFSET_MONSTER_FRAGS));
}

static cell AMX_NATIVE_CALL sc_set_frags(AMX *amx, cell *params) // sc_set_frags(index, Float:value); = 2 arguments
{
	// Sets a monster's or player's frags
	// params[1] = index = monster/player index
	// params[2] = (float) new frags

	// not CHECK_MONSTER because this works for players
	CHECK_ENTITY(params[1]); 
	edict_t *pEdict = GETEDICT(params[1]);

	if(!UTIL_IsPlayer(pEdict) && !UTIL_IsMonster(pEdict))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a player or monster_* entity", params[1], STRING(pEdict->v.classname));
		return 0;
	}

	float fValue = amx_ctof(params[2]);
	*((float *)pEdict->pvPrivateData + OFFSET_MONSTER_FRAGS) = fValue;
	
	if(UTIL_IsPlayer(pEdict))
	{
		pEdict->v.frags = fValue;

		// update scoreboard
		if(gmsgScoreInfo)
		{
			MESSAGE_BEGIN(MSG_ALL, gmsgScoreInfo);
			WRITE_BYTE(params[1]);
			WRITE_SHORT((int)fValue);
			WRITE_SHORT(*((int *)pEdict->pvPrivateData + OFFSET_PLAYER_DEATHS));
			MESSAGE_END();
		}
	}

	return 1;
}

static cell AMX_NATIVE_CALL sc_get_displayname(AMX *amx, cell *params) // sc_get_displayname(index, displayname[], len); = 3 arguments
{
	// Gets a monster's displayname
	// params[1] = monster index
	// params[2] = return variable
	// params[3] = variable len

	// check non-player (could be squadmaker)
	CHECK_NONPLAYER(params[1]);
	edict_t *pEdict = INDEXENT(params[1]);

	// check valid types
	const char *classname = STRING(pEdict->v.classname);
	if(strcmp(classname, "squadmaker") != 0 && strncmp(classname, "monster", 7) != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a monstermaker, squadmaker, or monster_* entity", params[1], classname);
		return 0;
	}

	// check for a custom one
	const char *displayname = STRING(pEdict->v.message);
	if(displayname[0])
	{
		MF_SetAmxString(amx, params[2], displayname, params[3]);
		return 1; // 1 means custom displayname
	}

	// maybe from a monstermaker? use its displayname.
	if(!strncmp(classname, "monster_", 8) && !FNullEnt(pEdict->v.owner))
	{
		const char *ownerclass = STRING(pEdict->v.owner->v.classname);
		if(!strcmp(ownerclass, "squadmaker") || !strcmp(ownerclass, "monstermaker"))
		{
			displayname = STRING(pEdict->v.owner->v.message);
			if(displayname[0])
			{
				return MF_SetAmxString(amx, params[2], displayname, params[3]);
				return 1; // 1 means custom displayname
			}
		}
	}

	String *name = NULL;

	if(*((int *)pEdict->pvPrivateData + OFFSET_MONSTER_ALLY))
		name = g_allyNameTrie.Retrieve(classname, strlen(classname));
	else
		name = g_enemyNameTrie.Retrieve(classname, strlen(classname));

	if(name != NULL)
	{
		MF_SetAmxString(amx, params[2], name->c_str(), params[3]);
		return -1; // -1 means default displayname
	}

	return 0; // invalid monster
}

static cell AMX_NATIVE_CALL sc_set_displayname(AMX *amx, cell *params) // sc_set_displayname(index, displayname[], {Float,Sql,Result,_}:...); = 2 arguments
{
	// Sets a monster's displayname
	// params[1] = monster index
	// params[2] = new displayname
	// params[3...] = formatting

	// check non-player (could be squadmaker)
	CHECK_NONPLAYER(params[1]);
	edict_t *pEdict = INDEXENT(params[1]);

	// check valid types
	const char *classname = STRING(pEdict->v.classname);
	if(strcmp(classname, "squadmaker") != 0 && strncmp(classname, "monster", 7) != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a monstermaker, squadmaker, or monster_* entity", params[1], classname);
		return 0;
	}

	// fetch string
	int len = 0;
	char *displayname = MF_FormatAmxString(amx, params, 2, &len);

	// set_kvd
	KeyValueData *kvd = &g_kvd;
	kvd->szClassName = const_cast<char *>(classname);
	kvd->szKeyName = const_cast<char *>("displayname");
	kvd->szValue = const_cast<char *>(displayname);
	kvd->fHandled = 0;
	gpGamedllFuncs->dllapi_table->pfnKeyValue(pEdict, kvd);

	// remember it
	pEdict->v.message = ALLOC_STRING(displayname);

	return 1;
}

static cell AMX_NATIVE_CALL sc_is_player_ally(AMX *amx, cell *params) // sc_is_player_ally(index); = 1 arguments
{
	// Checks if a monster is a player ally
	// params[1] = monster index

	CHECK_MONSTER(params[1]);
	edict_t *pEdict = INDEXENT(params[1]);

	return *((int *)pEdict->pvPrivateData + OFFSET_MONSTER_ALLY);
}

//
// PLAYER NATIVES
// (ammo excluded)
//

static cell AMX_NATIVE_CALL sc_get_user_longjump(AMX *amx, cell *params) // sc_get_user_longjump(index); = 1 arguments
{
	// Checks if a player can longjump
	// params[1] = player index

	CHECK_PLAYER(params[1]);
	edict_t *pEdict = MF_GetPlayerEdict(params[1]);

	const char *buffer = (*g_engfuncs.pfnGetPhysicsKeyValue)(pEdict, "slj");
	if(buffer[0] == '1') return 1;

	return 0;
}

static cell AMX_NATIVE_CALL sc_set_user_longjump(AMX *amx, cell *params) // sc_set_user_longjump(index, value); = 2 arguments
{
	// Sets if a player can longjump
	// params[1] = player index
	// params[2] = new value

	CHECK_PLAYER(params[1]);
	edict_t *pEdict = MF_GetPlayerEdict(params[1]);

	if(params[2]) (*g_engfuncs.pfnSetPhysicsKeyValue)(pEdict, "slj", "1");
	else (*g_engfuncs.pfnSetPhysicsKeyValue)(pEdict, "slj", "0");

	return 1;
}

static cell AMX_NATIVE_CALL sc_get_user_deaths(AMX *amx, cell *params) // sc_get_user_deaths(index); = 1 arguments
{
	// Gets the number of times a player has died (duh!)
	// params[1] = player index

	CHECK_PLAYER(params[1]);
	edict_t *pEdict = MF_GetPlayerEdict(params[1]);

	return *((int *)pEdict->pvPrivateData + OFFSET_PLAYER_DEATHS);
}

static cell AMX_NATIVE_CALL sc_set_user_deaths(AMX *amx, cell *params) // sc_set_user_deaths(index, value); = 2 arguments
{
	// Sets the number of times a player has died
	// params[1] = player index
	// params[2] = new death amount

	CHECK_PLAYER(params[1]);
	edict_t *pEdict = MF_GetPlayerEdict(params[1]);

	*((int *)pEdict->pvPrivateData + OFFSET_PLAYER_DEATHS) = params[2];

	// update scoreboard
	if(gmsgScoreInfo)
	{
		MESSAGE_BEGIN(MSG_ALL, gmsgScoreInfo);
		WRITE_BYTE(params[1]);
		WRITE_SHORT((int)pEdict->v.frags);
		WRITE_SHORT(params[2]);
		MESSAGE_END();
	}

	*static_cast<int *>(MF_PlayerPropAddr(params[1], Player_Deaths)) = params[2];

	return 1;
}

//
// AMMO NATIVES
//

static cell AMX_NATIVE_CALL sc_get_wbox_ammo(AMX *amx, cell *params) // sc_get_wbox_ammo(index); = 1 arguments
{
	// Gets the amount of ammo in dropped ammo weaponbox
	// params[1] = weaponbox entity index

	CHECK_NONPLAYER(params[1]);
	edict_t *pEdict = INDEXENT(params[1]);

	if(strcmp(STRING(pEdict->v.classname), "weaponbox") != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a weaponbox entity", params[1], STRING(pEdict->v.classname));
		return 0;
	}

	return *((int *)pEdict->pvPrivateData + OFFSET_WBOX_AMMO);
}

static cell AMX_NATIVE_CALL sc_set_wbox_ammo(AMX *amx, cell *params) // sc_set_wbox_ammo(index, value); = 1 arguments
{
	// Sets the amount of ammo in dropped ammo weaponbox
	// params[1] = weaponbox entity index
	// params[2] = new ammo amount

	CHECK_NONPLAYER(params[1]);
	edict_t *pEdict = INDEXENT(params[1]);

	if(strcmp(STRING(pEdict->v.classname), "weaponbox") != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a weaponbox entity", params[1], STRING(pEdict->v.classname));
		return 0;
	}

	*((int *)pEdict->pvPrivateData + OFFSET_WBOX_AMMO) = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL sc_get_weapon_id(AMX *amx, cell *params) // sc_get_weapon_id(index); = 1 arguments
{
	// Gets the SCW_* constant of a weapon_* entity
	// params[1] = weapon_* entity index

	CHECK_NONPLAYER(params[1]);
	edict_t *pEdict = INDEXENT(params[1]);

	// not a valid weapon_*
	if(strncmp(STRING(pEdict->v.classname), "weapon_", 7) != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a weapon_* entity", params[1], STRING(pEdict->v.classname));
		return 0;
	}

	return *((int *)pEdict->pvPrivateData + OFFSET_WEAPON_TYPE);
}

static cell AMX_NATIVE_CALL sc_get_weapon_ammo(AMX *amx, cell *params) // sc_get_weapon_ammo(index1, index2=0); = 2 arguments
{
	// Gets the amount of ammo in weapon's clip
	// params[1] = weapon_* entity index OR player index
	// params[2] = (optional) SCW_* constant if using player index

	CHECK_ENTITY(params[1]);
	edict_t *pEdict = GETEDICT(params[1]);

	// sc_get_weapon_ammo(id, SCW_9MMAR);
	if(params[2])
	{
		if(params[2] < 1 || params[2] > 23 || params[2] == 5)
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon index %d", params[2]);
			return 0;
		}

		CHECK_PLAYER(params[1]);
		edict_t *pWeapon = NULL;

		const char *weaponname = g_weapon_names[params[2]];

		while(true)
		{
			pWeapon = FIND_ENTITY_BY_STRING(pWeapon, "classname", weaponname);
			if(FNullEnt(pWeapon) || pWeapon->v.owner == pEdict) break;
		}

		if(FNullEnt(pWeapon))
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Player %d does not have a \"%s\" (index %d)", params[1], weaponname, params[2]);
			return 0;
		}

		return *((int *)pWeapon->pvPrivateData + OFFSET_WEAPON_CLIP);
	}

	if(strncmp(STRING(pEdict->v.classname), "weapon_", 7) != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a weapon_* entity", params[1], STRING(pEdict->v.classname));
		return 0;
	}

	return *((int *)pEdict->pvPrivateData + OFFSET_WEAPON_CLIP);
}

static cell AMX_NATIVE_CALL sc_set_weapon_ammo(AMX *amx, cell *params) // sc_set_weapon_ammo(index1, value, index2=0); = 3 arguments
{
	// Sets the amount of ammo in weapon's clip
	// params[1] = weapon_* entity index OR player index
	// params[2] = new clip ammo
	// params[3] = (optional) SCW_* constant if using player index

	CHECK_ENTITY(params[1]);
	edict_t *pEdict = GETEDICT(params[1]);

	// sc_set_weapon_ammo(id, SCW_9MMAR, 50);
	if(params[3])
	{
		if(params[3] < 1 || params[3] > 23 || params[3] == 5)
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon index %d", params[3]);
			return 0;
		}

		CHECK_PLAYER(params[1]);
		edict_t *pWeapon = NULL;

		const char *weaponname = g_weapon_names[params[3]];

		while(true)
		{
			pWeapon = FIND_ENTITY_BY_STRING(pWeapon, "classname", weaponname);
			if(FNullEnt(pWeapon) || pWeapon->v.owner == pEdict) break;
		}

		if(FNullEnt(pWeapon))
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Player %d does not have a \"%s\" (index %d)", params[1], weaponname, params[3]);
			return 0;
		}

		*((int *)pWeapon->pvPrivateData + OFFSET_WEAPON_CLIP) = params[2];

		return 1;
	}

	if(strncmp(STRING(pEdict->v.classname), "weapon_", 7) != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a weapon_* entity", params[1], STRING(pEdict->v.classname));
		return 0;
	}

	*((int *)pEdict->pvPrivateData + OFFSET_WEAPON_CLIP) = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL sc_get_user_bpammo(AMX *amx, cell *params) // sc_get_user_bpammo(index, weapon); = 2 arguments
{
	// Gets the amount of ammo in player's backpack for a specific weapon
	// params[1] = player index
	// params[2] = SCW_* constant

	CHECK_PLAYER(params[1]);
	edict_t *pEdict = MF_GetPlayerEdict(params[1]);

	if(params[2] < 1 || params[2] > 23 || params[2] == 5)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon index %d", params[2]);
		return 0;
	}

	// invalid weapon or no bpammo
	if(g_ammo_offsets[params[2]] <= 0)
		return 0;

	return *((int *)pEdict->pvPrivateData + g_ammo_offsets[params[2]]);
}

static cell AMX_NATIVE_CALL sc_set_user_bpammo(AMX *amx, cell *params) // sc_set_user_bpammo(index, weapon, value); = 3 arguments
{
	// Gets the amount of ammo in player's backpack for a specific weapon
	// params[1] = player index
	// params[2] = SCW_* constant
	// params[3] = new backpack ammo

	CHECK_PLAYER(params[1]);
	edict_t *pEdict = MF_GetPlayerEdict(params[1]);

	if(params[2] < 1 || params[2] > 23 || params[2] == 5)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon index %d", params[2]);
		return 0;
	}

	// invalid weapon or no bpammo
	if(g_ammo_offsets[params[2]] <= 0)
		return 0;

	*((int *)pEdict->pvPrivateData + g_ammo_offsets[params[2]]) = params[3];

	return 1;
}

//
// EXPORT LIST
//

AMX_NATIVE_INFO svencoop_Exports[] =
{
	// monster natives
	{"sc_get_frags",			sc_get_frags},
	{"sc_set_frags",			sc_set_frags},
	{"sc_get_displayname",		sc_get_displayname},
	{"sc_set_displayname",		sc_set_displayname},
	{"sc_is_player_ally",		sc_is_player_ally},

	// player natives
	{"sc_get_user_longjump",	sc_get_user_longjump},
	{"sc_set_user_longjump",	sc_set_user_longjump},
	{"sc_get_user_deaths",		sc_get_user_deaths},
	{"sc_set_user_deaths",		sc_set_user_deaths},

	// ammo natives
	{"sc_get_wbox_ammo",		sc_get_wbox_ammo},
	{"sc_set_wbox_ammo",		sc_set_wbox_ammo},
	{"sc_get_weapon_id",		sc_get_weapon_id},
	{"sc_get_weapon_ammo",		sc_get_weapon_ammo},
	{"sc_set_weapon_ammo",		sc_set_weapon_ammo},
	{"sc_get_user_bpammo",		sc_get_user_bpammo},
	{"sc_set_user_bpammo",		sc_set_user_bpammo},
	//------------------- <-- max 19 characters?? bleh!
	{NULL,						NULL}
};
