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

/* Weapon names aren't send in WeaponList message in DoD */
weapon_t weaponData[] = 
{
	{ false,	false,	{"mortar"},			{"mortar"},			0, DODWT_OTHER },
	{ true,		true,	{"amerknife"},		{"knife"},			0, DODWT_MELEE }, // aknife->bknife
	{ false,	true,	{"gerknife"},		{"knife"},			0, DODWT_MELEE },
	{ false,	false,	{"colt"},			{"Colt"},				4, DODWT_SECONDARY }, 
	{ false,	false,	{"luger"},			{"Luger"},			4, DODWT_SECONDARY },
	{ true,		false,	{"garand"},			{"Garand"},			3, DODWT_PRIMARY }, // Garand->Garand butt
	{ false,	false,	{"scopedkar"},		{"scoped K98"},		3, DODWT_PRIMARY }, 
	{ false,	false,	{"thompson"},		{"Thompson"},			1, DODWT_PRIMARY },
	{ false,	false,	{"mp44"},			{"STG44"},			6, DODWT_PRIMARY },
	{ false,	false,	{"spring"},			{"Springfield"},		5, DODWT_PRIMARY },  
	{ true,		false,	{"kar"},			{"K98"},				3, DODWT_PRIMARY }, // KAR->KAR bayonet
	{ false,	false,	{"bar"},			{"BAR"},				6, DODWT_PRIMARY },
	{ false,	false,	{"mp40"},			{"MP40"},				1, DODWT_PRIMARY }, 
	{ false,	false,	{"grenade"},		{"handgrenade"},		9, DODWT_GRENADE },
	{ false,	false,	{"grenade2"},		{"stickgrenade"},		11, DODWT_GRENADE },
	{ false,	false,	{"stickgrenade_ex"},{"stickgrenade_ex"},	11, DODWT_GRENADE },
	{ false,	false,	{"handgrenade_ex"},	{"handgrenade_ex"},	 9, DODWT_GRENADE },
	{ false,	false,	{"mg42"},			{"MG42"},				 7, DODWT_PRIMARY },
	{ false,	false,	{"30cal"},			{".30 cal"},			 8, DODWT_PRIMARY },
	{ false,	true,	{"spade"},			{"spade"},			 0, DODWT_MELEE },
	{ true,		false,	{"m1carbine"},		{"M1 Carbine"},		 2, DODWT_PRIMARY },  // M1 Carbine->Folding Carbine
	{ false,	false,	{"mg34"},			{"MG34"},				 2, DODWT_PRIMARY },
	{ false,	false,	{"greasegun"},		{"Greasegun"},		 1, DODWT_PRIMARY },
	{ true,		false,	{"fg42"},			{"FG42"},				 6, DODWT_PRIMARY }, // FG42 -> scoped FG42
	{ true,		false,	{"k43"},			{"K43"},				 2, DODWT_PRIMARY },
	{ true,		false,	{"enfield"},		{"Enfield"},			 3, DODWT_PRIMARY }, // Enfield->Scoped Enfield->Enfield bayonet
	{ false,	false,	{"sten"},			{"Sten"},				 1, DODWT_PRIMARY },
	{ false,	false,	{"bren"},			{"Bren"},				 6, DODWT_PRIMARY }, 
	{ false,	false,	{"webley"},			{"Webley"},			 4, DODWT_PRIMARY },
	{ false,	false,	{"bazooka"},		{"Bazooka"},			13, DODWT_PRIMARY },
	{ false,	false,	{"pschreck"},		{"Panzerschrek"},		13, DODWT_PRIMARY },
	{ false,	false,	{"piat"},			{"Piat"},				13, DODWT_PRIMARY },
	{ false,	false,	{"scoped_fg42"},	{"scoped FG42"},		 6, DODWT_PRIMARY },
	{ false,	false,	{"fcarbine"},		{"Folding Carbine"},	 0, DODWT_PRIMARY },
	{ false,	true,	{"bayonet"},		{"K98 bayonet"},		 0, DODWT_MELEE }, // KAR bayonet
	{ false,	false,	{"scoped_enfield"},	{"scoped Enfield"},	 3, DODWT_PRIMARY },
	{ false,	false,	{"mills_bomb"},		{"mills bomb"},		 9, DODWT_GRENADE },
	{ false,	true,	{"brit_knife"},		{"knife"},			 0, DODWT_MELEE }, 
	{ false,	true,	{"garandbutt"},		{"Garand butt"},		 0, DODWT_MELEE }, // Garand butt
	{ false,	true,	{"enf_bayonet"},	{"Enfield bayonet"},	 0, DODWT_MELEE },
	{ false,	false,	{"mortar"},			{"mortar"},			 0, DODWT_OTHER }, // mortar new id
	{ false,	true,	{"k43butt"},		{"K43 butt"},			 0, DODWT_MELEE },
};

/* Function will select correct id */
int get_weaponid(CPlayer* pPlayer)
{
	int weapon = pPlayer->current;
	const char *sz;
	switch(weapon) 
	{
	case  1: 
		if(g_map.detect_allies_country) weapon = 37; 
		break;

	case  5: 
		if(pPlayer->pEdict->v.button&IN_ATTACK2) weapon = 38; 
		break;

	case 10: 
		if(pPlayer->pEdict->v.button&IN_ATTACK2) weapon = 34; 
		break;

	case 20: 
		if(g_map.detect_allies_paras) weapon = 33;
		break;

	case 23: 
        sz = STRING(pPlayer->pEdict->v.weaponmodel);
        if(sz[13] == 's')
			weapon = 32; 	
		break;

	case 24: 
		if(pPlayer->pEdict->v.button&IN_ATTACK2) weapon = 41; 
		break;

	case 25:
        sz = STRING(pPlayer->pEdict->v.weaponmodel);
        if(sz[16] == 's')
			 weapon = 35; 
		else if(pPlayer->pEdict->v.button&IN_ATTACK2)
			 weapon = 39;
		break;

	case 15:
        weapon = 14;
        break;

    case 16:
        if(g_map.detect_allies_country) weapon = 36;
        else weapon = 13;
        break;
	}

	return weapon;
}

traceVault traceData[] = 
{
	{ "grenade", 13, ACT_NADE_PUT|ACT_NADE_SHOT, 2.0 }, // or 36
	{ "grenade2", 14, ACT_NADE_PUT|ACT_NADE_SHOT, 2.0 },  
	{ "shell_bazooka", 29, ACT_ROCKET_PUT|ACT_ROCKET_SHOT, 2.0 },
	{ "shell_pschreck", 30, ACT_ROCKET_PUT|ACT_ROCKET_SHOT, 2.0 },
	{ "shell_piat", 31, ACT_ROCKET_PUT|ACT_ROCKET_SHOT, 2.0 },
	{ "monster_mortar", 40, ACT_NADE_PUT|ACT_NADE_SHOT, 2.0 },
};

bool ignoreBots (edict_t *pEnt, edict_t *pOther)
{
	if(!rankBots && (pEnt->v.flags & FL_FAKECLIENT || (pOther && pOther->v.flags & FL_FAKECLIENT)))
		return true;

	return false;
}

bool isModuleActive()
{
	if(!(int)CVAR_GET_FLOAT("dodstats_pause"))
		return true;

	return false;
}

edict_t *FindEntityByString(edict_t *pentStart, const char *szKeyword, const char *szValue)
{
	edict_t *pentEntity;
	pentEntity = FIND_ENTITY_BY_STRING(pentStart, szKeyword, szValue);

	if(!FNullEnt(pentEntity))
		return pentEntity;

	return NULL;
}

edict_t *FindEntityByClassname(edict_t *pentStart, const char *szName)
{
	return FindEntityByString(pentStart, "classname", szName);
}

edict_t *FindEntityInSphere(edict_t *pentStart, edict_t *origin, float radius)
{
	edict_t* temp = FIND_ENTITY_IN_SPHERE(pentStart, origin->v.origin, radius);

	if(!temp)
		return NULL;

	return temp;
}
