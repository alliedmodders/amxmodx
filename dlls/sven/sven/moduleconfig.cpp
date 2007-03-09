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
// EXTERNS
//

int gmsgScoreInfo;
KeyValueData g_kvd;
Trie<char,String> g_allyNameTrie, g_enemyNameTrie;

//
// GLOBALS
//

int g_lastDeadflag[33] = { 0 }, g_grenadeCount = 0,
g_spawnFwd = -1, g_healFwd = -1, g_grenadeFwd = -1;

edict_t *g_grenadeList[32] = { NULL };

//
// AMXX HOOKS
//

void OnAmxxAttach()
{
	MF_AddNatives(svencoop_Exports);

	// sc_set_displayname
	g_kvd.szClassName = "";
	g_kvd.szKeyName = "";
	g_kvd.szValue = "";
	g_kvd.fHandled = 0;

	// sc_get_displayname, default displaynames
	g_allyNameTrie.Insert("monster_alien_babyvoltigore", 27, String("Friendly Baby Voltigore"));
	g_enemyNameTrie.Insert("monster_alien_babyvoltigore", 27, String("Baby Voltigore"));
	g_allyNameTrie.Insert("monster_alien_controller", 24, String("Friendly Alien Controller"));
	g_enemyNameTrie.Insert("monster_alien_controller", 24, String("Alien Controller"));
	g_allyNameTrie.Insert("monster_alien_grunt", 19, String("Friendly Alien Grunt"));
	g_enemyNameTrie.Insert("monster_alien_grunt", 19, String("Alien Grunt"));
	g_allyNameTrie.Insert("monster_alien_slave", 19, String("Friendly Alien Slave"));
	g_enemyNameTrie.Insert("monster_alien_slave", 19, String("Alien Slave"));
	g_allyNameTrie.Insert("monster_vortigaunt", 18, String("Friendly Alien Slave"));
	g_enemyNameTrie.Insert("monster_vortigaunt", 18, String("Alien Slave"));
	g_allyNameTrie.Insert("monster_alien_voltigore", 23, String("Friendly Voltigore"));
	g_enemyNameTrie.Insert("monster_alien_voltigore", 23, String("Voltigore"));
	g_allyNameTrie.Insert("monster_apache", 14, String("Apache"));
	g_enemyNameTrie.Insert("monster_apache", 14, String("Apache"));
	g_allyNameTrie.Insert("monster_blkop_apache", 20, String("Apache"));
	g_enemyNameTrie.Insert("monster_blkop_apache", 20, String("Apache"));
	g_allyNameTrie.Insert("monster_assassin_repel", 22, String("Friendly Male Assassin"));
	g_enemyNameTrie.Insert("monster_assassin_repel", 22, String("Male Assassin"));
	g_allyNameTrie.Insert("monster_male_assassin", 21, String("Friendly Male Assassin"));
	g_enemyNameTrie.Insert("monster_male_assassin", 21, String("Male Assassin"));
	g_allyNameTrie.Insert("monster_babycrab", 16, String("Friendly Head Crab"));
	g_enemyNameTrie.Insert("monster_babycrab", 16, String("Head Crab"));
	g_allyNameTrie.Insert("monster_headcrab", 16, String("Friendly Head Crab"));
	g_enemyNameTrie.Insert("monster_headcrab", 16, String("Head Crab"));
	g_allyNameTrie.Insert("monster_babygarg", 16, String("Friendly Baby Gargantua"));
	g_enemyNameTrie.Insert("monster_babygarg", 16, String("Baby Gargantua"));
	g_allyNameTrie.Insert("monster_barnacle", 16, String("Barnacle"));
	g_enemyNameTrie.Insert("monster_barnacle", 16, String("Barnacle"));
	g_allyNameTrie.Insert("monster_barney", 14, String("Barney"));
	g_enemyNameTrie.Insert("monster_barney", 14, String("Barnabus"));
	g_allyNameTrie.Insert("monster_bigmomma", 16, String("Friendly Big Momma"));
	g_enemyNameTrie.Insert("monster_bigmomma", 16, String("Big Momma"));
	g_allyNameTrie.Insert("monster_blkop_osprey", 20, String("Friendly Black Ops Osprey"));
	g_enemyNameTrie.Insert("monster_blkop_osprey", 20, String("Black Ops Osprey"));
	g_allyNameTrie.Insert("monster_bloater", 15, String("Friendly Bloater"));
	g_enemyNameTrie.Insert("monster_bloater", 15, String("Bloater"));
	g_allyNameTrie.Insert("monster_bullchicken", 19, String("Friendly Bull Squid"));
	g_enemyNameTrie.Insert("monster_bullchicken", 19, String("Bull Squid"));
	g_allyNameTrie.Insert("monster_chumtoad", 16, String("Chubby"));
	g_enemyNameTrie.Insert("monster_chumtoad", 16, String("Chumtoad"));
	g_allyNameTrie.Insert("monster_cleansuit_scientist", 27, String("Cleansuit Scientist"));
	g_enemyNameTrie.Insert("monster_cleansuit_scientist", 27, String("Cleansuit Scientist"));
	g_allyNameTrie.Insert("monster_cockroach", 17, String("Roach"));
	g_enemyNameTrie.Insert("monster_cockroach", 17, String("Roach"));
	g_allyNameTrie.Insert("monster_gargantua", 17, String("Friendly Gargantua"));
	g_enemyNameTrie.Insert("monster_gargantua", 17, String("Gargantua"));
	g_allyNameTrie.Insert("monster_gman", 12, String("Government Man"));
	g_enemyNameTrie.Insert("monster_gman", 12, String("Government Man"));
	g_allyNameTrie.Insert("monster_gonome", 14, String("Friendly Gonome"));
	g_enemyNameTrie.Insert("monster_gonome", 14, String("Gonome"));
	g_allyNameTrie.Insert("monster_houndeye", 16, String("Friendly Hound Eye"));
	g_enemyNameTrie.Insert("monster_houndeye", 16, String("Hound Eye"));
	g_allyNameTrie.Insert("monster_human_assassin", 22, String("Friendly Female Assassin"));
	g_enemyNameTrie.Insert("monster_human_assassin", 22, String("Female Assassin"));
	g_allyNameTrie.Insert("monster_human_grunt", 19, String("Friendly Human Grunt"));
	g_enemyNameTrie.Insert("monster_human_grunt", 19, String("Human Grunt"));
	g_allyNameTrie.Insert("monster_grunt_repel", 19, String("Friendly Human Grunt"));
	g_enemyNameTrie.Insert("monster_grunt_repel", 19, String("Human Grunt"));
	g_allyNameTrie.Insert("monster_human_grunt_ally", 24, String("Ally Grunt"));
	g_enemyNameTrie.Insert("monster_human_grunt_ally", 24, String("Enemy Grunt"));
	g_allyNameTrie.Insert("monster_grunt_ally_repel", 24, String("Ally Grunt"));
	g_enemyNameTrie.Insert("monster_grunt_ally_repel", 24, String("Enemy Grunt"));
	g_allyNameTrie.Insert("monster_human_medic_ally", 24, String("Medic Grunt"));
	g_enemyNameTrie.Insert("monster_human_medic_ally", 24, String("Enemy Medic Grunt"));
	g_allyNameTrie.Insert("monster_medic_ally_repel", 24, String("Medic Grunt"));
	g_enemyNameTrie.Insert("monster_medic_ally_repel", 24, String("Enemy Medic Grunt"));
	g_allyNameTrie.Insert("monster_human_torch_ally", 24, String("Torch Grunt"));
	g_enemyNameTrie.Insert("monster_human_torch_ally", 24, String("Enemy Torch Grunt"));
	g_allyNameTrie.Insert("monster_torch_ally_repel", 24, String("Torch Grunt"));
	g_enemyNameTrie.Insert("monster_torch_ally_repel", 24, String("Enemy Torch Grunt"));
	g_allyNameTrie.Insert("monster_hwgrunt", 15, String("Friendly Heavy Weapons Grunt"));
	g_enemyNameTrie.Insert("monster_hwgrunt", 15, String("Heavy Weapons Grunt"));
	g_allyNameTrie.Insert("monster_hwgrunt_repel", 21, String("Friendly Heavy Weapons Grunt"));
	g_enemyNameTrie.Insert("monster_hwgrunt_repel", 21, String("Heavy Weapons Grunt"));
	g_allyNameTrie.Insert("monster_ichthyosaur", 19, String("Friendly Ichthyosaur"));
	g_enemyNameTrie.Insert("monster_ichthyosaur", 19, String("Ichthyosaur"));
	g_allyNameTrie.Insert("monster_leech", 13, String("Leech"));
	g_enemyNameTrie.Insert("monster_leech", 13, String("Leech"));
	g_allyNameTrie.Insert("monster_miniturret", 18, String("Mini-Turret"));
	g_enemyNameTrie.Insert("monster_miniturret", 18, String("Mini-Turret"));
	g_allyNameTrie.Insert("monster_nihilanth", 17, String(""));
	g_enemyNameTrie.Insert("monster_nihilanth", 17, String(""));
	g_allyNameTrie.Insert("monster_osprey", 14, String("Friendly Osprey Helicopter"));
	g_enemyNameTrie.Insert("monster_osprey", 14, String("Osprey Helicopter"));
	g_allyNameTrie.Insert("monster_otis", 12, String("Otis"));
	g_enemyNameTrie.Insert("monster_otis", 12, String("Enemy Otis"));
	g_allyNameTrie.Insert("monster_pitdrone", 16, String("Friendly Pit Drone"));
	g_enemyNameTrie.Insert("monster_pitdrone", 16, String("Pit Drone"));
	g_allyNameTrie.Insert("monster_rat", 11, String("Rat"));
	g_enemyNameTrie.Insert("monster_rat", 11, String("Rat"));
	g_allyNameTrie.Insert("monster_robogrunt", 17, String("Friendly Robo Grunt"));
	g_enemyNameTrie.Insert("monster_robogrunt", 17, String("Robo Grunt"));
	g_allyNameTrie.Insert("monster_robogrunt_repel", 23, String("Friendly Robo Grunt"));
	g_enemyNameTrie.Insert("monster_robogrunt_repel", 23, String("Robo Grunt"));
	g_allyNameTrie.Insert("monster_scientist", 17, String("Scientist"));
	g_enemyNameTrie.Insert("monster_scientist", 17, String("Scientist"));
	g_allyNameTrie.Insert("monster_sitting_scientist", 25, String("Scientist"));
	g_enemyNameTrie.Insert("monster_sitting_scientist", 25, String("Scientist"));
	g_allyNameTrie.Insert("monster_sentry", 14, String("Sentry Turret"));
	g_enemyNameTrie.Insert("monster_sentry", 14, String("Sentry Turret"));
	g_allyNameTrie.Insert("monster_shockroach", 18, String("Friendly Shock Roach"));
	g_enemyNameTrie.Insert("monster_shockroach", 18, String("Shock Roach"));
	g_allyNameTrie.Insert("monster_shocktrooper", 20, String("Friendly Shock Trooper"));
	g_enemyNameTrie.Insert("monster_shocktrooper", 20, String("Shock Trooper"));
	g_allyNameTrie.Insert("monster_snark", 13, String("Snark"));
	g_enemyNameTrie.Insert("monster_snark", 13, String("Snark"));
	g_allyNameTrie.Insert("monster_tentacle", 16, String("Tentacle"));
	g_enemyNameTrie.Insert("monster_tentacle", 16, String("Tentacle"));
	g_allyNameTrie.Insert("monster_tentaclemaw", 19, String("Tentacle"));
	g_enemyNameTrie.Insert("monster_tentaclemaw", 19, String("Tentacle"));
	g_allyNameTrie.Insert("monster_turret", 14, String("Turret"));
	g_enemyNameTrie.Insert("monster_turret", 14, String("Turret"));
	g_allyNameTrie.Insert("monster_zombie", 14, String("Friendly Zombie"));
	g_enemyNameTrie.Insert("monster_zombie", 14, String("Zombie"));
	g_allyNameTrie.Insert("monster_zombie_barney", 21, String("Friendly Zombie Barney"));
	g_enemyNameTrie.Insert("monster_zombie_barney", 21, String("Zombie Barney"));
	g_allyNameTrie.Insert("monster_zombie_soldier", 22, String("Friendly Zombie Soldier"));
	g_enemyNameTrie.Insert("monster_zombie_soldier", 22, String("Zombie Soldier"));
}

void OnPluginsLoaded()
{
	g_spawnFwd = MF_RegisterForward("sc_client_spawn", ET_IGNORE, FP_CELL, FP_DONE);
	g_healFwd = MF_RegisterForward("sc_client_heal", ET_IGNORE, FP_CELL, FP_CELL, FP_FLOAT, FP_CELL, FP_CELL, FP_DONE);
	g_grenadeFwd = MF_RegisterForward("sc_grenade_throw", ET_IGNORE, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
}

//
// METAMOD HOOKS
//

/***GetEntityAPI2******************/

// sc_client_heal
void DispatchThink(edict_t *pEntity)
{
	if(g_healFwd != -1 && UTIL_IsMonster(pEntity))
		*((float *)pEntity->pvPrivateData + OFFSET_LAST_HEALTH) = pEntity->v.health;

	RETURN_META(MRES_IGNORED);
}

// sc_get_displayname
void DispatchKeyValue(edict_t *pentKeyvalue, KeyValueData *pkvd)
{
	// catch displayname and store it ourselves for our native
	// TODO: store this somewhere else besides in the pev
	if(FStrEq(pkvd->szKeyName, "displayname"))
		pentKeyvalue->v.message = ALLOC_STRING(pkvd->szValue);

	RETURN_META(MRES_IGNORED);
}

// sc_client_spawn
void ClientPutInServer(edict_t *pPlayer)
{
	g_lastDeadflag[ENTINDEX(pPlayer)] = -1;

	RETURN_META(MRES_IGNORED);
}

// sc_client_spawn / sc_client_heal
void PlayerPreThink(edict_t *pPlayer)
{
	if(g_spawnFwd != -1)
	{
		int index = ENTINDEX(pPlayer);

		if(g_lastDeadflag[index] == -1 || (g_lastDeadflag[index] != DEAD_NO && pPlayer->v.deadflag == DEAD_NO))
			MF_ExecuteForward(g_spawnFwd, index);

		g_lastDeadflag[index] = pPlayer->v.deadflag;
	}

	if(g_healFwd != -1)
		*((float *)pPlayer->pvPrivateData + OFFSET_LAST_HEALTH) = pPlayer->v.health;

	RETURN_META(MRES_IGNORED);
}

// sc_grenade_throw
void StartFrame()
{
	if(g_grenadeFwd == -1 || !g_grenadeCount)
		RETURN_META(MRES_IGNORED);

	edict_t *pGrenade = g_grenadeList[0];

	if(!FNullEnt(pGrenade) && pGrenade->v.owner)
	{
		const char *model = STRING(pGrenade->v.model);

		int wId = 0;
		switch(model[7])
		{
			case 'g': wId = SCW_ARGRENADE; break;	// models/grenade.mdl
			case 'w': wId = SCW_HANDGRENADE; break;	// models/w_grenade.mdl
			//case 'c': wId = SCW_BANANA; break;	// models/cretegibs.mdl
		}

		if(wId) MF_ExecuteForward(g_grenadeFwd, ENTINDEX(pGrenade->v.owner),
			ENTINDEX(pGrenade), wId, UTIL_IsPlayer(pGrenade->v.owner));
	}

	// shift the list
	g_grenadeList[0] = NULL;
	if(--g_grenadeCount)
	{
		for(int i=1;i<=g_grenadeCount;i++) g_grenadeList[i-1] = g_grenadeList[i];
		g_grenadeList[g_grenadeCount] = NULL;
	}

	RETURN_META(MRES_IGNORED);
}

/***GetEntityAPI2_Post*************/

// sc_set_frags
void ServerActivate_Post(struct edict_s *, int, int)
{
	gmsgScoreInfo = GET_USER_MSG_ID(PLID, "ScoreInfo", NULL);
	RETURN_META(MRES_IGNORED);
}

/***GetEngineAPI*******************/

// sc_client_heal
void EmitSound(edict_t *pEntity, int channel, const char *sample, float volume, float attenuation, int flags, int pitch)
{
	if(g_healFwd == -1 || !UTIL_IsPlayer(pEntity) || channel != CHAN_WEAPON || strcmp(sample, "items/medshot4.wav") != 0)
		RETURN_META(MRES_IGNORED);

	Vector trStart = pEntity->v.origin;

	if(pEntity->v.flags & FL_DUCKING) trStart.z += 12.0;
	else trStart.z += 28.0;

	Vector trEnd = trStart + gpGlobals->v_forward * 32;

	TraceResult tr;
	TRACE_LINE(trStart, trEnd, dont_ignore_monsters, pEntity, &tr);

	if(tr.flFraction == 1.0)
		TRACE_HULL(trStart, trEnd, dont_ignore_monsters, head_hull, pEntity, &tr);

	if(!FNullEnt(tr.pHit))
	{
		float amount = tr.pHit->v.health - *((float *)tr.pHit->pvPrivateData + OFFSET_LAST_HEALTH);

		if(UTIL_IsPlayer(tr.pHit))
			MF_ExecuteForward(g_healFwd, ENTINDEX(pEntity), ENTINDEX(tr.pHit), amount, 1, 1);

		else if(UTIL_IsMonster(tr.pHit))
			MF_ExecuteForward(g_healFwd, ENTINDEX(pEntity), ENTINDEX(tr.pHit), amount, 0, *((int *)tr.pHit->pvPrivateData + OFFSET_MONSTER_ALLY));
	}

	RETURN_META(MRES_IGNORED);
}

/***GetEngineAPI_Post**************/

// sc_grenade_throw
void SetModel_Post(edict_t *pEntity, const char *model)
{
	if(g_grenadeFwd == -1)
		RETURN_META(MRES_IGNORED);

	const char *classname = STRING(pEntity->v.classname);

	if(!pEntity->v.owner && classname[0] == 'g' && classname[2] == 'e' && model[7] == 'g')
	{
		if(g_grenadeCount < sizeof g_grenadeList)
			g_grenadeList[g_grenadeCount++] = pEntity;
	}

	RETURN_META(MRES_IGNORED);
}
