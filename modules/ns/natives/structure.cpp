// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Module
//

#include "amxxmodule.h"

#include "ns.h"

#include "utilfunctions.h"
#include "NEW_Util.h"

int IsValidBuilding[AVH_USER3_MAX + 1] = {
	0, // AVH_USER3_NONE = 0,
	0, // AVH_USER3_MARINE_PLAYER,
	0, // AVH_USER3_COMMANDER_PLAYER,
	0, // AVH_USER3_ALIEN_PLAYER1,
	0, // AVH_USER3_ALIEN_PLAYER2,
	0, // AVH_USER3_ALIEN_PLAYER3,
	0, // AVH_USER3_ALIEN_PLAYER4,
	0, // AVH_USER3_ALIEN_PLAYER5,
	0, // AVH_USER3_ALIEN_EMBRYO,
	0, // AVH_USER3_SPAWN_TEAMONE,
	0, // AVH_USER3_SPAWN_TEAMTWO,
	0, // AVH_USER3_PARTICLE_ON,				// only valid for AvHParticleEntity: entindex as int in fuser1, template index stored in fuser2
	0, // AVH_USER3_PARTICLE_OFF,				// only valid for AvHParticleEntity: particle system handle in fuser1
	0, // AVH_USER3_WELD,						// float progress (0 - 100) stored in fuser1
	0, // AVH_USER3_ALPHA,					// fuser1 indicates how much alpha this entity toggles to in commander mode, fuser2 for players
	0, // AVH_USER3_MARINEITEM,				// Something a friendly marine can pick up
	0, // AVH_USER3_WAYPOINT,
	2, // AVH_USER3_HIVE,
	0, // AVH_USER3_NOBUILD,
	0, // AVH_USER3_USEABLE,
	0, // AVH_USER3_AUDIO_ON,
	0, // AVH_USER3_AUDIO_OFF,
	0, // AVH_USER3_FUNC_RESOURCE,
	1, // AVH_USER3_COMMANDER_STATION,
	1, // AVH_USER3_TURRET_FACTORY, 
	1, // AVH_USER3_ARMORY, 
	1, // AVH_USER3_ADVANCED_ARMORY,
	1, // AVH_USER3_ARMSLAB,
	1, // AVH_USER3_PROTOTYPE_LAB, 
	1, // AVH_USER3_OBSERVATORY,
	0, // AVH_USER3_CHEMLAB,
	0, // AVH_USER3_MEDLAB,
	0, // AVH_USER3_NUKEPLANT,
	1, // AVH_USER3_TURRET,
	1, // AVH_USER3_SIEGETURRET,
	1, // AVH_USER3_RESTOWER,
	0, // AVH_USER3_PLACEHOLDER,
	1, // AVH_USER3_INFANTRYPORTAL,
	0, // AVH_USER3_NUKE,
	0, // AVH_USER3_BREAKABLE,
	0, // AVH_USER3_UMBRA,
	1, // AVH_USER3_PHASEGATE,
	2, // AVH_USER3_DEFENSE_CHAMBER,
	2, // AVH_USER3_MOVEMENT_CHAMBER,
	2, // AVH_USER3_OFFENSE_CHAMBER,
	2, // AVH_USER3_SENSORY_CHAMBER,
	2, // AVH_USER3_ALIENRESTOWER,
	0, // AVH_USER3_HEAVY,
	0, // AVH_USER3_JETPACK,
	1, // AVH_USER3_ADVANCED_TURRET_FACTORY,
	0, // AVH_USER3_SPAWN_READYROOM,
	0, // AVH_USER3_CLIENT_COMMAND,
	0, // AVH_USER3_FUNC_ILLUSIONARY,
	0, // AVH_USER3_MENU_BUILD,
	0, // AVH_USER3_MENU_BUILD_ADVANCED,
	0, // AVH_USER3_MENU_ASSIST,
	0, // AVH_USER3_MENU_EQUIP,
	0, // AVH_USER3_MINE,
	0 // AVH_USER3_MAX

};


// ns_build_structure(idStructure);
static cell AMX_NATIVE_CALL ns_build_structure(AMX *amx, cell *params)
{
	// Trick NS into thinking that this structure is being spawned from the map
	// set the "startbuilt" setting to 1, then remove it.
	// "startbuilt" is set as "spawnflag" "1" in the ns.fgd
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->free)
	{
		return 0;
	};

	if (Entity->v.iuser3 <= AVH_USER3_NONE || Entity->v.iuser3 >= AVH_USER3_MAX)
	{
		return 0;
	}


	int StructureType=IsValidBuilding[Entity->v.iuser3];
	if (StructureType==0)
	{
		return 0;
	}

	if (Entity->v.iuser3==AVH_USER3_HIVE)
	{
		return 0;
	}

	// Already set?
	if (Entity->v.spawnflags & 1)
	{
		MDLL_Spawn(Entity);

		goto undo_ghost;
	}

	Entity->v.spawnflags |= 1;

	MDLL_Spawn(Entity);

	Entity->v.spawnflags &= ~1;

undo_ghost:
	if (StructureType==1) // marine, remove "ghost" appearance
	{
		if (get_private(Entity,MAKE_OFFSET(GHOST_STRUCTURE))!=0)
		{
			set_private(Entity,MAKE_OFFSET(GHOST_STRUCTURE),0);

			Entity->v.rendermode=kRenderNormal;
			Entity->v.renderamt=100.0;
		}
	}

	return 1;
};

#define MASK_ELECTRICITY	8192
static cell AMX_NATIVE_CALL ns_get_build(AMX *amx, cell *params)
{
	int iLength;
	char *buildtype = MF_GetAmxString(amx,params[1],0,&iLength);
	int iBuiltOnly = params[2];
	int iNumber = params[3];
	edict_t* pBuild = NULL;
	int iCount=0;

	while ((pBuild = UTIL_FindEntityByString(pBuild,"classname",buildtype)) != NULL)
	{
		if (iBuiltOnly > 0)
		{
			if (FStrEq("team_advarmory",buildtype) || FStrEq("team_advturretfactory",buildtype))
			{
				iCount++;
			}
			else
			{
				if (pBuild->v.fuser1 >= 1000 || pBuild->v.iuser4 & MASK_ELECTRICITY)
				{
					iCount++;
				}
			}
		}
		else
		{
			iCount++;
		}
		if (iNumber > 0 && iCount == iNumber)
			return ENTINDEX_NEW(pBuild);
	}
	return iCount++;
}
static cell AMX_NATIVE_CALL ns_set_hive_trait(AMX *amx, cell *params)
{
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL)
	{
		return 0;
	}

	set_private(Entity,MAKE_OFFSET(HIVE_TRAIT),static_cast<int>(params[2]));
	return 1;
}
static cell AMX_NATIVE_CALL ns_get_hive_trait(AMX *amx, cell *params)
{
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL)
	{
		return 0;
	}

	return get_private(Entity,MAKE_OFFSET(HIVE_TRAIT));
}

static cell AMX_NATIVE_CALL ns_get_struct_owner(AMX *amx, cell *params)
{
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL)
	{
		return 0;
	}

	return get_private(Entity,MAKE_OFFSET(STRUCTOWNER));
}
// ns_set_struct_owner(idStructure,idPlayer) - -1 means no owner
static cell AMX_NATIVE_CALL ns_set_struct_owner(AMX *amx, cell *params)
{

	CreateNonPlayerEdict(amx,params[1]);

	if (params[2] > gpGlobals->maxClients || params[2] < -1)
	{
		return 0;
	}

	if (Entity->pvPrivateData == NULL)
	{
		return 0;
	}
	set_private(Entity,MAKE_OFFSET(STRUCTOWNER),params[2]);
	return 1;
}
// Float:ns_get_obs_energy(id);
static cell AMX_NATIVE_CALL ns_get_obs_energy(AMX *amx, cell *params)
{
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL)
	{
		return 0;
	}	

	return amx_ftoc2(get_private(Entity,MAKE_OFFSET(OBS_ENERGY)));
};
// Float:ns_set_obs_energy(id,Float:energy);
static cell AMX_NATIVE_CALL ns_set_obs_energy(AMX *amx, cell *params)
{
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL)
	{
		return 0;
	}

	set_private_f(Entity,MAKE_OFFSET(OBS_ENERGY),amx_ctof2(params[2]));
	return 1;
};

// Float:ns_add_obs_energy(id,Float:energy);
static cell AMX_NATIVE_CALL ns_add_obs_energy(AMX *amx, cell *params)
{
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL)
	{
		return 0;
	}

	return amx_ftoc2(inc_private_f(Entity,MAKE_OFFSET(OBS_ENERGY),amx_ctof2(params[2]),0.0,100.0));
};

// Float:ns_get_weld_time(idWeldable);
static cell AMX_NATIVE_CALL ns_get_weld_time(AMX *amx, cell *params)
{
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL)
	{
		return 0;
	}

	return amx_ftoc2(get_private_f(Entity,MAKE_OFFSET(WELD_TIME)));
};
// ns_set_weld_time(idWeldable,Float:value);
static cell AMX_NATIVE_CALL ns_set_weld_time(AMX *amx, cell *params)
{
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL)
	{
		return 0;
	}

	set_private_f(Entity,MAKE_OFFSET(WELD_TIME),amx_ctof2(params[2]));

	return 1;
};
// Float:ns_add_weld_time(idWeldable,Float:value);
static cell AMX_NATIVE_CALL ns_add_weld_time(AMX *amx, cell *params)
{
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL)
	{
		return 0;
	}

	return amx_ftoc2(inc_private_f(Entity,MAKE_OFFSET(WELD_TIME),amx_ctof2(params[2]),0.0));
};
// Float:ns_get_weld_done(idWeldable);
static cell AMX_NATIVE_CALL ns_get_weld_done(AMX *amx, cell *params)
{
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL)
	{
		return 0;
	}

	return amx_ftoc2(get_private_f(Entity,MAKE_OFFSET(WELD_DONE)));
};
// ns_set_weld_done(idWeldable,Float:value);
static cell AMX_NATIVE_CALL ns_set_weld_done(AMX *amx, cell *params)
{
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL)
	{
		return 0;
	}

	set_private_f(Entity,MAKE_OFFSET(WELD_DONE),amx_ctof2(params[2]));

	return 1;
};
// Float:ns_add_weld_done(idWeldable,Float:value);
static cell AMX_NATIVE_CALL ns_add_weld_done(AMX *amx, cell *params)
{
	CreateNonPlayerEdict(amx,params[1]);

	if (Entity->pvPrivateData == NULL)
	{
		return 0;
	}

	return amx_ftoc2(inc_private_f(Entity,MAKE_OFFSET(WELD_DONE),amx_ctof2(params[2]),0.0));
};


AMX_NATIVE_INFO structure_natives[] = {
	{ "ns_build_structure",		ns_build_structure },

	{ "ns_get_build",			ns_get_build },

	{ "ns_get_hive_trait",		ns_get_hive_trait },
	{ "ns_set_hive_trait",		ns_set_hive_trait },

	{ "ns_get_struct_owner",	ns_get_struct_owner },
	{ "ns_set_struct_owner",	ns_set_struct_owner },

	{ "ns_get_obs_energy",		ns_get_obs_energy },
	{ "ns_set_obs_energy",		ns_set_obs_energy },
	{ "ns_add_obs_energy",		ns_add_obs_energy },

	{ "ns_get_weld_time",		ns_get_weld_time },
	{ "ns_set_weld_time",		ns_set_weld_time },
	{ "ns_add_weld_time",		ns_add_weld_time },

	{ "ns_get_weld_done",			ns_get_weld_done },
	{ "ns_set_weld_done",			ns_set_weld_done },
	{ "ns_add_weld_done",			ns_add_weld_done },

	/* prototypes for natives i have planned */
	//{ "ns_get_phase_target",		ns_get_phase_target },
	//{ "ns_set_phase_target",		ns_set_phase_target },
	
	//{ "ns_set_phase_nextuse",		ns_set_phase_nextuse },
	//{ "ns_get_phase_nextuse",		ns_get_phase_nextuse },
	//{ "ns_add_phase_nextuse",		ns_add_phase_nextuse },

	{ NULL,						NULL }
};
void AddNatives_Structure()
{
	MF_AddNatives(structure_natives);
}
