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

#include "GameManager.h"
#include "CPlayer.h"

#include "AllocString.h"

StringManager AllocStringList;

// ns_set_player_model(id,const Model[]="")
static cell AMX_NATIVE_CALL ns_set_player_model(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	player->SetModel(MF_GetAmxString(amx,params[2],0,NULL));

	GameMan.HookPostThink_Post();

	return 1;
}

// ns_set_player_skin(id,skin=-1)
static cell AMX_NATIVE_CALL ns_set_player_skin(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	player->SetSkin(params[2]);

	GameMan.HookPostThink_Post();

	return 1;
}
// ns_set_player_body(id,body=-1)
static cell AMX_NATIVE_CALL ns_set_player_body(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	player->SetBody(params[2]);

	GameMan.HookPostThink_Post();

	return 1;
}
// ns_get_class(id)
static cell AMX_NATIVE_CALL ns_get_class(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	return player->GetClass();
}
// ns_get_jpfuel(id)
static cell AMX_NATIVE_CALL ns_get_jpfuel(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	REAL ret=(player->GetPev()->fuser3) / 10.0;

	return amx_ftoc2(ret);
}
// ns_set_jpfuel(id,Float:fuelpercent)
static cell AMX_NATIVE_CALL ns_set_jpfuel(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	REAL fuel = amx_ctof2(params[2]);
	if (fuel > 100.0)
	{
		fuel = 100.0;
	}
	if (fuel < 0.0)
	{
		fuel = 0.0;
	}

	player->GetPev()->fuser3 = fuel * 10.0;
	return 1;
}
static cell AMX_NATIVE_CALL ns_add_jpfuel(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	REAL fuel = clamp(amx_ctof2(params[2]),0.0,100.0);

	return amx_ftoc2(player->GetPev()->fuser3 = clamp(static_cast<float>(player->GetPev()->fuser3 + (fuel * 10.0)),static_cast<float>(0.0)));
};
// ns_get_speedchange(index)
static cell AMX_NATIVE_CALL ns_get_speedchange(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	return player->GetSpeedChange();
}

// ns_set_speedchange(index,speedchange=0)
static cell AMX_NATIVE_CALL ns_set_speedchange(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	player->SetSpeedChange(params[2]);

	// Update PreThink_Post if we need to
	GameMan.HookPreThink_Post();
	return 1;
}

// ns_get_maxspeed(index) (returns the max speed of the player BEFORE speed change is factored in.)
static cell AMX_NATIVE_CALL ns_get_maxspeed(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	return player->GetMaxSpeed();
}
// ns_set_fov(id,Float:fov);
static cell AMX_NATIVE_CALL ns_set_fov(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	return player->SetFOV(amx_ctof3(&params[2]));
}
// ns_giveiteM(id,"item");
static cell AMX_NATIVE_CALL ns_giveitem(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	char *classname = MF_GetAmxString(amx,params[2],0,NULL);

	if (!player->IsConnected())
	{
		return 0;
	}
	if (player->GetPev()->deadflag > 0)
	{
		return 0;
	}

	edict_t *object=CREATE_NAMED_ENTITY(ALLOC_STRING2(classname));

	if (!object)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Error creating entity \"%s\"", classname);
		return 0;
	}

	SET_ORIGIN(object,player->GetPev()->origin);						// move to player
	gpGamedllFuncs->dllapi_table->pfnSpawn(object);						// emulate spawn
	object->v.flags |= FL_ONGROUND;										// make it think it's touched the ground
	gpGamedllFuncs->dllapi_table->pfnThink(object);						// 
	gpGamedllFuncs->dllapi_table->pfnTouch(object,player->GetEdict());	// give it to the player

	return 1;
}

static cell AMX_NATIVE_CALL ns_get_user_team(AMX* amx, cell* params)
{
	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected())
	{
		return 0;
	}
	
	int team = player->GetPev()->team;
	
	if (team > 4 || team < 0)
	{
		MF_SetAmxString(amx, params[2], "undefinedteam", params[3]);
		
		return 0;
	}
	
	switch (team)
	{
		case 0:
		{
			// iuser1 means readyroom (I think!)
			if (player->GetPev()->iuser1 == 0)
			{
				MF_SetAmxString(amx, params[2], "undefinedteam", params[3]);
				
				return 0;
			}
			MF_SetAmxString(amx, params[2], "spectatorteam", params[3]);
			
			return 0;
		}
		case 1:
		{
			MF_SetAmxString(amx, params[2], "marine1team", params[3]);
			
			return 1;
		}
		case 2:
		{
			MF_SetAmxString(amx, params[2], "alien1team", params[3]);
			
			return 2;
		}
		case 3:
		{
			MF_SetAmxString(amx, params[2], "marine2team", params[3]);
			
			return 3;
		}
		case 4:
		{
			MF_SetAmxString(amx, params[2], "alien2team", params[3]);
			
			return 4;
		}
		default:
		{
			break;
		}
	}
	MF_SetAmxString(amx, params[2], "spectatorteam", params[3]);
	
	return 0;
}

AMX_NATIVE_INFO player_natives[] = {

	{ "ns_set_player_model",	ns_set_player_model },
	{ "ns_set_player_skin",		ns_set_player_skin },
	{ "ns_set_player_body",		ns_set_player_body },

	{ "ns_get_class",			ns_get_class },

	{ "ns_get_jpfuel",			ns_get_jpfuel },
	{ "ns_set_jpfuel",			ns_set_jpfuel },
	{ "ns_add_jpfuel",			ns_add_jpfuel },

	{ "ns_get_energy",			ns_get_jpfuel },  // They do the same thing...
	{ "ns_set_energy",			ns_set_jpfuel },  // 
	{ "ns_add_energy",			ns_add_jpfuel },

	{ "ns_get_speedchange",		ns_get_speedchange },
	{ "ns_set_speedchange",		ns_set_speedchange },
	{ "ns_get_maxspeed",		ns_get_maxspeed },

	{ "ns_set_fov",				ns_set_fov },

	{ "ns_give_item",			ns_giveitem },
	
	{ "ns_get_user_team",		ns_get_user_team },


	{ NULL,						NULL }
};
void AddNatives_Player()
{
	MF_AddNatives(player_natives);
}
