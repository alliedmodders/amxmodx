/* AMX Mod X 
 *   Natural Selection Module 
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

#include "../sdk/amxxmodule.h"

#include "../ns.h" 

#include "../utilfunctions.h"
#include "../NEW_Util.h"

#include "../GameManager.h"
#include "../CPlayer.h"

#include "../AllocString.h"

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


	{ NULL,						NULL }
};
void AddNatives_Player()
{
	MF_AddNatives(player_natives);
}
