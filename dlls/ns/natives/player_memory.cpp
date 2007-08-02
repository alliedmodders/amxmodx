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

// Float:ns_get_res(Player)
static cell AMX_NATIVE_CALL ns_get_res(AMX *amx, cell *params)
{
	if (GameMan.IsCombat())
	{
		return 0;
	}

	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected())
	{
		return 0;
	}

	if (!player->HasPrivateData())
	{
		return 0;
	}

	return amx_ftoc2(get_private_f(player->GetEdict(),MAKE_OFFSET(RESOURCES)));
}

// ns_set_res(Player,Float:res)
static cell AMX_NATIVE_CALL ns_set_res(AMX *amx, cell *params)
{
	if (GameMan.IsCombat())
	{
		return 0;
	}

	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected())
	{
		return 0;
	}

	if (!player->HasPrivateData())
	{
		return 0;
	}

	set_private_f(player->GetEdict(),MAKE_OFFSET(RESOURCES),amx_ctof2(params[2]));

	return 1;
}
// Float:ns_add_res(Player,Float:res)
static cell AMX_NATIVE_CALL ns_add_res(AMX *amx, cell *params)
{
	if (GameMan.IsCombat())
	{
		return 0;
	}

	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected())
	{
		return 0;
	}

	if (!player->HasPrivateData())
	{
		return 0;
	}

	return amx_ftoc2(inc_private_f(player->GetEdict(),MAKE_OFFSET(RESOURCES),amx_ctof2(params[2]),0.0,100.0));
}
// Float:ns_get_exp(Player)
static cell AMX_NATIVE_CALL ns_get_exp(AMX *amx, cell *params)
{
	if (!GameMan.IsCombat())
	{
		return 0;
	}

	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected())
	{
		return 0;
	}
	if (!player->HasPrivateData())
	{
		return 0;
	}

	return amx_ftoc2(get_private_f(player->GetEdict(),MAKE_OFFSET(EXP)));
}

// ns_set_exp(Player,Float:exp)
static cell AMX_NATIVE_CALL ns_set_exp(AMX *amx, cell *params)
{
	if (!GameMan.IsCombat())
	{
		return 0;
	}

	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected())
	{
		return 0;
	}
	if (!player->HasPrivateData())
	{
		return 0;
	}

	set_private_f(player->GetEdict(),MAKE_OFFSET(EXP),amx_ctof2(params[2]));

	return 1;
}
// Float:ns_add_exp(Player,Float:exp)
static cell AMX_NATIVE_CALL ns_add_exp(AMX *amx, cell *params)
{
	if (!GameMan.IsCombat())
	{
		return 0;
	}

	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected())
	{
		return 0;
	}
	if (!player->HasPrivateData())
	{
		return 0;
	}

	return amx_ftoc2(inc_private_f(player->GetEdict(),MAKE_OFFSET(EXP),amx_ctof2(params[2]),0.0));
}
 
// ns_get_points(Player)
static cell AMX_NATIVE_CALL ns_get_points(AMX *amx, cell *params)
{
	if (!GameMan.IsCombat())
	{
		return 0;
	}

	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected())
	{
		return 0;
	}

	if (!player->HasPrivateData())
	{
		return 0;
	}

	return get_private(player->GetEdict(),MAKE_OFFSET(POINTS));
}

// ns_set_points(Player,points)
static cell AMX_NATIVE_CALL ns_set_points(AMX *amx, cell *params)
{
	if (!GameMan.IsCombat())
	{
		return 0;
	}

	CreatePlayerPointer(amx, params[1]);

	if (!player->IsConnected())
	{
		return 0;
	}
	if (!player->HasPrivateData())
	{
		return 0;
	}

	set_private(player->GetEdict(),MAKE_OFFSET(POINTS),static_cast<int>(params[2]));
	return 1;
}
// ns_add_points(Player,points)
static cell AMX_NATIVE_CALL ns_add_points(AMX *amx, cell *params)
{
	if (!GameMan.IsCombat())
	{
		return 0;
	}

	CreatePlayerPointer(amx, params[1]);

	if (!player->IsConnected())
	{
		return 0;
	}
	if (!player->HasPrivateData())
	{
		return 0;
	}

	return inc_private(player->GetEdict(),MAKE_OFFSET(POINTS),static_cast<int>(params[2]),0,9);
}
static cell AMX_NATIVE_CALL ns_get_score(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected() || !player->HasPrivateData())
	{
		return 0;
	}
	return get_private(player->GetEdict(),MAKE_OFFSET(SCORE));
}
static cell AMX_NATIVE_CALL ns_set_score(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected() || !player->HasPrivateData())
	{
		return 0;
	}

	set_private(player->GetEdict(),MAKE_OFFSET(SCORE),static_cast<int>(params[2]));
	return 1;
}
static cell AMX_NATIVE_CALL ns_add_score(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected() || !player->HasPrivateData())
	{
		return 0;
	}

	return inc_private(player->GetEdict(),MAKE_OFFSET(SCORE),static_cast<int>(params[2]));
}
static cell AMX_NATIVE_CALL ns_get_deaths(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected() || !player->HasPrivateData())
	{
		return 0;
	}
	return get_private(player->GetEdict(),MAKE_OFFSET(DEATHS));
}
static cell AMX_NATIVE_CALL ns_set_deaths(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected() || !player->HasPrivateData())
	{
		return 0;
	}

	set_private(player->GetEdict(),MAKE_OFFSET(DEATHS),static_cast<int>(params[2]));
	return 1;
}
static cell AMX_NATIVE_CALL ns_add_deaths(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected() || !player->HasPrivateData())
	{
		return 0;
	}

	return inc_private(player->GetEdict(),MAKE_OFFSET(DEATHS),static_cast<int>(params[2]));
}

static cell AMX_NATIVE_CALL ns_get_hive_ability(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	int result = get_private(player->GetEdict(), MAKE_OFFSET(HIVEABILITY));

	return (params[2] > 0) ? (result >= params[2] - 1) : result;
}



AMX_NATIVE_INFO player_memory_natives[] = {

	{ "ns_get_res",				ns_get_res },
	{ "ns_set_res",				ns_set_res },
	{ "ns_add_res",				ns_add_res },

	{ "ns_get_exp",				ns_get_exp },
	{ "ns_set_exp",				ns_set_exp },
	{ "ns_add_exp",				ns_add_exp },

	{ "ns_get_points",			ns_get_points },
	{ "ns_set_points",			ns_set_points },
	{ "ns_add_points",			ns_add_points },

	{ "ns_set_score",			ns_set_score },
	{ "ns_get_score",			ns_get_score },
	{ "ns_add_score",			ns_add_score },

	{ "ns_get_deaths",			ns_get_deaths },
	{ "ns_set_deaths",			ns_set_deaths },
	{ "ns_add_deaths",			ns_add_deaths },

	{ "ns_get_hive_ability",	ns_get_hive_ability},



	{ NULL,						NULL }
};
void AddNatives_PlayerMemory()
{
	MF_AddNatives(player_memory_natives);
}
