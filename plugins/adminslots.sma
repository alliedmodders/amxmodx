// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Slots Reservation Plugin
//

#include <amxmodx>
#include <amxmisc>

new g_ResPtr
new g_HidePtr
new g_sv_visiblemaxplayers

public plugin_init()
{
	register_plugin("Slots Reservation", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("adminslots.txt")
	register_dictionary("common.txt")
	g_ResPtr = register_cvar("amx_reservation", "0", FCVAR_PROTECTED)
	g_HidePtr = register_cvar("amx_hideslots", "0")
	g_sv_visiblemaxplayers = get_cvar_pointer("sv_visiblemaxplayers")
}

public plugin_cfg()
{
	set_task(3.0, "MapLoaded")
}

public MapLoaded()
{
	if (get_pcvar_num(g_HidePtr))
	{
		setVisibleSlots(get_playersnum(1), MaxClients - get_pcvar_num(g_ResPtr))
	}
}

public client_authorized(id)
{
	new players = get_playersnum(1)
	new limit = MaxClients - get_pcvar_num(g_ResPtr)

	if (access(id, ADMIN_RESERVATION) || (players <= limit))
	{
		if (get_pcvar_num(g_HidePtr))
			setVisibleSlots(players, limit)
		return
	}
	
 	server_cmd("kick #%d ^"%L^"", get_user_userid(id), id, "DROPPED_RES")
}

public client_remove(id)
{
	if (get_pcvar_num(g_HidePtr))
	{
		setVisibleSlots(get_playersnum(1), MaxClients - get_pcvar_num(g_ResPtr))
	}
}

setVisibleSlots(players, limit)
{
	new num = players + 1

	if (players == MaxClients)
		num = MaxClients
	else if (players < limit)
		num = limit
	
	set_pcvar_num(g_sv_visiblemaxplayers, num)
}
