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

new CvarReservation;
new CvarHideSlots;

new CvarHandleMaxVisiblePlayers;

public plugin_init()
{
	register_plugin("Slots Reservation", AMXX_VERSION_STR, "AMXX Dev Team");

	register_dictionary("adminslots.txt");
	register_dictionary("common.txt");

	hook_cvar_change(register_cvar("amx_reservation", "0", FCVAR_PROTECTED), "@OnReservationChange");
	hook_cvar_change(register_cvar("amx_hideslots", "0"), "@OnHideSlotsChange");

	CvarHandleMaxVisiblePlayers = get_cvar_pointer("sv_visiblemaxplayers");
}

@OnReservationChange(const handle, const oldValue[], const newValue[])
{
	CvarReservation = strtol(newValue);

	if (CvarHideSlots)
	{
		setVisibleSlots(get_playersnum(1), freeVisibleSlots());
	}
}

@OnHideSlotsChange(const handle, const oldValue[], const newValue[])
{
	CvarHideSlots = strtol(newValue);

	if (CvarReservation)
	{
		setVisibleSlots(get_playersnum(1), freeVisibleSlots());
	}
}

public client_authorized(id)
{
	new players = get_playersnum(1);
	new limit = freeVisibleSlots();

	if (access(id, ADMIN_RESERVATION) || (players <= limit))
	{
		if (CvarHideSlots)
		{
			setVisibleSlots(players, limit);
		}
		return;
	}

 	server_cmd("kick #%d ^"%L^"", get_user_userid(id), id, "DROPPED_RES");
}

public client_remove(id)
{
	if (CvarHideSlots)
	{
		setVisibleSlots(get_playersnum(1), freeVisibleSlots());
	}
}

setVisibleSlots(players, limit)
{
	new num = players + 1;

	if (players == MaxClients)
	{
		num = MaxClients;
	}
	else if (players < limit)
	{
		num = limit;
	}

	set_pcvar_num(CvarHandleMaxVisiblePlayers, num);
}

freeVisibleSlots()
{
	return MaxClients - CvarReservation;
}