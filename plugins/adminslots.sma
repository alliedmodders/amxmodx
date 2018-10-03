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

	hook_cvar_change(create_cvar("amx_reservation", "0", FCVAR_PROTECTED, .has_min = true, .min_val = 0.0, .has_max = true, .max_val = float(MaxClients - 1)), "@OnReservationChange");
	hook_cvar_change(create_cvar("amx_hideslots"  , "0", FCVAR_NONE     , .has_min = true, .min_val = 0.0, .has_max = true, .max_val = 1.0), "@OnHideSlotsChange");

	CvarHandleMaxVisiblePlayers = get_cvar_pointer("sv_visiblemaxplayers");
}

@OnReservationChange(const handle, const oldValue[], const newValue[])
{
	CvarReservation = strtol(newValue);

	setVisibleSlots();
}

@OnHideSlotsChange(const handle, const oldValue[], const newValue[])
{
	CvarHideSlots = strtol(newValue);

	setVisibleSlots();
}

public client_authorized(id)
{
	setVisibleSlots(id);
}

public client_remove(id)
{
	setVisibleSlots();
}

setVisibleSlots(id = 0)
{
	if ((id == 0 && !CvarHideSlots) || !CvarReservation)
	{
		if (get_pcvar_num(CvarHandleMaxVisiblePlayers) > 0)
		{
			resetVisibleSlots(MaxClients);
		}

		return;
	}

	new players = get_playersnum(1);
	new limit   = freeVisibleSlots();

	if (id != 0)
	{
		if (players > limit && !access(id, ADMIN_RESERVATION))
		{
			server_cmd("kick #%d ^"%L^"", get_user_userid(id), id, "DROPPED_RES");
			return;
		}

		if (!CvarHideSlots)
		{
			return;
		}
	}

	new num = players + 1;

	if (players == MaxClients)
	{
		num = MaxClients;
	}
	else if (players < limit)
	{
		num = limit;
	}

	resetVisibleSlots(num);
}

resetVisibleSlots(value)
{
	if (value == MaxClients)
	{
		value = -1; // Default sv_visiblemaxplayers value.
	}

	set_pcvar_num(CvarHandleMaxVisiblePlayers, value);
}

freeVisibleSlots()
{
	return MaxClients - CvarReservation;
}