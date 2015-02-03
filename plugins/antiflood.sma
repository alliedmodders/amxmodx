// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Anti Flood Plugin
//

#include <amxmodx>

new Float:g_Flooding[MAX_PLAYERS + 1] = {0.0, ...}
new g_Flood[MAX_PLAYERS + 1] = {0, ...}

new amx_flood_time;

public plugin_init()
{
	register_plugin("Anti Flood", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("antiflood.txt")
	register_clcmd("say", "chkFlood")
	register_clcmd("say_team", "chkFlood")
	amx_flood_time=register_cvar("amx_flood_time", "0.75")
}

public chkFlood(id)
{
	new Float:maxChat = get_pcvar_float(amx_flood_time)

	if (maxChat)
	{
		new Float:nexTime = get_gametime()
		
		if (g_Flooding[id] > nexTime)
		{
			if (g_Flood[id] >= 3)
			{
				client_print(id, print_notify, "** %L **", id, "STOP_FLOOD")
				g_Flooding[id] = nexTime + maxChat + 3.0
				return PLUGIN_HANDLED
			}
			g_Flood[id]++
		}
		else if (g_Flood[id])
		{
			g_Flood[id]--
		}
		
		g_Flooding[id] = nexTime + maxChat
	}

	return PLUGIN_CONTINUE
}
