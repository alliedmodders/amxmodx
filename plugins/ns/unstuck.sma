// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// UnStuck Plugin
//

#include <amxmodx>
#include <ns>

#define START_DISTANCE	32			// The first search distance for finding a free location in the map
#define MAX_ATTEMPTS		128			// How many times to search in an area for a free space
#define BLOCKED_MASKS		MASK_PLAYER_STUNNED | MASK_ENSNARED | MASK_ALIEN_EMBRYO

new Float:g_lastcmdtime[MAX_PLAYERS + 1]

new amx_unstuck_frequency;

public plugin_init() 
{
	register_plugin("UnStuck",AMXX_VERSION_STR,"AMXX Dev Team")
	amx_unstuck_frequency=register_cvar("amx_unstuck_frequency", "4.0")
	register_clcmd("say_team /stuck", "unStuck")
	register_clcmd("say /stuck", "unStuck")
	register_clcmd("say_team /unstuck", "unStuck")
	register_clcmd("say /unstuck", "unStuck")
}

public unStuck(id) 
{
	new Float:minfrequency = get_pcvar_float(amx_unstuck_frequency);
	new Float:elapsedcmdtime = get_gametime() - g_lastcmdtime[id]
	if ( elapsedcmdtime < minfrequency ) 
	{
		client_print(id, print_chat, "[AMXX] You must wait %.1f seconds before trying to free yourself", minfrequency - elapsedcmdtime)
		return PLUGIN_HANDLED
	}
	g_lastcmdtime[id] = get_gametime()
	
	new val;
	
	// 1=success,0=no valid spot,-1=invalid state,-2=invalid class
	if ((val=ns_unstick_player(id,START_DISTANCE,MAX_ATTEMPTS))!=1)
	{
		switch(val)
		{
			case 0:
			{
				client_print(id, print_chat, "[AMXX] Couldn't find a free spot to move you too.");
			}
			case -1:
			{
				client_print(id, print_chat, "[AMXX] You cannot free yourself while stunned or webbed.");
			}
			case -2:
			{
				client_print(id, print_chat, "[AMXX] You cannot free yourself as a commander or while evolving.");
			}
			case -3:
			{
				client_print(id, print_chat, "[AMXX] You cannot free yourself as a spectator or from the ready room.");
			}
		}
	}
	return PLUGIN_CONTINUE;
}
