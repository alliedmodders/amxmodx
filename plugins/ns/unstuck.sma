/* AMX Mod X
*   UnStuck Plugin
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

#include <amxmodx>
#include <ns>

#define START_DISTANCE	32			// The first search distance for finding a free location in the map
#define MAX_ATTEMPTS		128			// How many times to search in an area for a free space
#define BLOCKED_MASKS		MASK_PLAYER_STUNNED | MASK_ENSNARED | MASK_ALIEN_EMBRYO

new Float:g_lastcmdtime[33]

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