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
#include <engine>
#include <ns>

#define START_DISTANCE	32			// The first search distance for finding a free location in the map
#define MAX_ATTEMPTS		128			// How many times to search in an area for a free space
#define BLOCKED_MASKS		MASK_PLAYER_STUNNED | MASK_ENSNARED | MASK_ALIEN_EMBRYO

new Float:g_lastcmdtime[33]

public plugin_init() {
	register_plugin("UnStuck",AMXX_VERSION_STR,"AMXX Dev Team")
	register_cvar("amx_unstuck_frequency", "4.0")
	register_clcmd("say_team /stuck", "unStuck")
	register_clcmd("say /stuck", "unStuck")
	register_clcmd("say_team /unstuck", "unStuck")
	register_clcmd("say /unstuck", "unStuck")
}

public unStuck(id) {
	new Float:minfrequency = get_cvar_float("amx_unstuck_frequency")
	new Float:elapsedcmdtime = get_gametime() - g_lastcmdtime[id]
	if ( elapsedcmdtime < minfrequency ) {
		client_print(id, print_chat, "[AMXX] You must wait %.1f seconds before trying to free yourself", minfrequency - elapsedcmdtime)
		return PLUGIN_HANDLED
	}
	g_lastcmdtime[id] = get_gametime()

	if (ns_get_mask(id, BLOCKED_MASKS)) {
		client_print(id, print_chat, "[AMXX] You cannot free yourself while evolving, stunned or webbed")
		return PLUGIN_CONTINUE
	}

	new Float:origin[3], Float:new_origin[3], hullsize, distance

	hullsize = getHullSize(id)
	if (!hullsize) {
		return PLUGIN_CONTINUE
	}

	entity_get_vector(id, EV_VEC_origin, origin)
	distance = START_DISTANCE

	while( distance < 1000 ) {	// 1000 is just incase, should never get anywhere near that
		for (new i = 0; i < MAX_ATTEMPTS; ++i) {
			new_origin[0] = random_float(origin[0]-distance,origin[0]+distance)
			new_origin[1] = random_float(origin[1]-distance,origin[1]+distance)
			new_origin[2] = random_float(origin[2]-distance,origin[2]+distance)

			if ( trace_hull(new_origin, hullsize, id) == 0 ) {
				entity_set_origin(id, new_origin)
				return PLUGIN_CONTINUE
			}
		}
		distance += START_DISTANCE
	}
	client_print(id, print_chat, "[AMXX] Couldn't find a free spot to move you too.")
	return PLUGIN_CONTINUE
}

getHullSize(id) {
	switch (ns_get_class(id)) {
		case 1,2,3:
			return HULL_HEAD
		case 4,6,7,8:
			return (entity_get_int(id, EV_INT_flags) & FL_DUCKING) ? HULL_HEAD : HULL_HUMAN
		case 5:
			return (entity_get_int(id, EV_INT_flags) & FL_DUCKING) ? HULL_HUMAN : HULL_LARGE
		default: {
			client_print(id, print_chat, "[AMXX] You cannot free yourself at this time.")
			return false
		}
	}
	return false
}
