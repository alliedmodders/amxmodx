/* AMX Mod X
*   Idle Player Remover
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
#include <amxmisc>

#define MIN_IDLE_TIME	30	// Prevents amx_idle_time being set to something silly.
#define WARNING_TIME	15		// Start warning the user this many seconds before they are about to be kicked.
#define CHECK_FREQ		5			// This is also the warning message frequency.
#define CLASS_GESTATE	9

new g_oldangles[33][3]
new g_idletime[33]
new bool:g_spawned[33] = {true, ...}
new g_class[33]	// stored info from the "ScoreInfo" message

new mp_tournamentmode;
new amx_idle_time;
new amx_idle_min_players;
new amx_idle_ignore_immunity;

new maxplayers;

public plugin_init() {
	register_plugin("Idle Player Remover",AMXX_VERSION_STR,"AMXX Dev Team") 
	
	amx_idle_time=register_cvar("amx_idle_time", "120")				// Kick people idle longer than this time
	amx_idle_min_players=register_cvar("amx_idle_min_players", "8")	// Only kick idle players when there is atleast this many players on the server
	amx_idle_ignore_immunity=register_cvar("amx_idle_ignore_immunity", "1")	// Kick admins with immunity?

	mp_tournamentmode=get_cvar_pointer("mp_tournamentmode");
	
	set_task(float(CHECK_FREQ),"checkPlayers",_,_,_,"b")
	register_event("ResetHUD", "playerSpawned", "be")
	if (cvar_exists("sv_structurelimit"))
	{
		// need the NS 3.2 ScoreInfo parser
		register_event("ScoreInfo","msgScoreInfo32","a")
	}
	else
	{
		register_event("ScoreInfo","msgScoreInfo","a")
	}
	
	
	maxplayers=get_maxplayers();
}

public checkPlayers() {
	if (get_pcvar_num(mp_tournamentmode)) return PLUGIN_HANDLED

	for (new i = 1; i <= maxplayers; i++) {
		if (is_user_alive(i) && g_class[i]!=CLASS_GESTATE && is_user_connected(i) && !is_user_bot(i) && !is_user_hltv(i) && g_spawned[i]) {
			if ( !get_pcvar_num(amx_idle_ignore_immunity) ) {
				if ( access(i, ADMIN_IMMUNITY) ) continue
			}
			new newangle[3]
			get_user_origin(i, newangle)

			if ( newangle[0] == g_oldangles[i][0] && newangle[1] == g_oldangles[i][1] && newangle[2] == g_oldangles[i][2] ) {
				g_idletime[i] += CHECK_FREQ
				check_idletime(i)
			} else {
				g_oldangles[i][0] = newangle[0]
				g_oldangles[i][1] = newangle[1]
				g_oldangles[i][2] = newangle[2]
				g_idletime[i] = 0
			}
		}
	}
	return PLUGIN_HANDLED
}

check_idletime(id) {
	new numplayers = get_playersnum()
					
	if (numplayers >= get_pcvar_num(amx_idle_min_players)) {
		new maxidletime = get_pcvar_num(amx_idle_time)
		if (maxidletime < MIN_IDLE_TIME) {
			log_message("cvar amx_idle_time %d is too low. Minimum value is %d.", maxidletime, MIN_IDLE_TIME)
			maxidletime = MIN_IDLE_TIME
			set_cvar_num("amx_idle_time", MIN_IDLE_TIME)
		}

		if ( maxidletime-WARNING_TIME <= g_idletime[id] < maxidletime) {
			new timeleft = maxidletime - g_idletime[id]
			client_print(id, print_chat, "[AMXX] You have %d seconds to move or you will be kicked for being idle", timeleft)
		} else if (g_idletime[id] > maxidletime) {
			new name[32]
			get_user_name(id, name, 31)
			client_print(0, print_chat, "[AMXX] %s was kicked for being idle longer than %d seconds", name, maxidletime)
			log_amx("%s was kicked for being idle longer than %d seconds", name, maxidletime)
			server_cmd("kick #%d ^"You were kicked for being idle longer than %d seconds^"", get_user_userid(id), maxidletime)
		}
	}
}

public client_connect(id) {
	g_idletime[id] = 0
	return PLUGIN_HANDLED
}

public client_putinserver(id) {
	g_idletime[id] = 0
	return PLUGIN_HANDLED
}

public playerSpawned(id) {
	g_spawned[id] = false
	new sid[1]
	sid[0] = id
	set_task(0.75, "delayedSpawn",_, sid, 1)	// Give the player time to drop to the floor when spawning
	return PLUGIN_HANDLED
}

public delayedSpawn(sid[]) {
	if (!is_user_alive(sid[0]))
		return PLUGIN_HANDLED

	get_user_origin(sid[0], g_oldangles[sid[0]])
	g_spawned[sid[0]] = true
	return PLUGIN_HANDLED
}

public msgScoreInfo() {
  new id=read_data(1);
  if (id>32||id<1) {
    // just incase..
    return;
  }
  g_class[id]=read_data(5);
}
public msgScoreInfo32() {
  new id=read_data(1);
  if (id>32||id<1) {
    // just incase..
    return;
  }
  g_class[id]=read_data(6);
}
