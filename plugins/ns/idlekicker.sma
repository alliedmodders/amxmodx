// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Idle Player Remover Plugin
//

#include <amxmodx>
#include <amxmisc>

#define MIN_IDLE_TIME	30	// Prevents amx_idle_time being set to something silly.
#define WARNING_TIME	15		// Start warning the user this many seconds before they are about to be kicked.
#define CHECK_FREQ		5			// This is also the warning message frequency.
#define CLASS_GESTATE	9

new g_oldangles[MAX_PLAYERS + 1][3]
new g_idletime[MAX_PLAYERS + 1]
new bool:g_spawned[MAX_PLAYERS + 1] = {true, ...}
new g_class[MAX_PLAYERS + 1]	// stored info from the "ScoreInfo" message

new mp_tournamentmode;
new amx_idle_time;
new amx_idle_min_players;
new amx_idle_ignore_immunity;

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
}

public checkPlayers() {
	if (get_pcvar_num(mp_tournamentmode)) return PLUGIN_HANDLED

	for (new i = 1; i <= MaxClients; i++) {
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
			new name[MAX_NAME_LENGTH]
			get_user_name(id, name, charsmax(name))
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
	set_task(0.75, "delayedSpawn",_, sid, sizeof(sid))	// Give the player time to drop to the floor when spawning
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
  g_class[id]=read_data(5);
}
public msgScoreInfo32() {
  new id=read_data(1);
  g_class[id]=read_data(6);
}
