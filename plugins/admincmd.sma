// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Admin Commands Plugin
//

#include <amxmodx>
#include <amxmisc>

// This is not a dynamic array because it would be bad for 24/7 map servers.
#define OLD_CONNECTION_QUEUE 10

new g_pause_con;
new g_pausable;
new bool:g_paused;
new bool:g_pause_allowed;

new g_cvar_pausable;
new g_cvar_rcon_password;
new g_cvar_timelimit;
new g_cvar_amx_tempban_maxtime;

// Old connection queue
new g_names[OLD_CONNECTION_QUEUE][MAX_NAME_LENGTH];
new g_steamids[OLD_CONNECTION_QUEUE][32];
new g_ips[OLD_CONNECTION_QUEUE][32];
new g_access[OLD_CONNECTION_QUEUE];
new g_tracker;
new g_size;

public Trie:g_tempbans;
new Trie:g_xvars_flags;

public plugin_init()
{
	register_plugin("Admin Commands", AMXX_VERSION_STR, "AMXX Dev Team");

	register_dictionary("admincmd.txt");
	register_dictionary("adminhelp.txt");
	register_dictionary("common.txt");

	register_concmd("amx_kick",         "cmdKick",      ADMIN_KICK,                 "AMX_KICK_SYNTAX",      .info_ml = true);
	register_concmd("amx_ban",          "cmdBan",       ADMIN_BAN|ADMIN_BAN_TEMP,   "AMX_BAN_SYNTAX",       .info_ml = true);
	register_concmd("amx_banip",        "cmdBanIP",     ADMIN_BAN|ADMIN_BAN_TEMP,   "AMX_BANIP_SYNTAX",     .info_ml = true);
	register_concmd("amx_addban",       "cmdAddBan",    ADMIN_BAN,                  "AMX_ADDBAN_SYNTAX",    .info_ml = true);
	register_concmd("amx_unban",        "cmdUnban",     ADMIN_BAN|ADMIN_BAN_TEMP,   "AMX_UNBAN_SYNTAX",     .info_ml = true);
	register_concmd("amx_slay",         "cmdSlay",      ADMIN_SLAY,                 "AMX_SLAY_SYNTAX",      .info_ml = true);
	register_concmd("amx_slap",         "cmdSlap",      ADMIN_SLAY,                 "AMX_SLAP_SYNTAX",      .info_ml = true);
	register_concmd("amx_leave",        "cmdLeave",     ADMIN_KICK,                 "AMX_LEAVE_SYNTAX",     .info_ml = true);
	register_concmd("amx_pause",        "cmdPause",     ADMIN_CVAR,                 "AMX_PAUSE_SYNTAX",     .info_ml = true);
	register_concmd("amx_who",          "cmdWho",       ADMIN_ADMIN,                "AMX_WHO_SYNTAX",       .info_ml = true);
	register_concmd("amx_cvar",         "cmdCvar",      ADMIN_CVAR,                 "AMX_CVAR_SYNTAX",      .info_ml = true);
	register_concmd("amx_xvar_float",   "cmdXvar",      ADMIN_CVAR,                 "AMX_XVAR_SYNTAX",      .info_ml = true);
	register_concmd("amx_xvar_int",     "cmdXvar",      ADMIN_CVAR,                 "AMX_XVAR_SYNTAX",      .info_ml = true);
	register_concmd("amx_plugins",      "cmdPlugins",   ADMIN_ADMIN,                "AMX_PLUGINS_SYNTAX",   .info_ml = true);
	register_concmd("amx_modules",      "cmdModules",   ADMIN_ADMIN,                "AMX_MODULES_SYNTAX",   .info_ml = true);
	register_concmd("amx_map",          "cmdMap",       ADMIN_MAP,                  "AMX_MAP_SYNTAX",       .info_ml = true);
	register_concmd("amx_extendmap",    "cmdExtendMap", ADMIN_MAP,                  "AMX_EXTENDMAP_SYNTAX", .info_ml = true);
	register_concmd("amx_cfg",          "cmdCfg",       ADMIN_CFG,                  "AMX_CFG_SYNTAX",       .info_ml = true);
	register_concmd("amx_nick",         "cmdNick",      ADMIN_SLAY,                 "AMX_NICK_SYNTAX",      .info_ml = true);
	register_concmd("amx_last",         "cmdLast",      ADMIN_BAN,                  "AMX_LAST_SYNTAX",      .info_ml = true);
	register_clcmd("amx_rcon",          "cmdRcon",      ADMIN_RCON,                 "AMX_RCON_SYNTAX",      .info_ml = true);
	register_clcmd("amx_showrcon",      "cmdShowRcon",  ADMIN_RCON,                 "AMX_RCON_SYNTAX",      .info_ml = true);
	register_clcmd("pauseAc",           "cmdLBack");

	g_cvar_rcon_password = get_cvar_pointer("rcon_password");
	g_cvar_pausable = get_cvar_pointer("pausable");
	g_cvar_timelimit = get_cvar_pointer("mp_timelimit");
	g_cvar_amx_tempban_maxtime = create_cvar("amx_tempban_maxtime", "4320", FCVAR_PROTECTED, "maximum ban time for temporary bans");
	g_tempbans = TrieCreate();

	new flags = get_pcvar_flags(g_cvar_rcon_password);

	if (!(flags & FCVAR_PROTECTED))
	{
		set_pcvar_flags(g_cvar_rcon_password, flags | FCVAR_PROTECTED);
	}
}

public plugin_end()
{
	TrieDestroy(g_tempbans);
	TrieDestroy(g_xvars_flags);
}

public client_disconnected(id)
{
	if (!is_user_bot(id))
	{
		InsertInfo(id);
	}
}

public cmdKick(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
	{
		return PLUGIN_HANDLED;
	}

	new player_arg[32];
	read_argv(1, player_arg, charsmax(player_arg));

	new playerid = cmd_target(id, player_arg, CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF);
	
	if (!playerid)
	{
		return PLUGIN_HANDLED;
	}

	new reason[32];
	read_argv(2, reason, charsmax(reason));
	remove_quotes(reason);

	log_amx("Kick: ^"%N^" kick ^"%N^" (reason ^"%s^")", id, playerid, reason);
	show_activity_key("ADMIN_KICK_1", "ADMIN_KICK_2", fmt("%n", id), fmt("%n", playerid));

	if (is_user_bot(playerid))
	{
		server_cmd("kick #%d", get_user_userid(playerid));
	}
	else
	{
		if (reason[0])
		{
			server_cmd("kick #%d ^"%s^"", get_user_userid(playerid), reason);
		}
		else
		{
			server_cmd("kick #%d", get_user_userid(playerid));
		}
	}
	
	console_print(id, "[AMXX] %l", "ADMIN_KICK_CON", playerid);	
	return PLUGIN_HANDLED;
}

public cmdUnban(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
	{
		return PLUGIN_HANDLED;
	}
	
	new arg[32], auth[32];
	read_argv(1, arg, charsmax(arg));
	get_user_authid(id, auth, charsmax(auth));

	if (!(get_user_flags(id) & (ADMIN_BAN | ADMIN_RCON)))
	{
		new stored_admin_auth[32];

		if (!TrieGetString(g_tempbans, arg, stored_admin_auth, charsmax(stored_admin_auth)) || !equal(stored_admin_auth, auth))
		{
			console_print(id, "[AMXX] %l", "ADMIN_MUST_TEMPUNBAN");
			return PLUGIN_HANDLED;
		}
	}
	
	if (contain(arg, ".") != -1)
	{
		server_cmd("removeip ^"%s^";writeip", arg);
		console_print(id, "[AMXX] %l", "IP_REMOVED", arg);
	}
	else
	{
		if (!isCommandArgSafe(arg))
		{
			console_print(id, "[AMXX] %l", "CL_NOT_FOUND");
			return PLUGIN_HANDLED;
		}

		server_cmd("removeid %s;writeid", arg);
		console_print(id, "[AMXX] %l", "AUTHID_REMOVED", arg);
	}

	show_activity_key("ADMIN_UNBAN_1", "ADMIN_UNBAN_2", fmt("%n", id), arg);
	log_amx("Cmd: ^"%N^" unban ^"%s^"", id, arg);

	return PLUGIN_HANDLED;
}

/* amx_addban is a special command now.
 * If a user with rcon uses it, it bans the user. No questions asked.
 * If a user without rcon but with ADMIN_BAN uses it, it will scan the old
 * connection queue, and if it finds the info for a player in it, it will
 * check their old access. If they have immunity, it will not ban.
 * If they do not have immunity, it will ban. If the user is not found,
 * it will refuse to ban the target.
 */
public cmdAddBan(id, level, cid)
{
	if (!cmd_access(id, level, cid, 3, true)) // check for ADMIN_BAN access
	{
		if (get_user_flags(id) & level) // Getting here means they didn't input enough args
		{
			return PLUGIN_HANDLED;
		}
		if (!cmd_access(id, ADMIN_RCON, cid, 3)) // If somehow they have ADMIN_RCON without ADMIN_BAN, continue
		{
			return PLUGIN_HANDLED;
		}
	}

	new arg[32], auth[32], reason[32];
	read_argv(1, arg, charsmax(arg));
	read_argv(3, reason, charsmax(reason));
	trim(arg);

	new minutes = clamp(read_argv_int(2), 0); // since negative values result in permanent bans
	
	if (!(get_user_flags(id) & ADMIN_RCON))
	{
		new bool:can_ban, bool:is_ip;
		static const szLimitedAccess[][] = { "STEAM_ID_PENDING", "STEAM_ID_LAN", "HLTV", "4294967295", "VALVE_ID_LAN", "VALVE_ID_PENDING" };

		// Limited access to this command
		for(new i; i < sizeof(szLimitedAccess); i++)
		{
			if(equali(arg, szLimitedAccess[i]))
			{
				console_print(id, "[AMXX] %l", "ADMIN_CANNOT_BAN", arg);
				return PLUGIN_HANDLED;
			}
		}
		
		if (contain(arg, ".") != -1)
		{
			is_ip = true;
		}
		
		new name[MAX_NAME_LENGTH], access;

		// Scan the disconnection queue
		if (is_ip)
		{
			for (new ip[16], i; i < g_size; i++)
			{
				GetInfo(i, name, charsmax(name), _, _, ip, charsmax(ip), access);
				
				if (equal(ip, arg))
				{
					if (access & ADMIN_IMMUNITY)
					{
						console_print(id, "[AMXX] %s : %l", ip, "CLIENT_IMM", name);
						return PLUGIN_HANDLED;
					}

					// User did not have immunity
					can_ban = true;
				}
			}
		}
		else
		{
			for (new auth[32], i; i < g_size; i++)
			{
				GetInfo(i, name, charsmax(name), auth, charsmax(auth), _, _, access);
				
				if (equal(auth, arg))
				{
					if (access & ADMIN_IMMUNITY)
					{
						console_print(id, "[AMXX] %s : %l", auth, "CLIENT_IMM", name);
						return PLUGIN_HANDLED;
					}

					// User did not have immunity
					can_ban = true;
				}
			}
		}
		
		if (!can_ban)
		{
			console_print(id, "[AMXX] %l", "ADMIN_BAN_ONLY_RECENT");
			return PLUGIN_HANDLED;
		}
	}
	
	// User has access to ban their target
	if (contain(arg, ".") != -1)
	{
		server_cmd("addip ^"%d^" ^"%s^";wait;writeip", minutes, arg);
		console_print(id, "[AMXX] %l", "ADMIN_IP_ADDED", arg);
	}
	else
	{
		if (!isCommandArgSafe(arg))
		{
			console_print(id, "[AMXX] %l", "CL_NOT_FOUND");
			return PLUGIN_HANDLED;
		}

		server_cmd("banid ^"%d^" %s;wait;writeid", minutes, arg);
		console_print(id, "[AMXX] %l", "ADMIN_AUTHID_ADDED", arg);
	}

	get_user_authid(id, auth, charsmax(auth));
	TrieSetString(g_tempbans, arg, auth);

	show_activity_key("ADMIN_ADDBAN_1", "ADMIN_ADDBAN_2", fmt("%n", id), arg);
	log_amx("Cmd: ^"%N^" ban ^"%s^" (minutes ^"%d^") (reason ^"%s^")", id, arg, minutes, reason);

	return PLUGIN_HANDLED;
}

public cmdBan(id, level, cid)
{
	if (!cmd_access(id, level, cid, 3))
	{
		return PLUGIN_HANDLED;
	}

	new player_arg[32], reason[32];
	read_argv(1, player_arg, charsmax(player_arg));
	read_argv(3, reason, charsmax(reason));

	new minutes = clamp(read_argv_int(2), 0); // since negative values result in permanent bans
	new playerid = cmd_target(id, player_arg, CMDTARGET_OBEY_IMMUNITY | CMDTARGET_NO_BOTS | CMDTARGET_ALLOW_SELF);
	
	if (!playerid)
	{
		return PLUGIN_HANDLED;
	}

	new const tempban_max_time = get_pcvar_num(g_cvar_amx_tempban_maxtime);

	if (!(get_user_flags(id) & (ADMIN_BAN | ADMIN_RCON)) && (minutes <= 0 || minutes > tempban_max_time))
	{
		console_print(id, "[AMXX] %l", "ADMIN_MUST_TEMPBAN", tempban_max_time);
		return PLUGIN_HANDLED;
	}

	new auth[32], auth2[32], name[MAX_NAME_LENGTH], name2[MAX_NAME_LENGTH];
	get_user_authid(playerid, auth2, charsmax(auth2));
	get_user_authid(id, auth, charsmax(auth));
	get_user_name(playerid, name2, charsmax(name2));
	get_user_name(id, name, charsmax(name));
	
	log_amx("Ban: ^"%N^" ban and kick ^"%N^" (minutes ^"%d^") (reason ^"%s^")", id, playerid, minutes, reason);
	TrieSetString(g_tempbans, auth2, auth); // store all bans in case a permanent ban would override a temporary one
	
	new temp[64], banned[16];

	if (minutes)
	{
		formatex(temp, charsmax(temp), "%L", playerid, "FOR_MIN", minutes);
	}
	else
	{
		formatex(temp, charsmax(temp), "%L", playerid, "PERM");
	}

	formatex(banned, charsmax(banned), "%L", playerid, "BANNED");

	if (reason[0])
	{
		server_cmd("kick #%d ^"%s (%s %s)^";wait;banid %d %s;wait;writeid", get_user_userid(playerid), reason, banned, temp, minutes, auth2);
	}
	else
	{
		server_cmd("kick #%d ^"%s %s^";wait;banid %d %s;wait;writeid", get_user_userid(playerid), banned, temp, minutes, auth2);
	}

	// display the message to all clients

	new message[256], players[MAX_PLAYERS], pnum, len;
	get_players_ex(players, pnum, GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV);

	for (new playerid, i; i < pnum; i++)
	{
		playerid = players[i];

		len = formatex(message, charsmax(message), "%L", playerid, "BAN");
		len += formatex(message[len], charsmax(message) - len, " %s ", name2);

		if (minutes)
		{
			len += formatex(message[len], charsmax(message) - len, "%L", playerid, "FOR_MIN", minutes);
		}
		else
		{
			len += formatex(message[len], charsmax(message) - len, "%L", playerid, "PERM");
		}

		if (strlen(reason) > 0)
		{
			formatex(message[len], charsmax(message) - len, " (%L: %s)", playerid, "REASON", reason);
		}

		show_activity_id(playerid, id, name, message);
	}
	
	console_print(id, "[AMXX] %l", "CLIENT_BANNED", name2);
	return PLUGIN_HANDLED;
}

public cmdBanIP(id, level, cid)
{
	if (!cmd_access(id, level, cid, 3))
	{
		return PLUGIN_HANDLED;
	}
	
	new player_arg[32], reason[32];
	read_argv(1, player_arg, charsmax(player_arg));
	read_argv(3, reason, charsmax(reason));
	
	new minutes = clamp(read_argv_int(2), 0); // since negative values result in permanent bans
	new playerid = cmd_target(id, player_arg, CMDTARGET_OBEY_IMMUNITY | CMDTARGET_NO_BOTS | CMDTARGET_ALLOW_SELF);
	
	if (!playerid)
	{
		return PLUGIN_HANDLED;
	}

	new const tempban_max_time = get_pcvar_num(g_cvar_amx_tempban_maxtime);

	if (!(get_user_flags(id) & (ADMIN_BAN | ADMIN_RCON)) && (minutes <= 0 || minutes > tempban_max_time))
	{
		console_print(id, "[AMXX] %l", "ADMIN_MUST_TEMPBAN", tempban_max_time);
		return PLUGIN_HANDLED;
	}
	
	new auth[32], auth2[32], name[MAX_NAME_LENGTH], name2[MAX_NAME_LENGTH];
	get_user_authid(playerid, auth2, charsmax(auth2));
	get_user_authid(id, auth, charsmax(auth));
	get_user_name(playerid, name2, charsmax(name2));
	get_user_name(id, name, charsmax(name));
	
	log_amx("Ban: ^"%N^" ban and kick ^"%N^" (minutes ^"%d^") (reason ^"%s^")", id, playerid, minutes, reason);
	TrieSetString(g_tempbans, auth2, auth);

	new temp[64], banned[16];

	if (minutes)
	{
		formatex(temp, charsmax(temp), "%L", playerid, "FOR_MIN", minutes);
	}
	else
	{
		formatex(temp, charsmax(temp), "%L", playerid, "PERM");
	}

	formatex(banned, charsmax(banned), "%L", playerid, "BANNED");

	new ip[16];
	get_user_ip(playerid, ip, charsmax(ip), 1);

	if (reason[0])
	{
		server_cmd("kick #%d ^"%s (%s %s)^";wait;addip ^"%d^" ^"%s^";wait;writeip", get_user_userid(playerid), reason, banned, temp, minutes, ip);
	}
	else
	{
		server_cmd("kick #%d ^"%s %s^";wait;addip ^"%d^" ^"%s^";wait;writeip", get_user_userid(playerid), banned, temp, minutes, ip);
	}

	// display the message to all clients

	new message[256], players[MAX_PLAYERS], pnum, len;
	get_players_ex(players, pnum, GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV);

	for (new playerid, i; i < pnum; i++)
	{
		playerid = players[i];

		len = formatex(message, charsmax(message), "%L", playerid, "BAN");
		len += formatex(message[len], charsmax(message) - len, " %s ", name2);

		if (minutes)
		{
			formatex(message[len], charsmax(message) - len, "%L", playerid, "FOR_MIN", minutes);
		}
		else
		{
			formatex(message[len], charsmax(message) - len, "%L", playerid, "PERM");
		}

		if (strlen(reason) > 0)
		{
			formatex(message[len], charsmax(message) - len, " (%L: %s)", playerid, "REASON", reason);
		}

		show_activity_id(playerid, id, name, message);
	}

	console_print(id, "[AMXX] %l", "CLIENT_BANNED", name2);
	return PLUGIN_HANDLED;
}

public cmdSlay(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
	{
		return PLUGIN_HANDLED;
	}
	
	new player_arg[32];	
	read_argv(1, player_arg, charsmax(player_arg));
	
	new playerid = cmd_target(id, player_arg, CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF | CMDTARGET_ONLY_ALIVE);
	
	if (!playerid)
	{
		return PLUGIN_HANDLED;
	}
	
	user_kill(playerid);

	log_amx("Cmd: ^"%N^" slay ^"%N^"", id, playerid);
	show_activity_key("ADMIN_SLAY_1", "ADMIN_SLAY_2", fmt("%n", id), fmt("%n", playerid));
	console_print(id, "[AMXX] %l", "CLIENT_SLAYED", playerid);

	return PLUGIN_HANDLED;
}

public cmdSlap(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
	{
		return PLUGIN_HANDLED;
	}

	new player_arg[32];
	read_argv(1, player_arg, charsmax(player_arg));

	new playerid = cmd_target(id, player_arg, CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF | CMDTARGET_ONLY_ALIVE);
	
	if (!playerid)
	{
		return PLUGIN_HANDLED;
	}

	new damage = clamp(read_argv_int(2), 0);
	user_slap(playerid, damage);
	
	log_amx("Cmd: ^"%N^" slap ^"%N^" (%d damage)", id, playerid, damage);
	show_activity_key("ADMIN_SLAP_1", "ADMIN_SLAP_2", fmt("%n", id), fmt("%n", playerid), damage);
	console_print(id, "[AMXX] %l", "CLIENT_SLAPPED", playerid, damage);
	
	return PLUGIN_HANDLED;
}

public cmdMap(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
	{
		return PLUGIN_HANDLED;
	}

	new map[32], map_len = read_argv(1, map, charsmax(map));
	
	if (!is_map_valid(map))
	{
		console_print(id, "[AMXX] %l", "MAP_NOT_FOUND");
		return PLUGIN_HANDLED;
	}

	show_activity_key("ADMIN_MAP_1", "ADMIN_MAP_2", fmt("%n", id), map);
	log_amx("Cmd: ^"%N^" change map to ^"%s^"", id, map);
	
	new mod[10];
	get_modname(mod, charsmax(mod));
	
	if (!equal(mod, "zp"))
	{
		message_begin(MSG_ALL, SVC_INTERMISSION);
		message_end();
	}
	
	set_task(2.0, "ChangeMap", 0, map, map_len + 1);
	return PLUGIN_HANDLED;
}

public cmdExtendMap(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
	{
		return PLUGIN_HANDLED;
	}
	
	new minutes = read_argv_int(1);
	
	if (minutes <= 0)
	{
		return PLUGIN_HANDLED;
	}
	
	static map[32];

	if(!map[0])
	{
		get_mapname(map, charsmax(map));
	}

	set_pcvar_num(g_cvar_timelimit, get_pcvar_num(g_cvar_timelimit) + minutes);

	show_activity_key("ADMIN_EXTEND_1", "ADMIN_EXTEND_2", fmt("%n", id), minutes);
	log_amx("ExtendMap: ^"%N^" extend map ^"%s^" for %d minutes", id, map, minutes);
	console_print(id, "%L", id, "MAP_EXTENDED", map, minutes);
	
	return PLUGIN_HANDLED;
}

public cmdCvar(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
	{
		return PLUGIN_HANDLED;
	}
	
	new cvar[32], value[64];
	read_argv(1, cvar, charsmax(cvar));
	read_argv(2, value, charsmax(value));
	
	new cvar_pointer;
	
	if (equal(cvar, "add") && (get_user_flags(id) & ADMIN_RCON))
	{
		if ((cvar_pointer = get_cvar_pointer(value))!=0)
		{
			new flags = get_pcvar_flags(cvar_pointer);
			
			if (!(flags & FCVAR_PROTECTED))
			{
				set_pcvar_flags(cvar_pointer, flags | FCVAR_PROTECTED);
			}
		}

		return PLUGIN_HANDLED;
	}
	
	trim(cvar);
	
	if ((cvar_pointer = get_cvar_pointer(cvar)) == 0)
	{
		console_print(id, "[AMXX] %l", "UNKNOWN_CVAR", cvar);
		return PLUGIN_HANDLED;
	}
	
	if (onlyRcon(cvar) && !(get_user_flags(id) & ADMIN_RCON))
	{
		// Exception for the new onlyRcon rules:
		// sv_password is allowed to be modified by ADMIN_PASSWORD
		if (!(equali(cvar,"sv_password") && (get_user_flags(id) & ADMIN_PASSWORD)))
		{
			console_print(id, "[AMXX] %l", "CVAR_NO_ACC");
			return PLUGIN_HANDLED;
		}
	}
	
	if (read_argc() < 3)
	{
		get_pcvar_string(cvar_pointer, value, charsmax(value));
		console_print(id, "[AMXX] %l", "CVAR_IS", cvar, value);
		return PLUGIN_HANDLED;
	}
	
	if (equali(cvar, "servercfgfile") || equali(cvar, "lservercfgfile"))
	{
		new pos = contain(value, ";");

		if (pos != -1)
		{
			value[pos] = '^0';
		}
	}

	log_amx("Cmd: ^"%N^" set cvar (name ^"%s^") (value ^"%s^")", id, cvar, value);
	set_pcvar_string(cvar_pointer, value);
	
	// display the message to all clients

	new cvar_value[64], players[MAX_PLAYERS], pnum;
	get_players_ex(players, pnum, GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV);

	for (new playerid, i; i < pnum; i++)
	{
		playerid = players[i];

		if (get_pcvar_flags(cvar_pointer) & FCVAR_PROTECTED || equali(cvar, "rcon_password"))
		{
			formatex(cvar_value, charsmax(cvar_value), "*** %L ***", playerid, "PROTECTED");
		}
		else
		{
			copy(cvar_value, charsmax(cvar_value), value);
		}

		show_activity_id(playerid, id, fmt("%n", id), "%L", playerid, "SET_CVAR_TO", "", cvar, cvar_value);
	}

	console_print(id, "[AMXX] %l", "CVAR_CHANGED", cvar, value);
	return PLUGIN_HANDLED;
}

public cmdXvar(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
	{
		return PLUGIN_HANDLED;
	}

	new command[15], xvar[32], value[32];
	read_argv(0, command, charsmax(command));
	read_argv(1, xvar, charsmax(xvar));
	trim(xvar);

	if (read_argc() > 2)
	{
		read_argv(2, value, charsmax(value));
		trim(value);

		if (equali(xvar, "add"))
		{
			if (get_user_flags(id) & ADMIN_RCON && xvar_exists(value))
			{
				if (!g_xvars_flags)
				{
					g_xvars_flags = TrieCreate();
				}

				TrieSetCell(g_xvars_flags, value, 1);
			}

			return PLUGIN_HANDLED;
		}
	}

	new bool:is_float = equali(command, "amx_xvar_float") != 0;
	new xvarid = get_xvar_id(xvar);

	if (xvarid == -1)
	{
		console_print(id, "[AMXX] %l", "UNKNOWN_XVAR", xvar);
		return PLUGIN_HANDLED;
	}

	new any:xvar_value;

	if (!value[0]) // get value
	{
		xvar_value = get_xvar_num(xvarid);

		if (is_float)
		{
			float_to_str(xvar_value, value, charsmax(value));
		}
		else
		{
			num_to_str(xvar_value, value, charsmax(value));
		}

		console_print(id, "[AMXX] %l", "XVAR_IS", xvar, value);
		return PLUGIN_HANDLED;
	}

	// set value
	if (g_xvars_flags && TrieKeyExists(g_xvars_flags, xvar) && ~get_user_flags(id) & ADMIN_RCON)
	{
		console_print(id, "[AMXX] %l", "XVAR_NO_ACC");
		return PLUGIN_HANDLED;
	}

	new endpos;

	if (is_float)
	{
		xvar_value = strtof(value, endpos);

		if (!endpos)
		{
			return PLUGIN_HANDLED;
		}
	}
	else
	{
		xvar_value = strtol(value, endpos);

		if (!endpos)
		{
			return PLUGIN_HANDLED;
		}
	}

	set_xvar_num(xvarid, xvar_value);

	// convert back value to string so admin can know value has been set correctly
	if (is_float)
	{
		float_to_str(xvar_value, value, charsmax(value));
	}
	else
	{
		num_to_str(xvar_value, value, charsmax(value));
	}

	log_amx("Cmd: ^"%N^" set xvar (name ^"%s^") (value ^"%s^")", id, xvar, value);
	
	// display the message to all clients
	new players[MAX_PLAYERS], pnum;
	get_players_ex(players, pnum, GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV);

	for (new playerid, i; i < pnum; i++)
	{
		playerid = players[i];
		show_activity_id(playerid, id, fmt("%n", id), "%L", playerid, "SET_XVAR_TO", "", xvar, value);
	}
	
	console_print(id, "[AMXX] %l", "XVAR_CHANGED", xvar, value);
	return PLUGIN_HANDLED;
}

public cmdPlugins(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}
		
	if (!id) // if server executes redirect this to "amxx plugins" for more in-depth output
	{
		server_cmd("amxx plugins");
		server_exec();
		return PLUGIN_HANDLED;
	}

	new plugins_num = get_pluginsnum();
	new temp[96], name[32], version[32], author[32], filename[32], status[32], start_plugin, end_plugin, plugins_running;
	
	if (read_argc() > 1)
	{
		start_plugin = read_argv_int(1) - 1; // zero-based
	}

	end_plugin = min(start_plugin + 10, plugins_num);
	
	console_print(id, "----- %l -----", "LOADED_PLUGINS");
	console_print(id, "%-18.17s %-11.10s %-17.16s %-16.15s %-9.8s", fmt("%l", "NAME"), fmt("%l", "VERSION"), fmt("%l", "AUTHOR"), fmt("%l", "FILE"), fmt("%l", "STATUS"));

	new i = start_plugin;

	while (i < end_plugin)
	{
		get_plugin(i++, filename, charsmax(filename), name, charsmax(name), version, charsmax(version), author, charsmax(author), status, charsmax(status));
		console_print(id, "%-18.17s %-11.10s %-17.16s %-16.15s %-9.8s", name, version, author, filename, status);
		
		if (status[0] == 'd' || status[0] == 'r') // "debug" or "running"
		{
			plugins_running++;
		}
	}

	console_print(id, "%l", "PLUGINS_RUN", end_plugin - start_plugin, plugins_running);
	console_print(id, "----- %l -----", "HELP_ENTRIES", start_plugin + 1, end_plugin, plugins_num);
	
	if (end_plugin < plugins_num)
	{
		formatex(temp, charsmax(temp), "----- %L -----", id, "HELP_USE_MORE", "amx_help", end_plugin + 1);
		replace_string(temp, charsmax(temp), "amx_help", "amx_plugins");
		console_print(id, temp);
	}
	else
	{
		formatex(temp, charsmax(temp), "----- %L -----", id, "HELP_USE_BEGIN", "amx_help");
		replace_string(temp, charsmax(temp), "amx_help", "amx_plugins");
		console_print(id, temp);
	}

	return PLUGIN_HANDLED;
}

public cmdModules(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}

	new modules_num = get_modulesnum();

	console_print(id, "%l:", "LOADED_MODULES");
	console_print(id, "%-23.22s %-11.10s %-20.19s %-11.10s", fmt("%l", "NAME"), fmt("%l", "VERSION"), fmt("%l", "AUTHOR"), fmt("%l", "STATUS"));
	
	for (new name[32], version[32], author[32], status[16], status_int, i; i < modules_num; i++)
	{
		get_module(i, name, charsmax(name), author, charsmax(author), version, charsmax(version), status_int);
		
		switch (status_int)
		{
			case module_loaded:
			{
				formatex(status, charsmax(status), "%L", id, "MODULE_RUNNING");
			}
			default:
			{
				formatex(status, charsmax(status), "%L", id, "MODULE_BAD_LOAD");
				formatex(name, charsmax(name), "%L", id, "MODULE_UNKNOWN");
				formatex(author, charsmax(author), "%L", id, "MODULE_UNKNOWN");
				formatex(version, charsmax(version), "%L", id, "MODULE_UNKNOWN");
			}
		}
		
		console_print(id, "%-23.22s %-11.10s %-20.19s %-11.10s", name, version, author, status);
	}

	console_print(id, "%l", "NUM_MODULES", modules_num);
	return PLUGIN_HANDLED;
}

public cmdCfg(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
	{
		return PLUGIN_HANDLED;
	}
	
	new file[128];
	read_argv(1, file, charsmax(file));
	
	if (!file_exists(file))
	{
		console_print(id, "[AMXX] %l", "FILE_NOT_FOUND", file);
		return PLUGIN_HANDLED;
	}
	
	log_amx("Cmd: ^"%N^" execute cfg file ^"%s^"", id, file);
	console_print(id, "[AMXX] %l", id, file);
	show_activity_key("ADMIN_CONF_1", "ADMIN_CONF_2", fmt("%n", id), file);

	server_cmd("exec ^"%s^"", file);
	return PLUGIN_HANDLED;
}

public cmdLBack()
{
	if (!g_pause_allowed)
	{
		return PLUGIN_CONTINUE;
	}

	set_pcvar_num(g_cvar_pausable, g_pausable);
	console_print(g_pause_con, "[AMXX] %l", g_paused ? "ADMIN_UNPAUSE_CON" : "ADMIN_PAUSE_CON");

	g_pause_allowed = false;
	g_paused = !g_paused;
	return PLUGIN_HANDLED;
}

public cmdPause(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}
	
	new playerid = id;
	
	if (g_cvar_pausable)
	{
		g_pausable = get_pcvar_num(g_cvar_pausable);
	}
	
	if (!playerid)
	{
		playerid = find_player_ex(FindPlayer_ExcludeBots);
	}
	
	if (!playerid)
	{ 
		console_print(id, "[AMXX] %l", "UNABLE_PAUSE");
		return PLUGIN_HANDLED;
	}

	g_pause_allowed = true;
	set_pcvar_num(g_cvar_pausable, 1);
	client_cmd(playerid, "pause;pauseAck");
	
	log_amx("Cmd: ^"%N^" %s server", id, g_paused ? "unpause" : "pause");
	console_print(id, "[AMXX] %l", g_paused ? "UNPAUSING" : "PAUSING");

	// display the message to all clients

	new players[MAX_PLAYERS], pnum;
	get_players_ex(players, pnum, GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV);

	for (new i; i < pnum; i++)
	{
		show_activity_id(players[i], id, fmt("%n", id), "%L server", i, g_paused ? "UNPAUSE" : "PAUSE");
	}

	g_pause_con = id;	
	return PLUGIN_HANDLED;
} 

public cmdShowRcon(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
	{
		return PLUGIN_HANDLED;
	}
		
	new password[64];	
	get_pcvar_string(g_cvar_rcon_password, password, charsmax(password));
	
	if (!password[0])
	{
		cmdRcon(id, level, cid);
	} 
	else 
	{
		new args[128];		
		read_args(args, charsmax(args));
		client_cmd(id, "rcon_password %s", password);
		client_cmd(id, "rcon %s", args);
	}
	
	return PLUGIN_HANDLED;
}

public cmdRcon(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
	{
		return PLUGIN_HANDLED;
	}
	
	new command[128];
	read_args(command, charsmax(command));

	log_amx("Cmd: ^"%N^" server console (cmdline ^"%s^")", id, command);
	console_print(id, "[AMXX] %l", "COM_SENT_SERVER", command);
	server_cmd("%s", command);

	return PLUGIN_HANDLED;
}

public cmdWho(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}

	new yes[16], no[16], players[MAX_PLAYERS], pnum, auth[32], name[MAX_NAME_LENGTH];
	formatex(yes, charsmax(yes), "%L", id, "YES");
	formatex(no, charsmax(no), "%L", id, "NO");
	
	get_players_ex(players, pnum, GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV);
	console_print(id, "^n%s:^n #  %-16.15s %-20s %-8s %-4.3s %-4.3s %s", fmt("%l", "CLIENTS_ON_SERVER"), "nick", "authid", "userid", fmt("%l", "IMMU"), fmt("%l", "RESERV"), fmt("%l", "ACCESS"));
	
	for (new flags[32], playerid, flags_int, i; i < pnum; i++)
	{
		playerid = players[i];
		flags_int = get_user_flags(playerid);

		get_user_authid(playerid, auth, charsmax(auth));
		get_user_name(playerid, name, charsmax(name));		
		get_flags(flags_int, flags, charsmax(flags));

		console_print(id, "%2d  %-16.15s %-20s %-8d %-6.5s %-6.5s %s", playerid, name, auth,
		get_user_userid(playerid), flags_int & ADMIN_IMMUNITY ? yes : no, flags_int & ADMIN_RESERVATION ? yes : no, flags);
	}
	
	console_print(id, "%l", "TOTAL_NUM", pnum);
	log_amx("Cmd: ^"%N^" ask for players list", id);
	return PLUGIN_HANDLED;
}

public cmdLeave(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
	{
		return PLUGIN_HANDLED;
	}
	
	new tags[4][32], tags_num;
	
	for (new arg_num = read_argc(), i = 1; i < 5; i++)
	{
		if (i < arg_num)
		{
			read_argv(i, tags[tags_num++], charsmax(tags[]));
		}
		else
		{
			tags[tags_num++][0] = 0;
		}
	}
	
	new name[MAX_NAME_LENGTH], players[MAX_PLAYERS], pnum, count;
	get_players_ex(players, pnum, GetPlayers_IncludeConnecting);
	
	for (new playerid, res, i; i < pnum; i++)
	{
		playerid = players[i];
		res = hasTag(name, tags, tags_num);
		get_user_name(playerid, name, charsmax(name));
		
		if (res != -1)
		{
			console_print(id, "[AMXX] %l", "SKIP_MATCH", name, tags[res]);
			continue;
		}
		
		if (get_user_flags(playerid) & ADMIN_IMMUNITY)
		{
			console_print(id, "[AMXX] %l", "SKIP_IMM", name);
			continue;
		}
		
		console_print(id, "[AMXX] %l", "KICK_PL", name);
		
		if (is_user_bot(playerid))
		{
			server_cmd("kick #%d", get_user_userid(playerid));
		}
		else
		{
			server_cmd("kick #%d ^"%L^"", get_user_userid(playerid), playerid, "YOU_DROPPED");
		}

		count++;
	}
	
	console_print(id, "[AMXX] %l", "KICKED_CLIENTS", count);
	log_amx("Kick: ^"%N^" leave some group (tag1 ^"%s^") (tag2 ^"%s^") (tag3 ^"%s^") (tag4 ^"%s^")", id, tags[0], tags[1], tags[2], tags[3]);
	show_activity_key("ADMIN_LEAVE_1", "ADMIN_LEAVE_2", fmt("%n", id), tags[0], tags[1], tags[2], tags[3]);

	return PLUGIN_HANDLED;
}

public cmdNick(id, level, cid)
{
	if (!cmd_access(id, level, cid, 3))
	{
		return PLUGIN_HANDLED;
	}

	new player_arg[32], nick[32], name2[MAX_NAME_LENGTH];
	read_argv(1, player_arg, charsmax(player_arg));
	read_argv(2, nick, charsmax(nick));

	new playerid = cmd_target(id, player_arg, CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF);
	
	if (!playerid)
	{
		return PLUGIN_HANDLED;
	}

	get_user_name(playerid, name2, charsmax(name2));
	set_user_info(playerid, "name", nick);

	log_amx("Cmd: ^"%N^" change nick to ^"%s^" ^"%N^"", id, nick, playerid);
	show_activity_key("ADMIN_NICK_1", "ADMIN_NICK_2", fmt("%n", id), name2, nick);
	console_print(id, "[AMXX] %l", "CHANGED_NICK", name2, nick);

	return PLUGIN_HANDLED;
}

public cmdLast(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}
	
	// This alignment is a bit weird (it should grow if the name is larger)
	// but otherwise for the more common shorter name, it'll wrap in server console
	// Steam client display is all skewed anyway because of the non fixed font.
	console_print(id, "%19s %20s %15s %s", "name", "authid", "ip", "access");
	
	for (new name[MAX_NAME_LENGTH], auth[32], flags[32], ip[16], access, i; i < g_size; i++)
	{
		GetInfo(i, name, charsmax(name), auth, charsmax(auth), ip, charsmax(ip), access);
		get_flags(access, flags, charsmax(flags));
		console_print(id, "%19s %20s %15s %s", name, auth, ip, flags);
	}
	
	console_print(id, "[AMXX] %l", "ADMIN_OLD_CONNS_SAVED", g_size);
	return PLUGIN_HANDLED;
}

InsertInfo(id)
{
	// Scan to see if this entry is the last entry in the list
	// If it is, then update the name and access
	// If it is not, then insert it again.

	if (g_size > 0)
	{
		new auth[34], ip[16], iLast;
		get_user_authid(id, auth, charsmax(auth));
		get_user_ip(id, ip, charsmax(ip), 1);

		if (g_size < sizeof(g_steamids))
		{
			iLast = g_size - 1;
		}
		else
		{
			iLast = g_tracker - 1;
			
			if (iLast < 0)
			{
				iLast = g_size - 1;
			}
		}
		
		if (equal(auth, g_steamids[iLast]) && equal(ip, g_ips[iLast])) // need to check ip too, or all the nosteams will while it doesn't work with their illegitimate server
		{
			get_user_name(id, g_names[iLast], charsmax(g_names[]));
			g_access[iLast] = get_user_flags(id);
			return;
		}
	}
	
	// Need to insert the entry
	
	new target;  // the slot to save the info at

	// Queue is not yet full
	if (g_size < sizeof(g_steamids))
	{
		target = g_size;
		++g_size;
	}
	else
	{
		target = g_tracker;
		++g_tracker;

		// If we reached the end of the array, then move to the front
		if (g_tracker == sizeof(g_steamids))
		{
			g_tracker = 0;
		}
	}
	
	get_user_authid(id, g_steamids[target], charsmax(g_steamids[]));
	get_user_name(id, g_names[target], charsmax(g_names[]));
	get_user_ip(id, g_ips[target], charsmax(g_ips[]), 1);
	g_access[target] = get_user_flags(id);
}

GetInfo(i, name[], namelen, auth[] = "", authlen = 0, ip[] = "", iplen = 0, &access = 0)
{
	if (i >= g_size)
	{
		abort(AMX_ERR_NATIVE, "GetInfo: Out of bounds (%d:%d)", i, g_size);
	}
	
	new target = (g_tracker + i) % sizeof(g_steamids);
	
	copy(name, namelen, g_names[target]);
	copy(auth, authlen, g_steamids[target]);
	copy(ip,   iplen,   g_ips[target]);
	access = g_access[target];
	
}

/**
 * ';' and '\n' are command delimiters. If a command arg contains these 2
 * it is not safe to be passed to server_cmd() as it may be trying to execute
 * a command.
 */
isCommandArgSafe(const arg[])
{
	return contain(arg, ";") == -1 && contain(arg, "\n") == -1;
}

public ChangeMap(map[])
{
	engine_changelevel(map);
}

bool:onlyRcon(const name[])
{
	new pointer = get_cvar_pointer(name);
	return pointer && get_pcvar_flags(pointer) & FCVAR_PROTECTED;
}

hasTag(name[], tags[4][32], tags_num)
{
	for (new i = 0; i < tags_num; i++)
	{
		if (contain(name, tags[i]) != -1)
		{
			return i;
		}
	}

	return -1;
}
