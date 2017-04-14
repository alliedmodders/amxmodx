// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Players Menu Plugin
//

#include <amxmodx>
#include <amxmisc>

/** skip autoloading since it's optional */
#define AMXMODX_NOAUTOLOAD
#include <cstrike>
#include <fakemeta>

new g_menuPosition[MAX_PLAYERS + 1];
new g_menuPlayers[MAX_PLAYERS + 1][MAX_PLAYERS];
new g_menuPlayersNum[MAX_PLAYERS + 1];
new g_menuOption[MAX_PLAYERS + 1];
new g_menuSettings[MAX_PLAYERS + 1];

new g_menuSelect[MAX_PLAYERS + 1][64];
new g_menuSelectNum[MAX_PLAYERS + 1];

#define MAX_CLCMDS 24

new g_clcmdName[MAX_CLCMDS][32];
new g_clcmdCmd[MAX_CLCMDS][64];
new g_clcmdMisc[MAX_CLCMDS][2];
new g_clcmdNum;

new g_coloredMenus;
new bool:g_cstrike = false;
new bool:g_fakemeta = false;

new Array:g_bantimes;
new Array:g_slapsettings;

new const g_CSTeamNames[3][] = {
	"TERRORIST",
	"CT",
	"SPECTATOR"
};
new const g_CSTeamNumbers[3][] = {
	"1",
	"2",
	"6"
};
new const g_CSTeamiNumbers[3] = {
	1,
	2,
	3
};

new g_CSPlayerCanSwitchFromSpec[MAX_PLAYERS + 1];
new g_transferingAdmin;

new allow_spectators, mp_limitteams;

new p_amx_tempban_maxtime;
new Trie:g_tempBans;

new g_silent[MAX_PLAYERS + 1];

public plugin_natives()
{
	set_module_filter("module_filter");
	set_native_filter("native_filter");
}

public plugin_init()
{
	register_plugin("Players Menu", AMXX_VERSION_STR, "AMXX Dev Team");
	register_dictionary("common.txt");
	register_dictionary("admincmd.txt");
	register_dictionary("plmenu.txt");

	register_clcmd("amx_kickmenu", "cmdKickMenu", ADMIN_KICK, "- displays kick menu");
	register_clcmd("amx_banmenu", "cmdBanMenu", ADMIN_BAN|ADMIN_BAN_TEMP, "- displays ban menu");
	register_clcmd("amx_slapmenu", "cmdSlapMenu", ADMIN_SLAY, "- displays slap/slay menu");
	register_clcmd("amx_teammenu", "cmdTeamMenu", ADMIN_LEVEL_A, "- displays team menu");
	register_clcmd("amx_clcmdmenu", "cmdClcmdMenu", ADMIN_LEVEL_A, "- displays client cmds menu");

	register_menucmd(register_menuid("Ban Menu"), 1023, "actionBanMenu");
	register_menucmd(register_menuid("Kick Menu"), 1023, "actionKickMenu");
	register_menucmd(register_menuid("Slap/Slay Menu"), 1023, "actionSlapMenu");
	register_menucmd(register_menuid("Team Menu"), 1023, "actionTeamMenu");
	register_menucmd(register_menuid("Client Cmds Menu"), 1023, "actionClcmdMenu");

	g_bantimes = ArrayCreate();
	// Load up the old default values
	ArrayPushCell(g_bantimes, 0);
	ArrayPushCell(g_bantimes, 5);
	ArrayPushCell(g_bantimes, 10);
	ArrayPushCell(g_bantimes, 15);
	ArrayPushCell(g_bantimes, 30);
	ArrayPushCell(g_bantimes, 45);
	ArrayPushCell(g_bantimes, 60);

	g_slapsettings = ArrayCreate();
	// Old default values
	ArrayPushCell(g_slapsettings, 0); // slap 0 damage
	ArrayPushCell(g_slapsettings, 1);
	ArrayPushCell(g_slapsettings, 5);
	ArrayPushCell(g_slapsettings, 0); // Last option is ignored - it is slay

	register_srvcmd("amx_plmenu_bantimes", "plmenu_setbantimes");
	register_srvcmd("amx_plmenu_slapdmg", "plmenu_setslapdmg");

	g_coloredMenus = colored_menus();

	new clcmds_ini_file[64];
	get_configsdir(clcmds_ini_file, charsmax(clcmds_ini_file));
	format(clcmds_ini_file, charsmax(clcmds_ini_file), "%s/clcmds.ini", clcmds_ini_file);
	load_settings(clcmds_ini_file);

	if (LibraryExists("cstrike", LibType_Library))
	{
		g_cstrike = true;
	}
	if (LibraryExists("fakemeta", LibType_Library))
	{
		g_fakemeta = true;
	}

	new modname[9];
	get_modname(modname, charsmax(modname));
	if (equal(modname, "cstrike") || equal(modname, "czero"))
	{
		register_event("TeamInfo", "Event_TeamInfo", "a", "2=TERRORIST", "2=CT");
		register_event("TextMsg", "Event_TextMsg", "b", "1=4", "2=#Only_1_Team_Change");
	}

	allow_spectators = get_cvar_pointer("allow_spectators");
	mp_limitteams = get_cvar_pointer("mp_limitteams");
}

public plugin_cfg()
{
	new x = get_xvar_id("g_tempBans");
	if (x)
	{
		g_tempBans = Trie:get_xvar_num(x);
	}
	new amx_tempban_maxtime[] = "amx_tempban_maxtime";
	p_amx_tempban_maxtime = get_cvar_pointer(amx_tempban_maxtime);
	if (!p_amx_tempban_maxtime)
	{
		p_amx_tempban_maxtime = register_cvar(amx_tempban_maxtime, "4320", FCVAR_PROTECTED);
		server_cmd("amx_cvar add %s", amx_tempban_maxtime);
	}
}

public plmenu_setbantimes()
{
	new buff[32];
	new args = read_argc();

	if (args <= 1)
	{
		server_print("usage: amx_plmenu_bantimes <time1> [time2] [time3] ...");
		server_print("   use time of 0 for permanent.");

		return;
	}

	ArrayClear(g_bantimes);

	for (new i = 1; i < args; i++)
	{
		read_argv(i, buff, charsmax(buff));

		ArrayPushCell(g_bantimes, str_to_num(buff));
	}
}

public plmenu_setslapdmg()
{
	new buff[32];
	new args = read_argc();

	if (args <= 1)
	{
		server_print("usage: amx_plmenu_slapdmg <dmg1> [dmg2] [dmg3] ...");
		server_print("   slay is automatically set for the last value.");

		return;
	}

	ArrayClear(g_slapsettings);

	for (new i = 1; i < args; i++)
	{
		read_argv(i, buff, charsmax(buff));

		ArrayPushCell(g_slapsettings, str_to_num(buff));
	}
	ArrayPushCell(g_slapsettings, 0); // compensate for slay
}

public module_filter(const module[])
{
	if (equali(module, "cstrike") || equali(module, "fakemeta"))
	{
		return PLUGIN_HANDLED;
	}

	return PLUGIN_CONTINUE;
}

public native_filter(const name[], index, trap)
{
	if (!trap)
	{
		return PLUGIN_HANDLED;
	}

	return PLUGIN_CONTINUE;
}

/* Ban menu */

public actionBanMenu(id, key)
{
	switch (key)
	{
		case 7:
		{
			/* BEGIN OF CHANGES BY MISTAGEE ADDED A FEW MORE OPTIONS */

			++g_menuOption[id];
			g_menuOption[id] %= ArraySize(g_bantimes);

			g_menuSettings[id] = ArrayGetCell(g_bantimes, g_menuOption[id]);

			displayBanMenu(id, g_menuPosition[id]);
		}
		case 8:
		{
			displayBanMenu(id, ++g_menuPosition[id]);
		}
		case 9:
		{
			displayBanMenu(id, --g_menuPosition[id]);
		}
		default:
		{
			new banTime = g_menuSettings[id];
			if (~get_user_flags(id) & (ADMIN_BAN | ADMIN_RCON) && (banTime <= 0 || banTime > get_pcvar_num(p_amx_tempban_maxtime)))
			{
				console_print(id, "%L", id, "NO_ACC_COM");
				displayBanMenu(id, g_menuPosition[id]);
				return PLUGIN_HANDLED;
			}
			new player = g_menuPlayers[id][g_menuPosition[id] * 7 + key];
			new name[MAX_NAME_LENGTH], name2[MAX_NAME_LENGTH], authid[32], authid2[32];

			get_user_name(player, name2, charsmax(name2));
			get_user_authid(id, authid, charsmax(authid));
			get_user_authid(player, authid2, charsmax(authid2));
			get_user_name(id, name, charsmax(name));

			new userid2 = get_user_userid(player);

			log_amx("Ban: ^"%s<%d><%s><>^" ban and kick ^"%s<%d><%s><>^" (minutes ^"%d^")", name, get_user_userid(id), authid, name2, userid2, authid2, banTime);

			if (!banTime) // permanent
			{
				for (new i = 1; i <= MaxClients; i++)
				{
					show_activity_id(i, id, name, "%L %s %L", i, "BAN", name2, i, "PERM");
				}
			}
			else
			{
				new tempTime[32];
				num_to_str(banTime, tempTime, charsmax(tempTime));
				for (new i = 1; i <= MaxClients; i++)
				{
					show_activity_id(i, id, name, "%L %s %L", i, "BAN", name2, i, "FOR_MIN", tempTime);
				}
			}
			/* ---------- check for Steam ID added by MistaGee -------------------- 
			IF AUTHID == 4294967295 OR VALVE_ID_LAN OR HLTV, BAN PER IP TO NOT BAN EVERYONE */

			if (equal("4294967295", authid2)
				|| equal("HLTV", authid2)
				|| equal("STEAM_ID_LAN", authid2)
				|| equali("VALVE_ID_LAN", authid2))
			{
				/* END OF MODIFICATIONS BY MISTAGEE */
				new ipa[32];
				get_user_ip(player, ipa, charsmax(ipa), 1);

				server_cmd("addip %d %s;writeip", banTime, ipa);
				if (g_tempBans)
				{
					TrieSetString(g_tempBans, ipa, authid);
				}
			}
			else
			{
				server_cmd("banid %d #%d kick;writeid", banTime, userid2);
				if (g_tempBans)
				{
					TrieSetString(g_tempBans, authid2, authid);
				}
			}

			server_exec();

			displayBanMenu(id, g_menuPosition[id]);
		}
	}

	return PLUGIN_HANDLED;
}

displayBanMenu(id, pos)
{
	if (pos < 0)
	{
		return;
	}

	get_players(g_menuPlayers[id], g_menuPlayersNum[id]);

	new menuBody[512];
	new b = 0;
	new i;
	new name[MAX_NAME_LENGTH];
	new start = pos * 7;

	if (start >= g_menuPlayersNum[id])
	{
		start = pos = g_menuPosition[id] = 0;
	}

	new len = formatex(menuBody, charsmax(menuBody), g_coloredMenus ? "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n", id, "BAN_MENU", pos + 1, (g_menuPlayersNum[id] / 7 + ((g_menuPlayersNum[id] % 7) ? 1 : 0)));
	new end = start + 7;
	new keys = MENU_KEY_0|MENU_KEY_8;

	if (end > g_menuPlayersNum[id])
	{
		end = g_menuPlayersNum[id];
	}

	for (new a = start; a < end; ++a)
	{
		i = g_menuPlayers[id][a];
		get_user_name(i, name, charsmax(name));

		if (is_user_bot(i) || (access(i, ADMIN_IMMUNITY) && i != id))
		{
			++b;

			if (g_coloredMenus)
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, "\d%d. %s^n\w", b, name);
			}
			else
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, "#. %s^n", name);
			}
		}
		else
		{
			keys |= (1<<b);

			if (is_user_admin(i))
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, g_coloredMenus ? "%d. %s \r*^n\w" : "%d. %s *^n", ++b, name);
			}
			else
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, "%d. %s^n", ++b, name);
			}
		}
	}

	if (g_menuSettings[id])
	{
		len += formatex(menuBody[len], charsmax(menuBody) - len, "^n8. %L^n", id, "BAN_FOR_MIN", g_menuSettings[id]);
	}
	else
	{
		len += formatex(menuBody[len], charsmax(menuBody) - len, "^n8. %L^n", id, "BAN_PERM");
	}

	if (end != g_menuPlayersNum[id])
	{
		formatex(menuBody[len], charsmax(menuBody) - len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT");
		keys |= MENU_KEY_9;
	}
	else
	{
		formatex(menuBody[len], charsmax(menuBody) - len, "^n0. %L", id, pos ? "BACK" : "EXIT");
	}

	show_menu(id, keys, menuBody, -1, "Ban Menu");
}

public cmdBanMenu(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}

	g_menuOption[id] = 0;

	if (ArraySize(g_bantimes) > 0)
	{
		g_menuSettings[id] = ArrayGetCell(g_bantimes, g_menuOption[id]);
	}
	else
	{
		// should never happen, but failsafe
		g_menuSettings[id] = 0;
	}
	displayBanMenu(id, g_menuPosition[id] = 0);

	return PLUGIN_HANDLED;
}

/* Slap/Slay */

public actionSlapMenu(id, key)
{
	switch (key)
	{
		case 7:
		{
			++g_menuOption[id];

			g_menuOption[id] %= ArraySize(g_slapsettings);

			g_menuSettings[id] = ArrayGetCell(g_slapsettings, g_menuOption[id]);

			displaySlapMenu(id, g_menuPosition[id]);
		}
		case 8:
		{
			displaySlapMenu(id, ++g_menuPosition[id]);
		}
		case 9:
		{
			displaySlapMenu(id, --g_menuPosition[id]);
		}
		default:
		{
			new player = g_menuPlayers[id][g_menuPosition[id] * 7 + key];
			new name2[MAX_NAME_LENGTH];

			get_user_name(player, name2, charsmax(name2));

			if (!is_user_alive(player))
			{
				client_print(id, print_chat, "%L", id, "CANT_PERF_DEAD", name2);
				displaySlapMenu(id, g_menuPosition[id]);
				return PLUGIN_HANDLED;
			}

			new authid[32], authid2[32], name[MAX_NAME_LENGTH];

			get_user_authid(id, authid, charsmax(authid));
			get_user_authid(player, authid2, charsmax(authid2));
			get_user_name(id, name, charsmax(name));

			new aSize = ArraySize(g_slapsettings);
			if (aSize > 1 && g_menuOption[id] < aSize -1)
			{
				log_amx("Cmd: ^"%s<%d><%s><>^" slap with %d damage ^"%s<%d><%s><>^"", name, get_user_userid(id), authid, g_menuSettings[id], name2, get_user_userid(player), authid2);

				show_activity_key("ADMIN_SLAP_1", "ADMIN_SLAP_2", name, name2, g_menuSettings[id]);

				user_slap(player, (get_user_health(player) > g_menuSettings[id]) ? g_menuSettings[id] : 0);
			}
			else // aSize == 1 or g_menuOption[id] == aSize - 1 // last option
			{
				log_amx("Cmd: ^"%s<%d><%s><>^" slay ^"%s<%d><%s><>^"", name, get_user_userid(id), authid, name2, get_user_userid(player), authid2);

				show_activity_key("ADMIN_SLAY_1", "ADMIN_SLAY_2", name, name2);

				user_kill(player);
			}

			displaySlapMenu(id, g_menuPosition[id]);
		}
	}

	return PLUGIN_HANDLED;
}

displaySlapMenu(id, pos)
{
	if (pos < 0)
	{
		return;
	}

	get_players(g_menuPlayers[id], g_menuPlayersNum[id]);

	new menuBody[512];
	new b = 0;
	new i;
	new name[MAX_NAME_LENGTH], team[4];
	new start = pos * 7;

	if (start >= g_menuPlayersNum[id])
	{
		start = pos = g_menuPosition[id] = 0;
	}

	new len = formatex(menuBody, charsmax(menuBody), g_coloredMenus ? "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n", id, "SLAP_SLAY_MENU", pos + 1, (g_menuPlayersNum[id] / 7 + ((g_menuPlayersNum[id] % 7) ? 1 : 0)));
	new end = start + 7;
	new keys = MENU_KEY_0|MENU_KEY_8;

	if (end > g_menuPlayersNum[id])
	{
		end = g_menuPlayersNum[id];
	}

	for (new a = start; a < end; ++a)
	{
		i = g_menuPlayers[id][a];
		get_user_name(i, name, charsmax(name));

		if (g_cstrike)
		{
			if (cs_get_user_team(i) == CS_TEAM_T)
			{
				copy(team, charsmax(team), "TE");
			}
			else if (cs_get_user_team(i) == CS_TEAM_CT)
			{
				copy(team, charsmax(team), "CT");
			}
			else
			{
				get_user_team(i, team, charsmax(team));
			}
		}
		else
		{
			get_user_team(i, team, charsmax(team));
		}

		if (!is_user_alive(i) || (access(i, ADMIN_IMMUNITY) && i != id))
		{
			++b;

			if (g_coloredMenus)
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, "\d%d. %s\R%s^n\w", b, name, team);
			}
			else
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, "#. %s   %s^n", name, team);
			}
		}
		else
		{
			keys |= (1<<b);

			if (is_user_admin(i))
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, g_coloredMenus ? "%d. %s \r*\y\R%s^n\w" : "%d. %s *   %s^n", ++b, name, team);
			}
			else
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, g_coloredMenus ? "%d. %s\y\R%s^n\w" : "%d. %s   %s^n", ++b, name, team);
			}
		}
	}

	if (g_menuOption[id] == ArraySize(g_slapsettings) - 1)
	{
		len += formatex(menuBody[len], charsmax(menuBody) - len, "^n8. %L^n", id, "SLAY");
	}
	else
	{
		len += formatex(menuBody[len], charsmax(menuBody) - len, "^n8. %L^n", id, "SLAP_WITH_DMG", g_menuSettings[id]);
	}

	if (end != g_menuPlayersNum[id])
	{
		formatex(menuBody[len], charsmax(menuBody) - len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT");
		keys |= MENU_KEY_9;
	}
	else
	{
		formatex(menuBody[len], charsmax(menuBody) - len, "^n0. %L", id, pos ? "BACK" : "EXIT");
	}

	show_menu(id, keys, menuBody, -1, "Slap/Slay Menu");
}

public cmdSlapMenu(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}

	g_menuOption[id] = 0;
	if (ArraySize(g_slapsettings) > 0)
	{
		g_menuSettings[id] = ArrayGetCell(g_slapsettings, g_menuOption[id]);
	}
	else
	{
		// should never happen, but failsafe
		g_menuSettings[id] = 0;
	}

	displaySlapMenu(id, g_menuPosition[id] = 0);

	return PLUGIN_HANDLED;
}

/* Kick */

public actionKickMenu(id, key)
{
	switch (key)
	{
		case 8:
		{
			displayKickMenu(id, ++g_menuPosition[id]);
		}
		case 9:
		{
			displayKickMenu(id, --g_menuPosition[id]);
		}
		default:
		{
			new player = g_menuPlayers[id][g_menuPosition[id] * 8 + key];
			new authid[32], authid2[32], name[MAX_NAME_LENGTH], name2[MAX_NAME_LENGTH];

			get_user_authid(id, authid, charsmax(authid));
			get_user_authid(player, authid2, charsmax(authid2));
			get_user_name(id, name, charsmax(name));
			get_user_name(player, name2, charsmax(name2));

			new userid2 = get_user_userid(player);

			log_amx("Kick: ^"%s<%d><%s><>^" kick ^"%s<%d><%s><>^"", name, get_user_userid(id), authid, name2, userid2, authid2);

			show_activity_key("ADMIN_KICK_1", "ADMIN_KICK_2", name, name2);

			server_cmd("kick #%d", userid2);
			server_exec();

			displayKickMenu(id, g_menuPosition[id]);
		}
	}

	return PLUGIN_HANDLED;
}

displayKickMenu(id, pos)
{
	if (pos < 0)
	{
		return;
	}

	get_players(g_menuPlayers[id], g_menuPlayersNum[id]);

	new menuBody[512];
	new b = 0;
	new i;
	new name[MAX_NAME_LENGTH];
	new start = pos * 8;

	if (start >= g_menuPlayersNum[id])
	{
		start = pos = g_menuPosition[id] = 0;
	}

	new len = formatex(menuBody, charsmax(menuBody), g_coloredMenus ? "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n", id, "KICK_MENU", pos + 1, (g_menuPlayersNum[id] / 8 + ((g_menuPlayersNum[id] % 8) ? 1 : 0)));
	new end = start + 8;
	new keys = MENU_KEY_0;

	if (end > g_menuPlayersNum[id])
	{
		end = g_menuPlayersNum[id];
	}

	for (new a = start; a < end; ++a)
	{
		i = g_menuPlayers[id][a];
		get_user_name(i, name, charsmax(name));

		if (access(i, ADMIN_IMMUNITY) && i != id)
		{
			++b;

			if (g_coloredMenus)
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, "\d%d. %s^n\w", b, name);
			}
			else
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, "#. %s^n", name);
			}
		}
		else
		{
			keys |= (1<<b);

			if (is_user_admin(i))
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, g_coloredMenus ? "%d. %s \r*^n\w" : "%d. %s *^n", ++b, name);
			}
			else
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, "%d. %s^n", ++b, name);
			}
		}
	}

	if (end != g_menuPlayersNum[id])
	{
		formatex(menuBody[len], charsmax(menuBody) - len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT");
		keys |= MENU_KEY_9;
	}
	else
	{
		formatex(menuBody[len], charsmax(menuBody) - len, "^n0. %L", id, pos ? "BACK" : "EXIT");
	}

	show_menu(id, keys, menuBody, -1, "Kick Menu");
}

public cmdKickMenu(id, level, cid)
{
	if (cmd_access(id, level, cid, 1))
	{
		displayKickMenu(id, g_menuPosition[id] = 0);
	}

	return PLUGIN_HANDLED;
}

/* Team menu */

public client_putinserver(id)
{
	g_CSPlayerCanSwitchFromSpec[id] = false;
	g_silent[id] = false;
}

public Event_TeamInfo()
{
	new id = read_data(1);
	if (is_user_connected(id))
	{
		g_CSPlayerCanSwitchFromSpec[id] = true;
	}
}

public Event_TextMsg(id) // #Only_1_Team_Change
{
	if (g_transferingAdmin && is_user_connected(id) && (id == g_transferingAdmin || is_user_connected(g_transferingAdmin)))
	{
		new name[MAX_NAME_LENGTH];
		get_user_name(id, name, charsmax(name));
		client_print(g_transferingAdmin, print_chat, "%L", g_transferingAdmin, "CANT_PERF_PLAYER", name);
	}
}

public actionTeamMenu(id, key)
{
	switch (key)
	{
		case 6: 
		{
			g_silent[id] = !g_silent[id];
			displayTeamMenu(id, g_menuPosition[id]);
		}
		case 7:
		{
			g_menuOption[id] = (g_menuOption[id] + 1) % 3;
			displayTeamMenu(id, g_menuPosition[id]);
		}
		case 8:
		{
			displayTeamMenu(id, ++g_menuPosition[id]);
		}
		case 9:
		{
			displayTeamMenu(id, --g_menuPosition[id]);
		}
		default:
		{
			new player = g_menuPlayers[id][g_menuPosition[id] * 6 + key];
			if (!is_user_connected(player)) // dunno why this check hasn't be implemented in the past
			{
				displayTeamMenu(id, g_menuPosition[id]);
				return PLUGIN_HANDLED;
			}

			g_transferingAdmin = id;

			new authid[32], authid2[32], name[MAX_NAME_LENGTH], name2[MAX_NAME_LENGTH];

			get_user_name(player, name2, charsmax(name2));
			get_user_authid(id, authid, charsmax(authid));
			get_user_authid(player, authid2, charsmax(authid2));
			get_user_name(id, name, charsmax(name));

			// This modulo math just aligns the option to the CsTeams-corresponding number
			new destTeamSlot = (g_menuOption[id] % 3);

			log_amx("Cmd: ^"%s<%d><%s><>^" transfer ^"%s<%d><%s><>^" (team ^"%s^")", name, get_user_userid(id), authid, name2, get_user_userid(player), authid2, g_CSTeamNames[destTeamSlot]);

			show_activity_key("ADMIN_TRANSF_1", "ADMIN_TRANSF_2", name, name2, g_CSTeamNames[destTeamSlot]);

			if (destTeamSlot == 2)
			{
				if (g_fakemeta)
				{
					if (get_ent_data(player, "CBasePlayer", "m_iMenu") == CS_Menu_ChooseAppearance)
					{
						// works for both vgui and old style menus, and send menuselect could close other menus (and since get_user_menu fails to return VGUI and old style classes menus...)
						engclient_cmd(player, "joinclass", "6");
					}
				}
				else // force
				{
					engclient_cmd(player, "joinclass", "6");
				}
			}

			if (g_CSPlayerCanSwitchFromSpec[player] && g_cstrike && (CS_TEAM_T <= cs_get_user_team(player) <= CS_TEAM_CT))
			{
				if (is_user_alive(player) && (!g_silent[id] || destTeamSlot == 2))
				{
					new deaths = cs_get_user_deaths(player);
					user_kill(player, 1);
					cs_set_user_deaths(player, deaths);
				}

				cs_set_user_team(player, destTeamSlot + 1);

			}
			else
			{
				if (is_user_alive(player) && (!g_silent[id] || destTeamSlot == 2))
				{
					user_kill(player, 1);
				}
				if (g_fakemeta)
				{
					set_ent_data(player, "CBasePlayer", "m_bTeamChanged", true);
				}
				new limit_setting;
				if (mp_limitteams)
				{
					limit_setting = get_pcvar_num(mp_limitteams);

					set_pcvar_num(mp_limitteams, 0);
				}

				if (destTeamSlot == 2)
				{
					new Float:allow_spectators_setting;
					if (allow_spectators)
					{
						allow_spectators_setting = get_pcvar_float(allow_spectators);
						if (allow_spectators_setting != 1.0)
						{
							set_pcvar_float(allow_spectators, 1.0);
						}
					}
					engclient_cmd(player, "jointeam", g_CSTeamNumbers[destTeamSlot]);
					if (allow_spectators && allow_spectators_setting != 1.0)
					{
						set_pcvar_float(allow_spectators, allow_spectators_setting);
					}
				}
				else
				{
					engclient_cmd(player, "jointeam", g_CSTeamNumbers[destTeamSlot]);
					engclient_cmd(player, "joinclass", "1");
				}
				if (mp_limitteams && limit_setting != 0)
				{
					set_pcvar_num(mp_limitteams, limit_setting);
				}
			}
			if (g_cstrike)
			{
				cs_reset_user_model(player);
			}
			if (g_fakemeta)
			{
				set_ent_data(player, "CBasePlayer", "m_bTeamChanged", true);
			}

			g_transferingAdmin = 0;
			displayTeamMenu(id, g_menuPosition[id]);
		}
	}

	return PLUGIN_HANDLED;
}

displayTeamMenu(id, pos)
{
	if (pos < 0)
	{
		return;
	}

	get_players(g_menuPlayers[id], g_menuPlayersNum[id]);

	new menuBody[512];
	new b = 0;
	new i, iteam;
	new name[MAX_NAME_LENGTH], team[4];
	new start = pos * 6;

	if (start >= g_menuPlayersNum[id])
	{
		start = pos = g_menuPosition[id] = 0;
	}

	new len = formatex(menuBody, charsmax(menuBody), g_coloredMenus ? "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n", id, "TEAM_MENU", pos + 1, (g_menuPlayersNum[id] / 6 + ((g_menuPlayersNum[id] % 6) ? 1 : 0)));
	new end = start + 6;
	new keys = MENU_KEY_0|MENU_KEY_7|MENU_KEY_8;

	if (end > g_menuPlayersNum[id])
	{
		end = g_menuPlayersNum[id];
	}

	for (new a = start; a < end; ++a)
	{
		i = g_menuPlayers[id][a];
		get_user_name(i, name, charsmax(name));

		if (g_cstrike)
		{
			iteam = _:cs_get_user_team(i);

			if (iteam == 1)
			{
				copy(team, charsmax(team), "TE");
			}
			else if (iteam == 2)
			{
				copy(team, charsmax(team), "CT");
			}
			else if (iteam == 3)
			{
				copy(team, charsmax(team), "SPE");
				// iteam = 6; // oO WTF is this ?? fixed g_CSTeamiNumbers.
			}
			else
			{
				iteam = get_user_team(i, team, charsmax(team));
			}
		}
		else
		{
			iteam = get_user_team(i, team, charsmax(team));
		}
		if (!iteam)
		{
			iteam = 3; // fix get_user_team returning 0 on spectators
		}

		if ((iteam == g_CSTeamiNumbers[g_menuOption[id] % 3]) || (access(i, ADMIN_IMMUNITY) && i != id))
		{
			++b;

			if (g_coloredMenus)
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, "\d%d. %s\R%s^n\w", b, name, team);
			}
			else
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, "#. %s   %s^n", name, team);
			}
		}
		else
		{
			keys |= (1<<b);

			if (is_user_admin(i))
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, g_coloredMenus ? "%d. %s \r*\y\R%s^n\w" : "%d. %s *   %s^n", ++b, name, team);
			}
			else
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, g_coloredMenus ? "%d. %s\y\R%s^n\w" : "%d. %s   %s^n", ++b, name, team);
			}
		}
	}

	len += formatex(menuBody[len], charsmax(menuBody) - len, "^n7. %L: %L", id, "TRANSF_SILENT", id, g_silent[id] ? "YES" : "NO");
	len += formatex(menuBody[len], charsmax(menuBody) - len, "^n8. %L^n", id, "TRANSF_TO", g_CSTeamNames[g_menuOption[id] % 3]);

	if (end != g_menuPlayersNum[id])
	{
		formatex(menuBody[len], charsmax(menuBody) - len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT");
		keys |= MENU_KEY_9;
	}
	else
	{
		formatex(menuBody[len], charsmax(menuBody) - len, "^n0. %L", id, pos ? "BACK" : "EXIT");
	}

	show_menu(id, keys, menuBody, -1, "Team Menu");
}

public cmdTeamMenu(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}

	g_menuOption[id] = 0;

	displayTeamMenu(id, g_menuPosition[id] = 0);

	return PLUGIN_HANDLED;
}

/* Client cmds menu */

public actionClcmdMenu(id, key)
{
	switch (key)
	{
		case 7:
		{
			++g_menuOption[id];
			g_menuOption[id] %= g_menuSelectNum[id];
			displayClcmdMenu(id, g_menuPosition[id]);
		}
		case 8:
		{
			displayClcmdMenu(id, ++g_menuPosition[id]);
		}
		case 9:
		{
			displayClcmdMenu(id, --g_menuPosition[id]);
		}
		default:
		{
			new player = g_menuPlayers[id][g_menuPosition[id] * 7 + key];
			new flags = g_clcmdMisc[g_menuSelect[id][g_menuOption[id]]][1];

			if (is_user_connected(player))
			{
				new command[512], authid[32], userid[32];

				copy(command, charsmax(command), g_clcmdCmd[g_menuSelect[id][g_menuOption[id]]]);
				get_user_authid(player, authid, charsmax(authid));
				num_to_str(get_user_userid(player), userid, charsmax(userid));

				replace(command, charsmax(command), "%userid%", userid);
				replace(command, charsmax(command), "%authid%", authid);

				if (flags & 1)
				{
					server_cmd("%s", command);
					server_exec();
				}
				else if (flags & 2)
				{
					client_cmd(id, "%s", command);
				}
				else if (flags & 4)
				{
					client_cmd(player, "%s", command);
				}
			}

			if (flags & 8)
			{
				displayClcmdMenu(id, g_menuPosition[id]);
			}
		}
	}

	return PLUGIN_HANDLED;
}

displayClcmdMenu(id, pos)
{
	if (pos < 0)
	{
		return;
	}

	get_players(g_menuPlayers[id], g_menuPlayersNum[id]);

	new menuBody[512];
	new b = 0;
	new i;
	new name[MAX_NAME_LENGTH];
	new start = pos * 7;

	if (start >= g_menuPlayersNum[id])
	{
		start = pos = g_menuPosition[id] = 0;
	}

	new len = formatex(menuBody, charsmax(menuBody), g_coloredMenus ? "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n", id, "CL_CMD_MENU", pos + 1, (g_menuPlayersNum[id] / 7 + ((g_menuPlayersNum[id] % 7) ? 1 : 0)));
	new end = start + 7;
	new keys = MENU_KEY_0|MENU_KEY_8;

	if (end > g_menuPlayersNum[id])
	{
		end = g_menuPlayersNum[id];
	}

	for (new a = start; a < end; ++a)
	{
		i = g_menuPlayers[id][a];
		get_user_name(i, name, charsmax(name));

		if (!g_menuSelectNum[id] || (access(i, ADMIN_IMMUNITY) && i != id))
		{
			++b;

			if (g_coloredMenus)
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, "\d%d. %s^n\w", b, name);
			}
			else
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, "#. %s^n", name);
			}
		}
		else
		{
			keys |= (1<<b);

			if (is_user_admin(i))
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, g_coloredMenus ? "%d. %s \r*^n\w" : "%d. %s *^n", ++b, name);
			}
			else
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, "%d. %s^n", ++b, name);
			}
		}
	}

	if (g_menuSelectNum[id])
	{
		len += formatex(menuBody[len], charsmax(menuBody) - len, "^n8. %s^n", g_clcmdName[g_menuSelect[id][g_menuOption[id]]]);
	}
	else
	{
		len += formatex(menuBody[len], charsmax(menuBody) - len, "^n8. %L^n", id, "NO_CMDS");
	}

	if (end != g_menuPlayersNum[id])
	{
		formatex(menuBody[len], charsmax(menuBody) - len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT");
		keys |= MENU_KEY_9;
	}
	else
	{
		formatex(menuBody[len], charsmax(menuBody) - len, "^n0. %L", id, pos ? "BACK" : "EXIT");
	}

	show_menu(id, keys, menuBody, -1, "Client Cmds Menu");
}

public cmdClcmdMenu(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}

	g_menuSelectNum[id] = 0;

	for (new a = 0; a < g_clcmdNum; ++a)
	{
		if (access(id, g_clcmdMisc[a][0]))
		{
			g_menuSelect[id][g_menuSelectNum[id]++] = a;
		}
	}

	g_menuOption[id] = 0;

	displayClcmdMenu(id, g_menuPosition[id] = 0);

	return PLUGIN_HANDLED;
}

load_settings(szFilename[])
{
	if (!file_exists(szFilename))
	{
		return 0;
	}

	new text[256], szFlags[32], szAccess[32];
	new a, pos = 0;

	while (g_clcmdNum < MAX_CLCMDS && read_file(szFilename, pos++, text, charsmax(text), a))
	{
		if (text[0] == ';')
		{
			continue;
		}

		if (parse(text, g_clcmdName[g_clcmdNum], charsmax(g_clcmdName[]), g_clcmdCmd[g_clcmdNum], charsmax(g_clcmdCmd[]), szFlags, charsmax(szFlags), szAccess, charsmax(szAccess)) > 3)
		{
			while (replace(g_clcmdCmd[g_clcmdNum], charsmax(g_clcmdCmd[]), "\'", "^""))
			{
				// do nothing
			}

			g_clcmdMisc[g_clcmdNum][1] = read_flags(szFlags);
			g_clcmdMisc[g_clcmdNum][0] = read_flags(szAccess);
			g_clcmdNum++;
		}
	}

	return 1;
}

public plugin_end()
{
	ArrayDestroy(g_bantimes);
	ArrayDestroy(g_slapsettings);
}
