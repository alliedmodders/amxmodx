// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Maps Menu Plugin
//

#include <amxmodx>
#include <amxmisc>

new Array:g_mapName;
new g_mapNums
new g_menuPosition[MAX_PLAYERS + 1]

new g_voteCount[5]

new g_voteSelected[MAX_PLAYERS + 1][4]
new g_voteSelectedNum[MAX_PLAYERS + 1]

new g_coloredMenus

new g_choosed

public plugin_init()
{
	register_plugin("Maps Menu", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("mapsmenu.txt")
	register_dictionary("common.txt")
	register_clcmd("amx_mapmenu", "cmdMapsMenu", ADMIN_MAP, "- displays changelevel menu")
	register_clcmd("amx_votemapmenu", "cmdVoteMapMenu", ADMIN_VOTE, "- displays votemap menu")

	register_menucmd(register_menuid("Changelevel Menu"), 1023, "actionMapsMenu")
	register_menucmd(register_menuid("Which map do you want?"), 527, "voteCount")
	register_menucmd(register_menuid("Change map to"), 527, "voteCount")
	register_menucmd(register_menuid("Votemap Menu"), 1023, "actionVoteMapMenu")
	register_menucmd(register_menuid("The winner: "), 3, "actionResult")

	g_mapName=ArrayCreate(32);
	
	new maps_ini_file[64];
	get_configsdir(maps_ini_file, charsmax(maps_ini_file));
	format(maps_ini_file, charsmax(maps_ini_file), "%s/maps.ini", maps_ini_file);

	if (!file_exists(maps_ini_file))
		get_cvar_string("mapcyclefile", maps_ini_file, charsmax(maps_ini_file));
		
	if (!file_exists(maps_ini_file))
		format(maps_ini_file, charsmax(maps_ini_file), "mapcycle.txt")
	
	load_settings(maps_ini_file)

	g_coloredMenus = colored_menus()
}

public autoRefuse()
{
	log_amx("Vote: %L", "en", "RESULT_REF")
	client_print(0, print_chat, "%L", LANG_PLAYER, "RESULT_REF")
}

public actionResult(id, key)
{
	remove_task(4545454)
	
	switch (key)
	{
		case 0:
		{
			new _modName[10]
			get_modname(_modName, charsmax(_modName))
			
			if (!equal(_modName, "zp"))
			{
				message_begin(MSG_ALL, SVC_INTERMISSION)
				message_end()
			}

			new tempMap[32];
			ArrayGetString(g_mapName, g_choosed, tempMap, charsmax(tempMap));
			
			set_task(2.0, "delayedChange", 0, tempMap, strlen(tempMap) + 1)
			log_amx("Vote: %L", "en", "RESULT_ACC")
			client_print(0, print_chat, "%L", LANG_PLAYER, "RESULT_ACC")
		}
		case 1: autoRefuse()
	}
	
	return PLUGIN_HANDLED
}

public checkVotes(id)
{
	id -= 34567
	new num, ppl[MAX_PLAYERS], a = 0
	
	get_players(ppl, num, "c")
	if (num == 0) num = 1
	g_choosed = -1
	
	for (new i = 0; i < g_voteSelectedNum[id]; ++i)
		if (g_voteCount[a] < g_voteCount[i])
			a = i

	new votesNum = g_voteCount[0] + g_voteCount[1] + g_voteCount[2] + g_voteCount[3] + g_voteCount[4]
	new iRatio = votesNum ? floatround(get_cvar_float("amx_votemap_ratio") * float(votesNum), floatround_ceil) : 1
	new iResult = g_voteCount[a]

	if (iResult >= iRatio)
	{
		g_choosed = g_voteSelected[id][a]
		new tempMap[32];
		ArrayGetString(g_mapName, g_choosed, tempMap, charsmax(tempMap));
		client_print(0, print_chat, "%L %s", LANG_PLAYER, "VOTE_SUCCESS", tempMap);
		log_amx("Vote: %L %s", "en", "VOTE_SUCCESS", tempMap);
	}
	
	if (g_choosed != -1)
	{
		if (is_user_connected(id))
		{
			new menuBody[512]
			new tempMap[32];
			ArrayGetString(g_mapName, g_choosed, tempMap, charsmax(tempMap));
			new len = format(menuBody, charsmax(menuBody), g_coloredMenus ? "\y%L: \w%s^n^n" : "%L: %s^n^n", id, "THE_WINNER", tempMap)
			
			len += format(menuBody[len], charsmax(menuBody) - len, g_coloredMenus ? "\y%L^n\w" : "%L^n", id, "WANT_CONT")
			format(menuBody[len], charsmax(menuBody) - len, "^n1. %L^n2. %L", id, "YES", id, "NO")

			show_menu(id, 0x03, menuBody, 10, "The winner: ")
			set_task(10.0, "autoRefuse", 4545454)
		} else {
			new _modName[10]
			get_modname(_modName, charsmax(_modName))
			
			if (!equal(_modName, "zp"))
			{
				message_begin(MSG_ALL, SVC_INTERMISSION)
				message_end()
			}
			new tempMap[32];
			ArrayGetString(g_mapName, g_choosed, tempMap, charsmax(tempMap));
			set_task(2.0, "delayedChange", 0, tempMap, strlen(tempMap) + 1)
		}
	} else {
		client_print(0, print_chat, "%L", LANG_PLAYER, "VOTE_FAILED")
		log_amx("Vote: %L", "en", "VOTE_FAILED")
	}
	
	remove_task(34567 + id)
}

public voteCount(id, key)
{
	if (key > 3)
	{
		client_print(0, print_chat, "%L", LANG_PLAYER, "VOT_CANC")
		remove_task(34567 + id)
		set_cvar_float("amx_last_voting", get_gametime())
		log_amx("Vote: Cancel vote session")
		
		return PLUGIN_HANDLED
	}
	
	if (get_cvar_float("amx_vote_answers"))
	{
		new name[MAX_NAME_LENGTH]
		
		get_user_name(id, name, charsmax(name))
		client_print(0, print_chat, "%L", LANG_PLAYER, "X_VOTED_FOR", name, key + 1)
	}
	
	++g_voteCount[key]
	
	return PLUGIN_HANDLED
}

isMapSelected(id, pos)
{
	for (new a = 0; a < g_voteSelectedNum[id]; ++a)
		if (g_voteSelected[id][a] == pos)
			return 1
	return 0
}

displayVoteMapsMenu(id, pos)
{
	if (pos < 0)
		return

	new menuBody[512], b = 0, start = pos * 7

	if (start >= g_mapNums)
		start = pos = g_menuPosition[id] = 0

	new len = format(menuBody, charsmax(menuBody), g_coloredMenus ? "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n", id, "VOTEMAP_MENU", pos + 1, (g_mapNums / 7 + ((g_mapNums % 7) ? 1 : 0)))
	new end = start + 7, keys = MENU_KEY_0

	if (end > g_mapNums)
		end = g_mapNums

	new tempMap[32];
	for (new a = start; a < end; ++a)
	{
		ArrayGetString(g_mapName, a, tempMap, charsmax(tempMap));
		if (g_voteSelectedNum[id] == 4 || isMapSelected(id, pos * 7 + b))
		{
			++b
			if (g_coloredMenus)
				len += format(menuBody[len], charsmax(menuBody) - len, "\d%d. %s^n\w", b, tempMap)
			else
				len += format(menuBody[len], charsmax(menuBody) - len, "#. %s^n", tempMap)
		} else {
			keys |= (1<<b)
			len += format(menuBody[len], charsmax(menuBody) - len, "%d. %s^n", ++b, tempMap)
		}
	}

	if (g_voteSelectedNum[id])
	{
		keys |= MENU_KEY_8
		len += format(menuBody[len], charsmax(menuBody) - len, "^n8. %L^n", id, "START_VOT")
	}
	else
		len += format(menuBody[len], charsmax(menuBody) - len, g_coloredMenus ? "^n\d8. %L^n\w" : "^n#. %L^n", id, "START_VOT")

	if (end != g_mapNums)
	{
		len += format(menuBody[len], charsmax(menuBody) - len, "^n9. %L...^n0. %L^n", id, "MORE", id, pos ? "BACK" : "EXIT")
		keys |= MENU_KEY_9
	}
	else
		len += format(menuBody[len], charsmax(menuBody) - len, "^n0. %L^n", id, pos ? "BACK" : "EXIT")

	if (g_voteSelectedNum[id])
		len += format(menuBody[len], charsmax(menuBody) - len, g_coloredMenus ? "^n\y%L:^n\w" : "^n%L:^n", id, "SEL_MAPS")
	else
		len += format(menuBody[len], charsmax(menuBody) - len, "^n^n")

	for (new c = 0; c < 4; c++)
	{
		if (c < g_voteSelectedNum[id])
		{
			ArrayGetString(g_mapName, g_voteSelected[id][c], tempMap, charsmax(tempMap));
			len += format(menuBody[len], charsmax(menuBody) - len, "%s^n", tempMap)
		}
		else
			len += format(menuBody[len], charsmax(menuBody) - len, "^n")
	}

	new menuName[64]
	format(menuName, charsmax(menuName), "%L", "en", "VOTEMAP_MENU")

	show_menu(id, keys, menuBody, -1, menuName)
}

public cmdVoteMapMenu(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
		return PLUGIN_HANDLED

	if (get_cvar_float("amx_last_voting") > get_gametime())
	{
		client_print(id, print_chat, "%L", id, "ALREADY_VOT")
		return PLUGIN_HANDLED
	}

	g_voteSelectedNum[id] = 0

	if (g_mapNums)
	{
		displayVoteMapsMenu(id, g_menuPosition[id] = 0)
	} else {
		console_print(id, "%L", id, "NO_MAPS_MENU")
		client_print(id, print_chat, "%L", id, "NO_MAPS_MENU")
	}

	return PLUGIN_HANDLED
}

public cmdMapsMenu(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
		return PLUGIN_HANDLED

	if (g_mapNums)
	{
		displayMapsMenu(id, g_menuPosition[id] = 0)
	} else {
		console_print(id, "%L", id, "NO_MAPS_MENU")
		client_print(id, print_chat, "%L", id, "NO_MAPS_MENU")
	}

	return PLUGIN_HANDLED
}

public delayedChange(mapname[])
{
	engine_changelevel(mapname)
}

public actionVoteMapMenu(id, key)
{
	new tempMap[32];
	switch (key)
	{
		case 7:
		{
			new Float:voting = get_cvar_float("amx_last_voting")
		
			if (voting > get_gametime())
			{
				client_print(id, print_chat, "%L", id, "ALREADY_VOT")
				return PLUGIN_HANDLED
			}

			if (voting && voting + get_cvar_float("amx_vote_delay") > get_gametime())
			{
				client_print(id, print_chat, "%L", id, "VOT_NOW_ALLOW")
				return PLUGIN_HANDLED
			}

			g_voteCount = {0, 0, 0, 0, 0}
			
			new Float:vote_time = get_cvar_float("amx_vote_time") + 2.0
			set_cvar_float("amx_last_voting", get_gametime() + vote_time)
			new iVoteTime = floatround(vote_time)

			set_task(vote_time, "checkVotes", 34567 + id)

			new menuBody[512]
			new players[MAX_PLAYERS]
			new pnum, keys, len

			get_players(players, pnum)

			if (g_voteSelectedNum[id] > 1)
			{
				len = format(menuBody, charsmax(menuBody), g_coloredMenus ? "\y%L^n\w^n" : "%L^n^n", id, "WHICH_MAP")
				
				for (new c = 0; c < g_voteSelectedNum[id]; ++c)
				{
					ArrayGetString(g_mapName, g_voteSelected[id][c], tempMap, charsmax(tempMap));
					len += format(menuBody[len], charsmax(menuBody) - len, "%d. %s^n", c + 1, tempMap)
					keys |= (1<<c)
				}
				
				keys |= (1<<8)
				len += format(menuBody[len], charsmax(menuBody) - len, "^n9. %L^n", id, "NONE")
			} else {
				ArrayGetString(g_mapName, g_voteSelected[id][0], tempMap, charsmax(tempMap));
				len = format(menuBody, charsmax(menuBody), g_coloredMenus ? "\y%L^n%s?^n\w^n1. %L^n2. %L^n" : "%L^n%s?^n^n1. %L^n2. %L^n", id, "CHANGE_MAP_TO", tempMap, id, "YES", id, "NO")
				keys = MENU_KEY_1|MENU_KEY_2
			}

			new menuName[64]
			format(menuName, charsmax(menuName), "%L", "en", "WHICH_MAP")

			for (new b = 0; b < pnum; ++b)
				if (players[b] != id)
					show_menu(players[b], keys, menuBody, iVoteTime, menuName)

			format(menuBody[len], charsmax(menuBody), "^n0. %L", id, "CANC_VOTE")
			keys |= MENU_KEY_0
			show_menu(id, keys, menuBody, iVoteTime, menuName)

			new authid[32], name[MAX_NAME_LENGTH]
			
			get_user_authid(id, authid, charsmax(authid))
			get_user_name(id, name, charsmax(name))

			show_activity_key("ADMIN_V_MAP_1", "ADMIN_V_MAP_2", name);

			new tempMapA[32];
			new tempMapB[32];
			new tempMapC[32];
			new tempMapD[32];
			if (g_voteSelectedNum[id] > 0)
			{
				ArrayGetString(g_mapName, g_voteSelected[id][0], tempMapA, charsmax(tempMapA));
			}
			else
			{
				copy(tempMapA, charsmax(tempMapA), "");
			}
			if (g_voteSelectedNum[id] > 1)
			{
				ArrayGetString(g_mapName, g_voteSelected[id][1], tempMapB, charsmax(tempMapB));
			}
			else
			{
				copy(tempMapB, charsmax(tempMapB), "");
			}
			if (g_voteSelectedNum[id] > 2)
			{
				ArrayGetString(g_mapName, g_voteSelected[id][2], tempMapC, charsmax(tempMapC));
			}
			else
			{
				copy(tempMapC, charsmax(tempMapC), "");
			}
			if (g_voteSelectedNum[id] > 3)
			{
				ArrayGetString(g_mapName, g_voteSelected[id][3], tempMapD, charsmax(tempMapD));
			}
			else
			{
				copy(tempMapD, charsmax(tempMapD), "");
			}
			
			log_amx("Vote: ^"%s<%d><%s><>^" vote maps (map#1 ^"%s^") (map#2 ^"%s^") (map#3 ^"%s^") (map#4 ^"%s^")", 
					name, get_user_userid(id), authid, 
					tempMapA, tempMapB, tempMapC, tempMapD)
		}
		case 8: displayVoteMapsMenu(id, ++g_menuPosition[id])
		case 9: displayVoteMapsMenu(id, --g_menuPosition[id])
		default:
		{
			g_voteSelected[id][g_voteSelectedNum[id]++] = g_menuPosition[id] * 7 + key
			displayVoteMapsMenu(id, g_menuPosition[id])
		}
	}

	return PLUGIN_HANDLED
}

public actionMapsMenu(id, key)
{
	switch (key)
	{
		case 8: displayMapsMenu(id, ++g_menuPosition[id])
		case 9: displayMapsMenu(id, --g_menuPosition[id])
		default:
		{
			new a = g_menuPosition[id] * 8 + key
			new _modName[10]

			get_modname(_modName, charsmax(_modName))
			if (!equal(_modName, "zp"))
			{
				message_begin(MSG_ALL, SVC_INTERMISSION)
				message_end()
			}
			
			new authid[32], name[MAX_NAME_LENGTH]
			
			get_user_authid(id, authid, charsmax(authid))
			get_user_name(id, name, charsmax(name))

			new tempMap[32];
			ArrayGetString(g_mapName, a, tempMap, charsmax(tempMap));
			
			show_activity_key("ADMIN_CHANGEL_1", "ADMIN_CHANGEL_2", name, tempMap);

			log_amx("Cmd: ^"%s<%d><%s><>^" changelevel ^"%s^"", name, get_user_userid(id), authid, tempMap)
			set_task(2.0, "delayedChange", 0, tempMap, strlen(tempMap) + 1)
			/* displayMapsMenu(id, g_menuPosition[id]) */
		}
	}
	
	return PLUGIN_HANDLED
}

displayMapsMenu(id, pos)
{
	if (pos < 0)
		return

	new menuBody[512]
	new tempMap[32]
	new start = pos * 8
	new b = 0

	if (start >= g_mapNums)
		start = pos = g_menuPosition[id] = 0

	new len = format(menuBody, charsmax(menuBody), g_coloredMenus ? "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n", id, "CHANGLE_MENU", pos + 1, (g_mapNums / 8 + ((g_mapNums % 8) ? 1 : 0)))
	new end = start + 8
	new keys = MENU_KEY_0

	if (end > g_mapNums)
		end = g_mapNums

	for (new a = start; a < end; ++a)
	{
		keys |= (1<<b)
		ArrayGetString(g_mapName, a, tempMap, charsmax(tempMap));
		len += format(menuBody[len], charsmax(menuBody) - len, "%d. %s^n", ++b, tempMap)
	}

	if (end != g_mapNums)
	{
		format(menuBody[len], charsmax(menuBody) - len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT")
		keys |= MENU_KEY_9
	}
	else
		format(menuBody[len], charsmax(menuBody) - len, "^n0. %L", id, pos ? "BACK" : "EXIT")

	new menuName[64]
	format(menuName, 63, "%L", "en", "CHANGLE_MENU")

	show_menu(id, keys, menuBody, -1, menuName)
}
stock bool:ValidMap(mapname[])
{
	if ( is_map_valid(mapname) )
	{
		return true;
	}
	// If the is_map_valid check failed, check the end of the string
	new len = strlen(mapname) - 4;
	
	// The mapname was too short to possibly house the .bsp extension
	if (len < 0)
	{
		return false;
	}
	if ( equali(mapname[len], ".bsp") )
	{
		// If the ending was .bsp, then cut it off.
		// the string is byref'ed, so this copies back to the loaded text.
		mapname[len] = '^0';
		
		// recheck
		if ( is_map_valid(mapname) )
		{
			return true;
		}
	}
	
	return false;
}

load_settings(filename[])
{
	new fp = fopen(filename, "r");
	
	if (!fp)
	{
		return 0;
	}
		

	new text[256];
	new tempMap[32];
	
	while (fgets(fp, text, charsmax(text)))
	{
		if (text[0] == ';')
		{
			continue;
		}
		if (parse(text, tempMap, charsmax(tempMap)) < 1)
		{
			continue;
		}
		if (!ValidMap(tempMap))
		{
			continue;
		}
		
		ArrayPushString(g_mapName, tempMap);
		g_mapNums++;
	}

	fclose(fp);

	return 1;
}

public plugin_end()
{
	ArrayDestroy(g_mapName)
}
