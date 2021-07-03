// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Nextmap Chooser Plugin
//

#include <amxmodx>
#include <amxmisc>

#define SELECTMAPS  5

new Array:g_mapName;
new g_mapNums;

new g_nextName[SELECTMAPS]
new g_voteCount[SELECTMAPS + 2]
new g_mapVoteNum
new g_teamScore[2]
new g_lastMap[32]

new g_coloredMenus
new bool:g_selected = false

public plugin_init()
{
	register_plugin("Nextmap Chooser", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("mapchooser.txt")
	register_dictionary("common.txt")
	
	g_mapName=ArrayCreate(32);
	
	new MenuName[64]
	
	format(MenuName, charsmax(MenuName), "%L", "en", "CHOOSE_NEXTM")
	register_menucmd(register_menuid(MenuName), (-1^(-1<<(SELECTMAPS+2))), "countVote")
	register_cvar("amx_extendmap_max", "90")
	register_cvar("amx_extendmap_step", "15")

	if (cstrike_running())
		register_event("TeamScore", "team_score", "a")

	get_localinfo("lastMap", g_lastMap, charsmax(g_lastMap))
	set_localinfo("lastMap", "")

	new maps_ini_file[64]
	get_configsdir(maps_ini_file, charsmax(maps_ini_file));
	format(maps_ini_file, charsmax(maps_ini_file), "%s/maps.ini", maps_ini_file);
	
	if (!file_exists(maps_ini_file))
		get_cvar_string("mapcyclefile", maps_ini_file, charsmax(maps_ini_file))
	if (loadSettings(maps_ini_file))
		set_task(15.0, "voteNextmap", 987456, "", 0, "b")

	g_coloredMenus = colored_menus()
	
}

public checkVotes()
{
	new b = 0
	
	for (new a = 0; a < g_mapVoteNum; ++a)
		if (g_voteCount[b] < g_voteCount[a])
			b = a

	
	if (g_voteCount[SELECTMAPS] > g_voteCount[b]
	    && g_voteCount[SELECTMAPS] > g_voteCount[SELECTMAPS+1])
	{
		new mapname[32]
		
		get_mapname(mapname, charsmax(mapname))
		new Float:steptime = get_cvar_float("amx_extendmap_step")
		set_cvar_float("mp_timelimit", get_cvar_float("mp_timelimit") + steptime)
		client_print(0, print_chat, "%L", LANG_PLAYER, "CHO_FIN_EXT", steptime)
		log_amx("Vote: Voting for the nextmap finished. Map %s will be extended to next %.0f minutes", mapname, steptime)
		
		return
	}
	
	new smap[32]
	if (g_voteCount[b] && g_voteCount[SELECTMAPS + 1] <= g_voteCount[b])
	{
		ArrayGetString(g_mapName, g_nextName[b], smap, charsmax(smap));
		set_cvar_string("amx_nextmap", smap);
	}

	
	get_cvar_string("amx_nextmap", smap, charsmax(smap))
	client_print(0, print_chat, "%L", LANG_PLAYER, "CHO_FIN_NEXT", smap)
	log_amx("Vote: Voting for the nextmap finished. The nextmap will be %s", smap)
}

public countVote(id, key)
{
	if (get_cvar_float("amx_vote_answers"))
	{
		new name[MAX_NAME_LENGTH]
		get_user_name(id, name, charsmax(name))
		
		if (key == SELECTMAPS)
			client_print(0, print_chat, "%L", LANG_PLAYER, "CHOSE_EXT", name)
		else if (key < SELECTMAPS)
		{
			new map[32];
			ArrayGetString(g_mapName, g_nextName[key], map, charsmax(map));
			client_print(0, print_chat, "%L", LANG_PLAYER, "X_CHOSE_X", name, map);
		}
	}
	++g_voteCount[key]
	
	return PLUGIN_HANDLED
}

bool:isInMenu(id)
{
	for (new a = 0; a < g_mapVoteNum; ++a)
		if (id == g_nextName[a])
			return true
	return false
}

public voteNextmap()
{
	new winlimit = get_cvar_num("mp_winlimit")
	new maxrounds = get_cvar_num("mp_maxrounds")
	
	if (winlimit)
	{
		new c = winlimit - 2
		
		if ((c > g_teamScore[0]) && (c > g_teamScore[1]))
		{
			g_selected = false
			return
		}
	}
	else if (maxrounds)
	{
		if ((maxrounds - 2) > (g_teamScore[0] + g_teamScore[1]))
		{
			g_selected = false
			return
		}
	} else {
		new timeleft = get_timeleft()
		
		if (timeleft < 1 || timeleft > 129)
		{
			g_selected = false
			return
		}
	}

	if (g_selected)
		return

	g_selected = true
	
	new menu[512], a, mkeys = (1<<SELECTMAPS + 1)

	new pos = format(menu, charsmax(menu), g_coloredMenus ? "\y%L:\w^n^n" : "%L:^n^n", LANG_SERVER, "CHOOSE_NEXTM")
	new dmax = (g_mapNums > SELECTMAPS) ? SELECTMAPS : g_mapNums
	
	for (g_mapVoteNum = 0; g_mapVoteNum < dmax; ++g_mapVoteNum)
	{
		a = random_num(0, g_mapNums - 1)
		
		while (isInMenu(a))
			if (++a >= g_mapNums) a = 0
		
		g_nextName[g_mapVoteNum] = a
		pos += format(menu[pos], charsmax(menu) - pos, "%d. %a^n", g_mapVoteNum + 1, ArrayGetStringHandle(g_mapName, a));
		mkeys |= (1<<g_mapVoteNum)
		g_voteCount[g_mapVoteNum] = 0
	}
	
	menu[pos++] = '^n'
	g_voteCount[SELECTMAPS] = 0
	g_voteCount[SELECTMAPS + 1] = 0
	
	new mapname[32]
	get_mapname(mapname, charsmax(mapname))

	if ((winlimit + maxrounds) == 0 && (get_cvar_float("mp_timelimit") < get_cvar_float("amx_extendmap_max")))
	{
		pos += format(menu[pos], charsmax(menu) - pos, "%d. %L^n", SELECTMAPS + 1, LANG_SERVER, "EXTED_MAP", mapname)
		mkeys |= (1<<SELECTMAPS)
	}

	format(menu[pos], charsmax(menu), "%d. %L", SELECTMAPS+2, LANG_SERVER, "NONE")
	new MenuName[64]
	
	format(MenuName, charsmax(MenuName), "%L", "en", "CHOOSE_NEXTM")
	show_menu(0, mkeys, menu, 15, MenuName)
	set_task(15.0, "checkVotes")
	client_print(0, print_chat, "%L", LANG_SERVER, "TIME_CHOOSE")
	client_cmd(0, "spk Gman/Gman_Choose2")
	log_amx("Vote: Voting for the nextmap started")
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

loadSettings(filename[])
{
	if (!file_exists(filename))
		return 0

	new szText[32]
	new currentMap[32]
	
	new buff[256];
	
	get_mapname(currentMap, charsmax(currentMap))

	new fp=fopen(filename,"r");
	
	while (fgets(fp, buff, charsmax(buff)))
	{
		szText[0]='^0';
		
		parse(buff, szText, charsmax(szText));
		
		
		if (szText[0] != ';' &&
			ValidMap(szText) &&
			!equali(szText, g_lastMap) &&
			!equali(szText, currentMap))
		{
			ArrayPushString(g_mapName, szText);
			++g_mapNums;
		}
		
	}
	
	fclose(fp);

	return g_mapNums
}

public team_score()
{
	new team[2]
	
	read_data(1, team, charsmax(team))
	g_teamScore[(team[0]=='C') ? 0 : 1] = read_data(2)
}

public plugin_end()
{
	new current_map[32]

	get_mapname(current_map, charsmax(current_map))
	set_localinfo("lastMap", current_map)

	ArrayDestroy(g_mapName)
}
