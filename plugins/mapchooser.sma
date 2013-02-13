/* AMX Mod X
*   Nextmap Chooser Plugin
*
* by the AMX Mod X Development Team
*  originally developed by OLO
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

#define SELECTMAPS  5

#define charsof(%1) (sizeof(%1)-1)

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
	
	format(MenuName, 63, "%L", "en", "CHOOSE_NEXTM")
	register_menucmd(register_menuid(MenuName), (-1^(-1<<(SELECTMAPS+2))), "countVote")
	register_cvar("amx_extendmap_max", "90")
	register_cvar("amx_extendmap_step", "15")

	if (cstrike_running())
		register_event("TeamScore", "team_score", "a")

	get_localinfo("lastMap", g_lastMap, 31)
	set_localinfo("lastMap", "")

	new maps_ini_file[64]
	get_configsdir(maps_ini_file, 63);
	format(maps_ini_file, 63, "%s/maps.ini", maps_ini_file);
	
	if (!file_exists(maps_ini_file))
		get_cvar_string("mapcyclefile", maps_ini_file, 63)
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
		
		get_mapname(mapname, 31)
		new Float:steptime = get_cvar_float("amx_extendmap_step")
		set_cvar_float("mp_timelimit", get_cvar_float("mp_timelimit") + steptime)
		client_print(0, print_chat, "%L", LANG_PLAYER, "CHO_FIN_EXT", steptime)
		log_amx("Vote: Voting for the nextmap finished. Map %s will be extended to next %.0f minutes", mapname, steptime)
		
		return
	}
	
	new smap[32]
	if (g_voteCount[b] && g_voteCount[SELECTMAPS + 1] <= g_voteCount[b])
	{
		ArrayGetString(g_mapName, g_nextName[b], smap, charsof(smap));
		set_cvar_string("amx_nextmap", smap);
	}

	
	get_cvar_string("amx_nextmap", smap, 31)
	client_print(0, print_chat, "%L", LANG_PLAYER, "CHO_FIN_NEXT", smap)
	log_amx("Vote: Voting for the nextmap finished. The nextmap will be %s", smap)
}

public countVote(id, key)
{
	if (get_cvar_float("amx_vote_answers"))
	{
		new name[32]
		get_user_name(id, name, 31)
		
		if (key == SELECTMAPS)
			client_print(0, print_chat, "%L", LANG_PLAYER, "CHOSE_EXT", name)
		else if (key < SELECTMAPS)
		{
			new map[32];
			ArrayGetString(g_mapName, g_nextName[key], map, charsof(map));
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

	new pos = format(menu, 511, g_coloredMenus ? "\y%L:\w^n^n" : "%L:^n^n", LANG_SERVER, "CHOOSE_NEXTM")
	new dmax = (g_mapNums > SELECTMAPS) ? SELECTMAPS : g_mapNums
	
	for (g_mapVoteNum = 0; g_mapVoteNum < dmax; ++g_mapVoteNum)
	{
		a = random_num(0, g_mapNums - 1)
		
		while (isInMenu(a))
			if (++a >= g_mapNums) a = 0
		
		g_nextName[g_mapVoteNum] = a
		pos += format(menu[pos], 511, "%d. %a^n", g_mapVoteNum + 1, ArrayGetStringHandle(g_mapName, a));
		mkeys |= (1<<g_mapVoteNum)
		g_voteCount[g_mapVoteNum] = 0
	}
	
	menu[pos++] = '^n'
	g_voteCount[SELECTMAPS] = 0
	g_voteCount[SELECTMAPS + 1] = 0
	
	new mapname[32]
	get_mapname(mapname, 31)

	if ((winlimit + maxrounds) == 0 && (get_cvar_float("mp_timelimit") < get_cvar_float("amx_extendmap_max")))
	{
		pos += format(menu[pos], 511, "%d. %L^n", SELECTMAPS + 1, LANG_SERVER, "EXTED_MAP", mapname)
		mkeys |= (1<<SELECTMAPS)
	}

	format(menu[pos], 511, "%d. %L", SELECTMAPS+2, LANG_SERVER, "NONE")
	new MenuName[64]
	
	format(MenuName, 63, "%L", "en", "CHOOSE_NEXTM")
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
	
	get_mapname(currentMap, 31)

	new fp=fopen(filename,"r");
	
	while (!feof(fp))
	{
		buff[0]='^0';
		szText[0]='^0';
		
		fgets(fp, buff, charsof(buff));
		
		parse(buff, szText, charsof(szText));
		
		
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
	
	read_data(1, team, 1)
	g_teamScore[(team[0]=='C') ? 0 : 1] = read_data(2)
}

public plugin_end()
{
	new current_map[32]

	get_mapname(current_map, 31)
	set_localinfo("lastMap", current_map)
}
