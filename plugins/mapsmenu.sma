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
new g_mapNums;

new g_voteCount[5];

new g_voteSelected[MAX_PLAYERS + 1][4];
new g_voteSelectedNum[MAX_PLAYERS + 1];

new g_choosed;

public plugin_init()
{
	register_plugin("Maps Menu", AMXX_VERSION_STR, "AMXX Dev Team");
	
	register_dictionary("mapsmenu.txt");
	register_dictionary("common.txt");
	
	register_clcmd("amx_mapmenu", "cmdMapsMenu", ADMIN_MAP, "- displays changelevel menu");
	register_clcmd("amx_votemapmenu", "cmdVoteMapMenu", ADMIN_VOTE, "- displays votemap menu");
	register_clcmd("searchmap", "searchMap", ADMIN_MAP);
	register_clcmd("searchvotemap", "searchVoteMap", ADMIN_VOTE);

	g_mapName = ArrayCreate(32);
	
	new maps_ini_file[64];
	get_configsdir(maps_ini_file, charsmax(maps_ini_file));
	add(maps_ini_file, charsmax(maps_ini_file), "/maps.ini");

	if (!file_exists(maps_ini_file))
	{
		get_cvar_string("mapcyclefile", maps_ini_file, charsmax(maps_ini_file));
	}
		
	if (!file_exists(maps_ini_file))
	{
		formatex(maps_ini_file, charsmax(maps_ini_file), "mapcycle.txt");
	}
	
	load_settings(maps_ini_file);
}

public autoRefuse()
{
	log_amx("Vote: %L", "en", "RESULT_REF");
	client_print(0, print_chat, "%l", "RESULT_REF");
}

public actionResult(id, Menu, Item)
{
	remove_task(4545454);
	
	switch (Item)
	{
		case 0:
		{
			new _modName[10];
			get_modname(_modName, charsmax(_modName));
			
			if (!equal(_modName, "zp"))
			{
				message_begin(MSG_ALL, SVC_INTERMISSION);
				message_end();
			}

			new tempMap[32];
			ArrayGetString(g_mapName, g_choosed, tempMap, charsmax(tempMap));
			
			set_task(2.0, "delayedChange", 0, tempMap, strlen(tempMap) + 1);
			log_amx("Vote: %L", "en", "RESULT_ACC");
			client_print(0, print_chat, "%l", "RESULT_ACC");
		}
		case 1: autoRefuse();
	}
	
	return PLUGIN_HANDLED;
}

public checkVotes(id)
{
	id -= 34567;
	new num, ppl[MAX_PLAYERS], a = 0;
	
	get_players(ppl, num, "c");
	if (num == 0) num = 1;
	g_choosed = -1;
	
	for (new i = 0; i < g_voteSelectedNum[id]; ++i)
		if (g_voteCount[a] < g_voteCount[i])
			a = i;

	new votesNum = g_voteCount[0] + g_voteCount[1] + g_voteCount[2] + g_voteCount[3] + g_voteCount[4];
	new iRatio = votesNum ? floatround(get_cvar_float("amx_votemap_ratio") * float(votesNum), floatround_ceil) : 1;
	new iResult = g_voteCount[a];
	if (iResult >= iRatio)
	{
		new tempMap[32];

		g_choosed = g_voteSelected[id][a];
		ArrayGetString(g_mapName, g_choosed, tempMap, charsmax(tempMap));
		client_print(0, print_chat, "%l %s", "VOTE_SUCCESS", tempMap);
		log_amx("Vote: %L %s", "en", "VOTE_SUCCESS", tempMap);
	}
	if (g_choosed != -1)
	{
		if (is_user_connected(id))
		{
			new menuBody[64], tempMap[32], Lang[20];
			
			ArrayGetString(g_mapName, g_choosed, tempMap, charsmax(tempMap));
			formatex(menuBody, charsmax(menuBody), "\y%l  \r%s\y^n%l", "THE_WINNER", tempMap, "WANT_CONT");
			new Menu = menu_create(menuBody, "actionResult");
			
			formatex(Lang, charsmax(Lang), "%l", "YES");
			menu_additem(Menu, Lang);
			formatex(Lang, charsmax(Lang), "%l", "NO");
			menu_additem(Menu, Lang);
			menu_setprop(Menu, MPROP_EXIT, MEXIT_NEVER);
			menu_display(id, Menu);
			
			set_task(10.0, "autoRefuse", 4545454);
		} 
		else 
		{
			new _modName[10], tempMap[32];		
			get_modname(_modName, charsmax(_modName));	
					
			if (!equal(_modName, "zp"))		
			{		
				message_begin(MSG_ALL, SVC_INTERMISSION);
				message_end();
			}
			
			ArrayGetString(g_mapName, g_choosed, tempMap, charsmax(tempMap));
			set_task(2.0, "delayedChange", 0, tempMap, strlen(tempMap) + 1);
		}
	} 
	else 
	{
		client_print(0, print_chat, "%l", "VOTE_FAILED");
		log_amx("Vote: %L", "en", "VOTE_FAILED");
	}
	
	remove_task(34567 + id);
}

public voteCount(id, Menu, Item)
{
	if (Item < 0)
	{
		return PLUGIN_HANDLED;
	}
	if (Item > 3)
	{
		client_print(0, print_chat, "%l", "VOT_CANC");
		remove_task(34567 + id);
		set_cvar_float("amx_last_voting", get_gametime());
		log_amx("Vote: Cancel vote session");
		
		return PLUGIN_HANDLED;
	}
	
	if (get_cvar_float("amx_vote_answers"))
	{
		new name[MAX_NAME_LENGTH], MapName[32], Access, Info[1], Callback;
		
		get_user_name(id, name, charsmax(name));
		menu_item_getinfo(Menu, Item, Access, Info, charsmax(Info), MapName, charsmax(MapName), Callback);
		client_print(0, print_chat, "%l", "X_VOTED_FOR", name, MapName);
	}
	
	++g_voteCount[Item];
	
	return PLUGIN_HANDLED;
}

bool:isMapSelected(id, pos)
{
	for (new i = 0; i < g_voteSelectedNum[id]; ++i)
		if (g_voteSelected[id][i] == pos)
			return true;
	return false;
}

displayVoteMapsMenu(id, Page = 0, Search[] = "")
{
	new menuBody[64], tempMap[32];
	formatex(menuBody, charsmax(menuBody), "\y%l", "CHANGLE_MENU");
	new Menu = menu_create(menuBody, "actionVoteMapMenu");
	
	new Counter = 1, bool:LastItem, SearchBy[80], StartVoting[50], Info[5];
	formatex(StartVoting, charsmax(StartVoting), "%l", "VOT_START", g_voteSelectedNum[id]);
	new len = strlen(Search);
	formatex(SearchBy, charsmax(SearchBy), "%l ^"\r%s\w^"", "VOT_SEARCH", Search);
	
	for (new i = 0; i < g_mapNums; ++i)
	{
		ArrayGetString(g_mapName, i, tempMap, charsmax(tempMap));
		if (len != 0)
		{
			if (containi(tempMap, Search) == -1)
			{
				continue;
			}
		}
		num_to_str(i, Info, charsmax(Info));
		if ( Counter == 5 )
		{
			add(tempMap, charsmax(tempMap), "^n");
			if (isMapSelected(id, i))
			{
				format(tempMap, charsmax(tempMap), "\r%s", tempMap);
				menu_additem(Menu, tempMap, Info);
			}
			else
			{
				menu_additem(Menu, tempMap, Info);
			}
			menu_additem(Menu, SearchBy);
			menu_additem(Menu, StartVoting);
			Counter = 1;
			LastItem = true;
		}
		else
		{
			num_to_str(i, Info, charsmax(Info));
			if (isMapSelected(id, i))
			{
				format(tempMap, charsmax(tempMap), "\r%s", tempMap);
				menu_additem(Menu, tempMap, Info);
			}
			else
			{
				menu_additem(Menu, tempMap, Info);
			}
			Counter++;
			LastItem = false;
		}
	}
	
	if (menu_items(Menu) == 0)
	{
		addBlankOnMenu(Menu, 5);
		menu_additem(Menu, SearchBy);
		menu_additem(Menu, StartVoting);
	}
	
	if ( LastItem )
	{
		--Counter;
	}
	
	if ( Counter & 6 != 0 )
	{
		new Lines = 7;
		while ( Lines-- - Counter & 6 )
		{
			menu_addblank2(Menu);
		}
		menu_additem(Menu, SearchBy);
		menu_additem(Menu, StartVoting);
	}
	
	new Lang[20];
	formatex(Lang, charsmax(Lang), "%l", "EXIT");
	menu_setprop(Menu, MPROP_EXITNAME, Lang);
	formatex(Lang, charsmax(Lang), "%l", "MORE");
	menu_setprop(Menu, MPROP_NEXTNAME, Lang);
	formatex(Lang, charsmax(Lang), "%l", "BACK");
	menu_setprop(Menu, MPROP_BACKNAME, Lang);
	menu_display(id, Menu, Page);
}

public searchVoteMap(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}
	
	new Search[32];
	read_argv(1, Search, charsmax(Search));
	
	displayVoteMapsMenu(id, 0, Search);
	return PLUGIN_HANDLED;
}

public cmdVoteMapMenu(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}

	if (get_cvar_float("amx_last_voting") > get_gametime())
	{
		client_print(id, print_chat, "%l", "ALREADY_VOT");
		return PLUGIN_HANDLED;
	}

	g_voteSelectedNum[id] = 0;

	if (g_mapNums)
	{
		displayVoteMapsMenu(id);
	} 
	else
	{
		console_print(id, "%l", "NO_MAPS_MENU");
		client_print(id, print_chat, "%l", "NO_MAPS_MENU");
	}

	return PLUGIN_HANDLED;
}

public cmdMapsMenu(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}

	if (g_mapNums)
	{
		displayMapsMenu(id);
	} 
	else 
	{
		console_print(id, "%l", "NO_MAPS_MENU");
		client_print(id, print_chat, "%l", "NO_MAPS_MENU");
	}

	return PLUGIN_HANDLED;
}

public delayedChange(mapname[])
{	
	engine_changelevel(mapname);
}

public actionVoteMapMenu(id, Menu, Item)
{
	if (Item < 0)
	{
		return PLUGIN_HANDLED;
	}
	if ((Item+2) % 7 == 0)
	{
		client_cmd(id, "messagemode searchvotemap");
		client_print(id, print_center, "%l", "VOT_SEARCH2");
		return PLUGIN_HANDLED;
	}
	if ((Item+1) % 7 == 0)
	{
		if (g_voteSelectedNum[id] == 0)
		{
			client_print(id, print_chat, "%l", "VOT_NO_MAPS");
			return PLUGIN_HANDLED;
		}
		new Float:voting = get_cvar_float("amx_last_voting");
		
		if (voting > get_gametime())
		{
			client_print(id, print_chat, "%l", "ALREADY_VOT");
			return PLUGIN_HANDLED;
		}

		if (voting && voting + get_cvar_float("amx_vote_delay") > get_gametime())
		{
			client_print(id, print_chat, "%l", "VOT_NOW_ALLOW");
			return PLUGIN_HANDLED;
		}

		g_voteCount = {0, 0, 0, 0, 0};
			
		new Float:vote_time = get_cvar_float("amx_vote_time") + 2.0;
		set_cvar_float("amx_last_voting", get_gametime() + vote_time);
		new VoteTime = floatround(vote_time);

		set_task(vote_time, "checkVotes", 34567 + id);

		new menuBody[64], tempMap[32], Lang[20];
		new players[MAX_PLAYERS];
		new pnum, VoteMenu;		

		get_players(players, pnum);

		if (g_voteSelectedNum[id] > 1)
		{
			formatex(menuBody, charsmax(menuBody), "\y%l", "WHICH_MAP");
			VoteMenu = menu_create(menuBody, "voteCount");
			for (new i = 0; i < g_voteSelectedNum[id]; ++i)
			{
				ArrayGetString(g_mapName, g_voteSelected[id][i], tempMap, charsmax(tempMap));
				menu_additem(VoteMenu, tempMap);
			}
			
			menu_addblank2(VoteMenu);
			formatex(Lang, charsmax(Lang), "%l", "NONE");
			menu_additem(VoteMenu, Lang);
		} 
		else 
		{
			ArrayGetString(g_mapName, g_voteSelected[id][0], tempMap, charsmax(tempMap));
			formatex(menuBody, charsmax(menuBody), "\y%l %s?", "CHANGE_MAP_TO", tempMap);
			VoteMenu = menu_create(menuBody, "voteCount");
			formatex(Lang, charsmax(Lang), "%l", "YES");
			menu_additem(VoteMenu, Lang);
			formatex(Lang, charsmax(Lang), "%l", "NO");
			menu_additem(VoteMenu, Lang);
		}
		
		menu_setprop(VoteMenu, MPROP_EXIT, MEXIT_NEVER);
		
		for (new i = 0; i < pnum; ++i)
			if (players[i] != id)
				menu_display(players[i], VoteMenu, _, VoteTime);

		menu_addblank2(VoteMenu);
		formatex(Lang, charsmax(Lang), "%l", "CANC_VOTE");
		menu_additem(VoteMenu, Lang);
		menu_display(id, VoteMenu, _, VoteTime);

		new authid[32], name[MAX_NAME_LENGTH];
			
		get_user_authid(id, authid, charsmax(authid));
		get_user_name(id, name, charsmax(name));

		show_activity_key("ADMIN_V_MAP_1", "ADMIN_V_MAP_2", name);

		new tempMapA[32], tempMapB[32], tempMapC[32], tempMapD[32];
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
				tempMapA, tempMapB, tempMapC, tempMapD);
		return PLUGIN_HANDLED;
	}
	
	new MapName[32], Access, Info[5], Callback, pos;
	menu_item_getinfo(Menu, Item, Access, Info, charsmax(Info), MapName, charsmax(MapName), Callback);
	pos = str_to_num(Info);
	if (isMapSelected(id, pos))
	{
		g_voteSelected[id][--g_voteSelectedNum[id]] = 0;
	}
	else
	{
		if (g_voteSelectedNum[id] < 4)
		{
			g_voteSelected[id][g_voteSelectedNum[id]++] = pos;
		}
		else
		{
			client_print(id, print_chat, "%l", "VOT_TOO_MANY");
		}
	}
	displayVoteMapsMenu(id, Item/7);
	return PLUGIN_HANDLED;
}

public actionMapsMenu(id, Menu, Item)
{
	if (Item < 0)
	{
		return PLUGIN_HANDLED;
	}
	if ((Item+1) % 7 == 0)
	{
		client_cmd(id, "messagemode searchmap");
		client_print(id, print_center, "%l", "VOT_SEARCH2");
		return PLUGIN_HANDLED;
	}
	
	new _modName[10];

	get_modname(_modName, charsmax(_modName));
	if (!equal(_modName, "zp"))
	{
		message_begin(MSG_ALL, SVC_INTERMISSION);
		message_end();
	}
	
	new MapName[32], Name[MAX_NAME_LENGTH], authid[32], Access, Info[1], Callback;
	get_user_name(id, Name, charsmax(Name));
	get_user_authid(id, authid, charsmax(authid));
	menu_item_getinfo(Menu, Item, Access, Info, charsmax(Info), MapName, charsmax(MapName), Callback);
	show_activity_key("ADMIN_CHANGEL_1", "ADMIN_CHANGEL_2", Name, MapName); 
	log_amx("Cmd: ^"%s<%d><%s><>^" changelevel ^"%s^"", Name, get_user_userid(id), authid, MapName);
	set_task(2.0, "delayedChange", 0, MapName, strlen(MapName) + 1);
	return PLUGIN_HANDLED;
}

displayMapsMenu(id, Search[] = "")
{
	new menuBody[64], tempMap[32];
	formatex(menuBody, charsmax(menuBody), "\y%l", "CHANGLE_MENU");
	new Menu = menu_create(menuBody, "actionMapsMenu");
	
	new Counter = 1, bool:LastItem;
	new len = strlen(Search);
	new SearchBy[80];
	formatex(SearchBy, charsmax(SearchBy), "%l ^"\r%s\w^"", "VOT_SEARCH", Search);
	
	for (new i = 0; i < g_mapNums; ++i)
	{
		ArrayGetString(g_mapName, i, tempMap, charsmax(tempMap));
		if (len != 0)
		{
			if (containi(tempMap, Search) == -1)
			{
				continue;
			}
		}
		if ( Counter == 6 )
		{
			add(tempMap, charsmax(tempMap), "^n");
			menu_additem(Menu, tempMap);
			menu_additem(Menu, SearchBy);
			Counter = 1;
			LastItem = true;
		}
		else
		{
			menu_additem(Menu, tempMap);
			Counter++;
			LastItem = false;
		}
	}
	
	if (menu_items(Menu) == 0)
	{
		addBlankOnMenu(Menu, 6);
		menu_additem(Menu, SearchBy);
	}
	
	if ( LastItem )
	{
		--Counter;
	}
	
	if ( Counter & 7 != 0 )
	{
		new Lines = 7;
		while ( Lines-- - Counter & 7 )
		{
			menu_addblank2(Menu);
		}
		menu_additem(Menu, SearchBy);
	}
	
	new Lang[20];
	formatex(Lang, charsmax(Lang), "%l", "EXIT");
	menu_setprop(Menu, MPROP_EXITNAME, Lang);
	formatex(Lang, charsmax(Lang), "%l", "MORE");
	menu_setprop(Menu, MPROP_NEXTNAME, Lang);
	formatex(Lang, charsmax(Lang), "%l", "BACK");
	menu_setprop(Menu, MPROP_BACKNAME, Lang);
	menu_display(id, Menu);
}

public searchMap(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}
	
	new Search[64];
	read_argv(1, Search, charsmax(Search));
	
	displayMapsMenu(id, Search);
	return PLUGIN_HANDLED;
}

bool:ValidMap(mapname[])
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
	
	while (!feof(fp))
	{
		fgets(fp, text, charsmax(text));
		
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

addBlankOnMenu(Menu, blanks)
{
	for(new i = 0; i < blanks; ++i)
			menu_addblank2(Menu);
}

public plugin_end()
{
	ArrayDestroy(g_mapName);
}
