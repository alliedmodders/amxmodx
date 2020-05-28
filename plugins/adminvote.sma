// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Admin Votes Plugin
//

#include <amxmodx>
#include <amxmisc>


new g_Answer[128]
new g_optionName[4][64]
new g_voteCount[4]
new g_validMaps
new g_yesNoVote
new g_coloredMenus
new g_voteCaller
new g_Execute[256]
new g_execLen

new bool:g_execResult
new Float:g_voteRatio

public plugin_init()
{
	register_plugin("Admin Votes", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("adminvote.txt")
	register_dictionary("common.txt")
	register_dictionary("mapsmenu.txt")
	register_menucmd(register_menuid("Change map to "), MENU_KEY_1|MENU_KEY_2, "voteCount")
	register_menucmd(register_menuid("Choose map: "), MENU_KEY_1|MENU_KEY_2|MENU_KEY_3|MENU_KEY_4, "voteCount")
	register_menucmd(register_menuid("Kick "), MENU_KEY_1|MENU_KEY_2, "voteCount")
	register_menucmd(register_menuid("Ban "), MENU_KEY_1|MENU_KEY_2, "voteCount")
	register_menucmd(register_menuid("Vote: "), MENU_KEY_1|MENU_KEY_2|MENU_KEY_3|MENU_KEY_4, "voteCount")
	register_menucmd(register_menuid("The result: "), MENU_KEY_1|MENU_KEY_2, "actionResult")
	register_concmd("amx_votemap", "cmdVoteMap", ADMIN_VOTE, "<map> [map] [map] [map]")
	register_concmd("amx_votekick", "cmdVoteKickBan", ADMIN_VOTE, "<name or #userid>")
	register_concmd("amx_voteban", "cmdVoteKickBan", ADMIN_VOTE, "<name or #userid>")
	register_concmd("amx_vote", "cmdVote", ADMIN_VOTE, "<question> <answer#1> <answer#2>")
	register_concmd("amx_cancelvote", "cmdCancelVote", ADMIN_VOTE, "- cancels last vote")
	
	g_coloredMenus = colored_menus()
}

public cmdCancelVote(id, level, cid)
{
	if (!cmd_access(id, level, cid, 0))
		return PLUGIN_HANDLED

	if (task_exists(99889988, 1))
	{
		new authid[32], name[MAX_NAME_LENGTH]
		
		get_user_authid(id, authid, charsmax(authid))
		get_user_name(id, name, charsmax(name))
		log_amx("Vote: ^"%s<%d><%s><>^" cancel vote session", name, get_user_userid(id), authid)
	
		new msg[256];
		for (new i = 1; i <= MaxClients; i++)
		{
			if (is_user_connected(i) && !is_user_bot(i))
			{
				// HACK: ADMIN_CANC_VOTE_{1,2} keys were designed very poorly.  Remove all : and %s in it.
				LookupLangKey(msg, charsmax(msg), "ADMIN_CANC_VOTE_1", i);
				replace_all(msg, charsmax(msg), "%s", "");
				replace_all(msg, charsmax(msg), ":", "");
				trim(msg);
				show_activity_id(i, id, name, msg);
			}
		}
		
		console_print(id, "%L", id, "VOTING_CANC")
		client_print(0,print_chat,"%L",LANG_PLAYER,"VOTING_CANC")
		remove_task(99889988, 1)
		set_cvar_float("amx_last_voting", get_gametime())
	}
	else
		console_print(id, "%L", id, "NO_VOTE_CANC")

	return PLUGIN_HANDLED
}

public delayedExec(cmd[])
	server_cmd("%s", cmd)

public autoRefuse()
{
	log_amx("Vote: %L", "en", "RES_REF")
	client_print(0, print_chat, "%L", LANG_PLAYER, "RES_REF")
}

public actionResult(id, key)
{
	remove_task(4545454)
	
	switch (key)
	{
		case 0:
		{
			set_task(2.0, "delayedExec", 0, g_Execute, g_execLen)
			log_amx("Vote: %L", "en", "RES_ACCEPTED")
			client_print(0, print_chat, "%L", LANG_PLAYER, "RES_ACCEPTED")
		}
		case 1: autoRefuse()
	}
	
	return PLUGIN_HANDLED
}

public checkVotes()
{
	new best = 0
	
	if (!g_yesNoVote)
	{
		for (new a = 0; a < 4; ++a)
			if (g_voteCount[a] > g_voteCount[best])
		
		best = a
	}

	new votesNum = g_voteCount[0] + g_voteCount[1] + g_voteCount[2] + g_voteCount[3]
	new iRatio = votesNum ? floatround(g_voteRatio * float(votesNum), floatround_ceil) : 1
	new iResult = g_voteCount[best]
	new players[MAX_PLAYERS], pnum, i
	
	get_players(players, pnum, "c")
	
	if (iResult < iRatio)
	{
		new lVotingFailed[64]
		
		for (i = 0; i < pnum; i++)
		{
			format(lVotingFailed, 63, "%L", players[i], "VOTING_FAILED")
			if (g_yesNoVote)
				client_print(players[i], print_chat, "%L", players[i], "VOTING_RES_1", lVotingFailed, g_voteCount[0], g_voteCount[1], iRatio)
			else
				client_print(players[i], print_chat, "%L", players[i], "VOTING_RES_2", lVotingFailed, iResult, iRatio)
		}
		
		format(lVotingFailed, 63, "%L", "en", "VOTING_FAILED")
		log_amx("Vote: %s (got ^"%d^") (needed ^"%d^")", lVotingFailed, iResult, iRatio)
		
		return PLUGIN_CONTINUE
	}

	g_execLen = format(g_Execute, charsmax(g_Execute), g_Answer, g_optionName[best]) + 1
	
	if (g_execResult)
	{
		g_execResult = false
		
		if (is_user_connected(g_voteCaller))
		{
			new menuBody[512], lTheResult[32], lYes[16], lNo[16]
			
			format(lTheResult, charsmax(lTheResult), "%L", g_voteCaller, "THE_RESULT")
			format(lYes, charsmax(lYes), "%L", g_voteCaller, "YES")
			format(lNo, charsmax(lNo), "%L", g_voteCaller, "NO")
			
			new len = format(menuBody, charsmax(menuBody), g_coloredMenus ? "\y%s: \w%s^n^n" : "%s: %s^n^n", lTheResult, g_Execute)
			
			len += format(menuBody[len], charsmax(menuBody) - len, g_coloredMenus ? "\y%L^n\w" : "%L^n", g_voteCaller, "WANT_CONTINUE")
			format(menuBody[len], charsmax(menuBody) - len, "^n1. %s^n2. %s", lYes, lNo)
			show_menu(g_voteCaller, 0x03, menuBody, 10, "The result: ")
			set_task(10.0, "autoRefuse", 4545454)
		}
		else
			set_task(2.0, "delayedExec", 0, g_Execute, g_execLen)
	}
	
	new lVotingSuccess[32]
	
	for (i = 0; i < pnum; i++)
	{
		format(lVotingSuccess, charsmax(lVotingSuccess), "%L", players[i], "VOTING_SUCCESS")
		client_print(players[i], print_chat, "%L", players[i], "VOTING_RES_3", lVotingSuccess, iResult, iRatio, g_Execute)
	}
	
	format(lVotingSuccess, charsmax(lVotingSuccess), "%L", "en", "VOTING_SUCCESS")
	log_amx("Vote: %s (got ^"%d^") (needed ^"%d^") (result ^"%s^")", lVotingSuccess, iResult, iRatio, g_Execute)
	
	return PLUGIN_CONTINUE
}

public voteCount(id, key)
{
	if (get_cvar_num("amx_vote_answers"))
	{
		new name[MAX_NAME_LENGTH]
		get_user_name(id, name, charsmax(name))
		
		if (g_yesNoVote)
			client_print(0, print_chat, "%L", LANG_PLAYER, key ? "VOTED_AGAINST" : "VOTED_FOR", name)
		else
			client_print(0, print_chat, "%L", LANG_PLAYER, "VOTED_FOR_OPT", name, key + 1)
	}
	++g_voteCount[key]
	
	return PLUGIN_HANDLED
}

public cmdVoteMap(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
		return PLUGIN_HANDLED
	
	new Float:voting = get_cvar_float("amx_last_voting")
	if (voting > get_gametime())
	{
		console_print(id, "%L", id, "ALREADY_VOTING")
		return PLUGIN_HANDLED
	}
	
	if (voting && voting + get_cvar_float("amx_vote_delay") > get_gametime())
	{
		console_print(id, "%L", id, "VOTING_NOT_ALLOW")
		return PLUGIN_HANDLED
	}

	new argc = read_argc()
	if (argc > 5) argc = 5
	
	g_validMaps = 0
	g_optionName[0][0] = 0
	g_optionName[1][0] = 0
	g_optionName[2][0] = 0
	g_optionName[3][0] = 0
	
	for (new i = 1; i < argc; ++i)
	{
		read_argv(i, g_optionName[g_validMaps], 31)

		if (contain(g_optionName[g_validMaps], "..") != -1)
			continue

		if (is_map_valid(g_optionName[g_validMaps]))
			g_validMaps++
	}
	
	if (g_validMaps == 0)
	{
		new lMaps[16]
		
		format(lMaps, charsmax(lMaps), "%L", id, (argc == 2) ? "MAP_IS" : "MAPS_ARE")
		console_print(id, "%L", id, "GIVEN_NOT_VALID", lMaps)
		return PLUGIN_HANDLED
	}

	new menu_msg[256], len = 0
	new keys = 0
	
	if (g_validMaps > 1)
	{
		keys = MENU_KEY_0
		len = format(menu_msg, charsmax(menu_msg), g_coloredMenus ? "\y%L: \w^n^n" : "%L: ^n^n", LANG_SERVER, "CHOOSE_MAP")
		new temp[128]
		
		for (new a = 0; a < g_validMaps; ++a)
		{
			format(temp, charsmax(temp), "%d.  %s^n", a+1, g_optionName[a])
			len += copy(menu_msg[len], charsmax(menu_msg) - len, temp)
			keys |= (1<<a)
		}
		
		format(menu_msg[len], charsmax(menu_msg) - len, "^n0.  %L", LANG_SERVER, "NONE")
		g_yesNoVote = 0
	} else {
		new lChangeMap[32], lYes[16], lNo[16]
		
		format(lChangeMap, charsmax(lChangeMap), "%L", LANG_SERVER, "CHANGE_MAP_TO")
		format(lYes, charsmax(lYes), "%L", LANG_SERVER, "YES")
		format(lNo, charsmax(lNo), "%L", LANG_SERVER, "NO")
		format(menu_msg, charsmax(menu_msg), g_coloredMenus ? "\y%s %s?\w^n^n1.  %s^n2.  %s" : "%s %s?^n^n1.  %s^n2.  %s", lChangeMap, g_optionName[0], lYes, lNo)
		keys = MENU_KEY_1|MENU_KEY_2
		g_yesNoVote = 1
	}
	
	new authid[32], name[MAX_NAME_LENGTH]
	
	get_user_authid(id, authid, charsmax(authid))
	get_user_name(id, name, charsmax(name))
	
	if (argc == 2)
		log_amx("Vote: ^"%s<%d><%s><>^" vote map (map ^"%s^")", name, get_user_userid(id), authid, g_optionName[0])
	else
		log_amx("Vote: ^"%s<%d><%s><>^" vote maps (map#1 ^"%s^") (map#2 ^"%s^") (map#3 ^"%s^") (map#4 ^"%s^")", name, get_user_userid(id), authid, g_optionName[0], g_optionName[1], g_optionName[2], g_optionName[3])

	new msg[256];
	for (new i = 1; i <= MaxClients; i++)
	{
		if (is_user_connected(i) && !is_user_bot(i))
		{
			// HACK: ADMIN_VOTE_MAP_{1,2} keys were designed very poorly.  Remove all : and %s in it.
			LookupLangKey(msg, charsmax(msg), "ADMIN_VOTE_MAP_1", i);
			replace_all(msg, charsmax(msg), "%s", "");
			replace_all(msg, charsmax(msg), ":", "");
			trim(msg);
			show_activity_id(i, id, name, msg);
		}
	}

	g_execResult = true
	new Float:vote_time = get_cvar_float("amx_vote_time") + 2.0
	
	set_cvar_float("amx_last_voting", get_gametime() + vote_time)
	g_voteRatio = get_cvar_float("amx_votemap_ratio")
	g_Answer = "changelevel %s"
	show_menu(0, keys, menu_msg, floatround(vote_time), (g_validMaps > 1) ? "Choose map: " : "Change map to ")
	set_task(vote_time, "checkVotes", 99889988)
	g_voteCaller = id
	console_print(id, "%L", id, "VOTING_STARTED")
	g_voteCount = {0, 0, 0, 0}
	
	return PLUGIN_HANDLED
}

public cmdVote(id, level, cid)
{
	if (!cmd_access(id, level, cid, 4))
		return PLUGIN_HANDLED
	
	new Float:voting = get_cvar_float("amx_last_voting")
	if (voting > get_gametime())
	{
		console_print(id, "%L", id, "ALREADY_VOTING")
		return PLUGIN_HANDLED
	}
	
	if (voting && voting + get_cvar_float("amx_vote_delay") > get_gametime())
	{
		console_print(id, "%L", id, "VOTING_NOT_ALLOW")
		return PLUGIN_HANDLED
	}

	new quest[48]
	read_argv(1, quest, charsmax(quest))
	
	trim(quest);
	
	if (containi(quest, "sv_password") != -1 || containi(quest, "rcon_password") != -1)
	{
		console_print(id, "%L", id, "VOTING_FORBIDDEN")
		return PLUGIN_HANDLED
	}
	
	new count=read_argc();

	for (new i=0;i<4 && (i+2)<count;i++)
	{
		read_argv(i+2, g_optionName[i], charsmax(g_optionName[]));
	}

	new authid[32], name[MAX_NAME_LENGTH]
	
	get_user_authid(id, authid, charsmax(authid))
	get_user_name(id, name, charsmax(name))
	log_amx("Vote: ^"%s<%d><%s><>^" vote custom (question ^"%s^") (option#1 ^"%s^") (option#2 ^"%s^")", name, get_user_userid(id), authid, quest, g_optionName[0], g_optionName[1])

	new msg[256];
	for (new i = 1; i <= MaxClients; i++)
	{
		if (is_user_connected(i) && !is_user_bot(i))
		{
			// HACK: ADMIN_VOTE_CUS_{1,2} keys were designed very poorly.  Remove all : and %s in it.
			LookupLangKey(msg, charsmax(msg), "ADMIN_VOTE_CUS_1", i);
			replace_all(msg, charsmax(msg), "%s", "");
			replace_all(msg, charsmax(msg), ":", "");
			trim(msg);
			show_activity_id(i, id, name, msg);
		}
	}

	new menu_msg[512], lVote[16]
	
	format(lVote, charsmax(lVote), "%L", LANG_SERVER, "VOTE")
	
	count-=2;
	if (count>4)
	{
		count=4;
	}
	// count now shows how many options were listed
	new keys=0;
	for (new i=0;i<count;i++)
	{
		keys |= (1<<i);
	}
	
	new len=formatex(menu_msg, charsmax(menu_msg), g_coloredMenus ? "\y%s: %s\w^n^n" : "%s: %s^n^n", lVote, quest);
	
	for (new i=0;i<count;i++)
	{
		len+=formatex(menu_msg[len], charsmax(menu_msg) - len ,"%d.  %s^n",i+1,g_optionName[i]);
	}
	g_execResult = false
	
	new Float:vote_time = get_cvar_float("amx_vote_time") + 2.0
	
	set_cvar_float("amx_last_voting", get_gametime() + vote_time)
	g_voteRatio = get_cvar_float("amx_vote_ratio")
	replace_all(quest, charsmax(quest), "%", "");
	format(g_Answer, charsmax(g_Answer), "%s - ^"%%s^"", quest)
	show_menu(0, keys, menu_msg, floatround(vote_time), "Vote: ")
	set_task(vote_time, "checkVotes", 99889988)
	g_voteCaller = id
	console_print(id, "%L", id, "VOTING_STARTED")
	g_voteCount = {0, 0, 0, 0}
	g_yesNoVote = 0
	
	return PLUGIN_HANDLED
}

public cmdVoteKickBan(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
		return PLUGIN_HANDLED
	
	new Float:voting = get_cvar_float("amx_last_voting")
	if (voting > get_gametime())
	{
		console_print(id, "%L", id, "ALREADY_VOTING")
		return PLUGIN_HANDLED
	}

	if (voting && voting + get_cvar_float("amx_vote_delay") > get_gametime())
	{
		console_print(id, "%L", id, "VOTING_NOT_ALLOW")
		return PLUGIN_HANDLED
	}

	new cmd[32]
	
	read_argv(0, cmd, charsmax(cmd))
	
	new voteban = equal(cmd, "amx_voteban")
	new arg[32]
	read_argv(1, arg, charsmax(arg))
	
	new player = cmd_target(id, arg, CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF)
	
	if (!player)
		return PLUGIN_HANDLED
	
	if (voteban && is_user_bot(player))
	{
		new imname[MAX_NAME_LENGTH]
		
		get_user_name(player, imname, charsmax(imname))
		console_print(id, "%L", id, "ACTION_PERFORMED", imname)
		return PLUGIN_HANDLED
	}

	new keys = MENU_KEY_1|MENU_KEY_2
	new menu_msg[256], lYes[16], lNo[16], lKickBan[16]
	
	format(lYes, charsmax(lYes), "%L", LANG_SERVER, "YES") 
	format(lNo, charsmax(lNo), "%L", LANG_SERVER, "NO")
	format(lKickBan, charsmax(lKickBan), "%L", LANG_SERVER, voteban ? "BAN" : "KICK")
	ucfirst(lKickBan)
	get_user_name(player, arg, charsmax(arg))
	format(menu_msg, charsmax(menu_msg), g_coloredMenus ? "\y%s %s?\w^n^n1.  %s^n2.  %s" : "%s %s?^n^n1.  %s^n2.  %s", lKickBan, arg, lYes, lNo)
	g_yesNoVote = 1
	
	new bool:ipban=false;
	
	if (voteban)
	{
		get_user_authid(player, g_optionName[0], charsmax(g_optionName[]));
		
		// Do the same check that's in plmenu to determine if this should be an IP ban instead
		if (equal("4294967295", g_optionName[0])
			|| equal("HLTV", g_optionName[0])
			|| equal("STEAM_ID_LAN", g_optionName[0])
			|| equali("VALVE_ID_LAN", g_optionName[0]))
		{
			get_user_ip(player, g_optionName[0], charsmax(g_optionName[]), 1);
			
			ipban=true;
		}

	}
	else
	{
		num_to_str(get_user_userid(player), g_optionName[0], charsmax(g_optionName[]))
	}
	
	new authid[32], name[MAX_NAME_LENGTH]
	
	get_user_authid(id, authid, charsmax(authid))
	get_user_name(id, name, charsmax(name))
	log_amx("Vote: ^"%s<%d><%s><>^" vote %s (target ^"%s^")", name, get_user_userid(id), authid, voteban ? "ban" : "kick", arg)

	new msg[256];
	new right[256];
	new dummy[1];
	for (new i = 1; i <= MaxClients; i++)
	{
		if (is_user_connected(i) && !is_user_bot(i))
		{
			formatex(lKickBan, charsmax(lKickBan), "%L", i, voteban ? "BAN" : "KICK");
			
			// HACK: ADMIN_VOTE_FOR{1,2} keys are really weird.  Tokenize and ignore the text before the :
			LookupLangKey(msg, charsmax(msg), "ADMIN_VOTE_FOR_1", i);
			strtok(msg, dummy, 0, right, charsmax(right), ':');
			trim(right);
			show_activity_id(i, id, name, right, lKickBan, arg);
		}
	}

	g_execResult = true
	
	new Float:vote_time = get_cvar_float("amx_vote_time") + 2.0
	
	set_cvar_float("amx_last_voting", get_gametime() + vote_time)
	g_voteRatio = get_cvar_float(voteban ? "amx_voteban_ratio" : "amx_votekick_ratio")

	if (voteban)
	{
		if (ipban==true)
		{
			g_Answer = "addip 30.0 %s";
		}
		else
		{
			g_Answer = "banid 30.0 %s kick";

		}
	}
	else
	{
		g_Answer = "kick #%s";
	}
	show_menu(0, keys, menu_msg, floatround(vote_time), voteban ? "Ban " : "Kick ")
	set_task(vote_time, "checkVotes", 99889988)
	g_voteCaller = id
	console_print(id, "%L", id, "VOTING_STARTED")
	g_voteCount = {0, 0, 0, 0}
	
	return PLUGIN_HANDLED
}
