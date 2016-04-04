// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#if defined _amxmisc_included
	#endinput
#endif
#define _amxmisc_included

#if !defined _fakemeta_included
	#include <fakemeta>
#endif

#if !defined _amxmodx_included
	#include <amxmodx>
#endif

/**
 * Returns if the client has any admin flags set
 *
 * @param id    Client index
 *
 * @return      1 if client has any admin flags, 0 otherwise
 */
stock is_user_admin(id)
{
	new __flags = get_user_flags(id);
	return (__flags > 0 && !(__flags & ADMIN_USER));
}

/**
 * Returns if the user can execute the current command by checking the necessary
 * admin flags and parameter count. Displays a denied access message to the user
 * if missing privileges or a usage example if too few parameters are provided.
 *
 * @note This should be used inside of a command forward as it uses read_argc()
 *       to check the parameter count.
 *
 * @param id            Client index
 * @param level         Required admin flags
 * @param cid           Command id
 * @param num           Required number of parameters
 * @param acesssilent   If true no denied access message will be printed
 *
 * @return              1 if access granted and parameters provided, 0 otherwise
 */
stock cmd_access(id, level, cid, num, bool:accesssilent = false)
{
	new has_access = 0;
	if (id == (is_dedicated_server() ? 0 : 1))
	{
		has_access = 1;
	}
	else if (level == ADMIN_ADMIN)
	{
		if (is_user_admin(id))
		{
			has_access = 1;
		}
	}
	else if (get_user_flags(id) & level)
	{
		has_access = 1;
	}
	else if (level == ADMIN_ALL)
	{
		has_access = 1;
	}

	if (has_access == 0)
	{
		if (!accesssilent)
		{
			console_print(id, "%L", id, "NO_ACC_COM");
		}
		return 0;
	}
	if (read_argc() < num)
	{
		new hcmd[32], hinfo[128], hflag;
		get_concmd(cid, hcmd, charsmax(hcmd), hflag, hinfo, charsmax(hinfo), level);
		console_print(id, "%L:  %s %s", id, "USAGE", hcmd, hinfo);
		return 0;
	}

	return 1;
}

/**
 * Returns if the client has the specified admin flags.
 *
 * @param id        Client index
 * @param level     Required admin flags
 *
 * @return          1 if client has the admin flags, 0 otherwise
 */
stock access(id, level)
{
	if (level == ADMIN_ADMIN)
	{
		return is_user_admin(id);
	}
	else if (level == ADMIN_ALL)
	{
		return 1;
	}

	return (get_user_flags(id) & level);
}

/**
 * cmd_target flags
 */
#define CMDTARGET_OBEY_IMMUNITY       (1<<0) // Obey immunity
#define CMDTARGET_ALLOW_SELF          (1<<1) // Allow self targeting
#define CMDTARGET_ONLY_ALIVE          (1<<2) // Target must be alive
#define CMDTARGET_NO_BOTS             (1<<3) // Target can't be a bot
#define CMDTARGET_NO_SPEC             (1<<4) // Target can't be spectator
#define CMDTARGET_SAFETY              (1<<5) // Allow only safety commands (@aim @bots @me @spec @spectating name)
#define CMDTARGET_NO_MULTIPLE_TARGETS (1<<6) // Allow targeting only one target

/**
 * Processes a generic target pattern and tries to match it to a client based
 * on filtering flags. If no unique target is found an appropriate message is
 * displayed to the admin.
 *
 * @note The pattern is first matched case insensitively against client names.
 *       If no match is found it is matched against client authids. If still no
 *       match is found and the pattern starts with '#' it is finally matched
 *       against client userids.
 * @note Since client names are matched by substring the pattern can potentially
 *       match multiple targets. In that case the function will return 0 and ask
 *       the admin to provide a unique pattern.
 * @note The filtering flags are applied after the pattern matching has
 *       finished. That means the pattern has to be unique against all clients
 *       on the server even if some of them are not eligible.
 *
 * @param id        Client index of admin performing an action
 * @param arg       Target pattern
 * @param flags     Filtering flags, see CMDTARGET_* constants above
 *
 * @return          Client index, or 0 if no or multiple clients matched
 */
stock cmd_target(id, const arg[], flags = CMDTARGET_OBEY_IMMUNITY)
{
	new player = find_player("bl", arg);
	if (player)
	{
		if (player != find_player("blj", arg))
		{
			console_print(id, "%L", id, "MORE_CL_MATCHT");
			return 0;
		}
	}
	else if ((player = find_player("c", arg)) == 0 && arg[0] == '#' && arg[1])
	{
		player = find_player("k", str_to_num(arg[1]));
	}
	if (!player)
	{
		console_print(id, "%L", id, "CL_NOT_FOUND");
		return 0;
	}
	if (flags & CMDTARGET_OBEY_IMMUNITY)
	{
		if ((get_user_flags(player) & ADMIN_IMMUNITY) && ((flags & CMDTARGET_ALLOW_SELF) ? (id != player) : true))
		{
			new imname[MAX_NAME_LENGTH];
			get_user_name(player, imname, charsmax(imname));
			console_print(id, "%L", id, "CLIENT_IMM", imname);
			return 0;
		}
	}
	if (flags & CMDTARGET_ONLY_ALIVE)
	{
		if (!is_user_alive(player))
		{
			new imname[MAX_NAME_LENGTH];
			get_user_name(player, imname, charsmax(imname));
			console_print(id, "%L", id, "CANT_PERF_DEAD", imname);
			return 0;
		}
	}
	if (flags & CMDTARGET_NO_BOTS)
	{
		if (is_user_bot(player))
		{
			new imname[MAX_NAME_LENGTH];
			get_user_name(player, imname, charsmax(imname));
			console_print(id, "%L", id, "CANT_PERF_BOT", imname);
			return 0;
		}
	}

	return player;
}

/**
 * Same as above, but allow a more complex and flexible targeting system.
 * cmd_target allows targeting a single client, if you want to target more use this function.
 * Any special targeting type must be preceded by @, otherwise it will be treated as single-client and will call cmd_target. 
 * New targeting types are: @all, @alive, @aim, @bots, @dead, @humans, @me, @!me, @ct, @t, @spec, @spectating. 
 *
 * @all        : target all players from server. 
 * @alive      : target all alive player from server.
 * @aim        : target the player that the admin is currently looking at.
 * @bots       : target only bots.
 * @dead       : target only dead players. 
 * @humans     : target only human players.
 * @me         : target the admin that write the command. 
 * @!me        : target everyone in the server except the admin that use the command. 
 * @ct         : target anyone from CT team.
 * @t          : target anyone from TERRORIST team.
 * @spec       : target anyone from SPECTATOR team.
 * @spectating : target the player that the admin is currently spectating.
 *
 * @note Unlike cmd_target, here the pattern flags(CMDTARGET_*) are applied before any other check is performed.
 * @note The array is filled only with fully valid players that passed all filterings. It works in get_players style.
 * @note Work with clasic targeting ways: name/ip/authid/userid. This are checked after the special ones(@*).
 * @note Some commands does not work on dead and/or spectator players(like amx_slay). In such case, a message is send to the admin.
 * @note They are case-sensitive.
 *
 * @example new ArrayPlayers[MAX_PLAYERS], Num = cmd_target_ex(params), id; for(new i; i < Num; i++) { id = ArrayPlayers[i]; other_code_here }
 * @example amx_slay @dead is not valid and will throw an error. amx_slay @alive is fully valid.
 *
 * @param id       	        	Client index of admin performing an action
 * @param target    	        Target pattern
 * @param cmd_flags             Filtering flags, see CMDTARGET_* constants above
 * @param players_list          The array that will be filled with all players that match the patter specified 
 * @param size                  The dimension of the array, you can use this to limit the number of players retured
 *
 * @return                      The number of players that are added into array. You need this to loop all players. It will return 0 in case no match is possible
 */
stock cmd_targetex(id, const target[], cmd_flags, players_list[MAX_PLAYERS], lang[] = "", len = -1)
{
	//server_print("cmd_targetex : intra in functie");
	new players[32], flags[10], team[10], bool:parse_players, player, ignored_player, num;
	if(target[0] == '@')	
	{
		//server_print("cmd_targetex : are @");
		switch(target[1])
		{
			case 'a':
			{
				if(equal(target[1], "all"))
				{
					if(cmd_flags & CMDTARGET_NO_MULTIPLE_TARGETS)
					{
						console_print(id, "%l", "CANT_PERF_MULTIPLE");
						return 0;
					}
					if(cmd_flags & CMDTARGET_SAFETY)
					{
						console_print(id, "%l", "CANT_PERF_SAFETY");
						return 0;
					}
					if(cmd_flags & CMDTARGET_ONLY_ALIVE || cmd_flags & CMDTARGET_NO_SPEC)
					{
						copy(flags, charsmax(flags), "a")
					}
					
					if( len > 0 )
					{
						copy(lang, len, "ALL");
					}
					
					parse_players = true;
				}
				else if(equal(target[1], "alive"))
				{
					if(cmd_flags & CMDTARGET_NO_MULTIPLE_TARGETS)
					{
						console_print(id, "%l", "CANT_PERF_MULTIPLE");
						return 0;
					}
					if(cmd_flags & CMDTARGET_SAFETY)
					{
						console_print(id, "%l", "CANT_PERF_SAFETY");
						return 0;
					}
					parse_players = true;
					copy(flags, charsmax(flags), "a");
					
					if( len > 0 )
					{
						copy(lang, len, "ALIVE");
					}
				}
				else if(equal(target[1], "aim"))
				{
					new aim_target, body;
					get_user_aiming(id, aim_target, body);
					
					if(1 <= aim_target <= MaxClients)
					{
						player = aim_target;
					}
				}
			}
			case 'b':
			{
				if(equal(target[1], "bots"))
				{
					if(cmd_flags & CMDTARGET_NO_MULTIPLE_TARGETS)
					{
						console_print(id, "%l", "CANT_PERF_MULTIPLE");
						return 0;
					}
					parse_players = true;
					copy(flags, charsmax(flags), "d");
					if( len > 0 )
					{
						copy(lang, len, "BOTS");
					}
				}
			}
			case 'd':
			{
				if(!(cmd_flags & CMDTARGET_ONLY_ALIVE) && equal(target[1], "dead"))
				{
					if(cmd_flags & CMDTARGET_NO_MULTIPLE_TARGETS)
					{
						console_print(id, "%l", "CANT_PERF_MULTIPLE");
						return 0;
					}
					if(cmd_flags & CMDTARGET_SAFETY)
					{
						console_print(id, "%l", "CANT_PERF_SAFETY");
						return 0;
					}
					parse_players = true;
					copy(flags, charsmax(flags), "b");
					if( len > 0 )
					{
						copy(lang, len, "DEAD");
					}
				}
			}
			case 'h':
			{
				if(equal(target[1], "humans"))
				{
					if(cmd_flags & CMDTARGET_NO_MULTIPLE_TARGETS)
					{
						console_print(id, "%l", "CANT_PERF_MULTIPLE");
						return 0;
					}
					if(cmd_flags & CMDTARGET_SAFETY)
					{
						console_print(id, "%l", "CANT_PERF_SAFETY");
						return 0;
					}
					parse_players = true;
					copy(flags, charsmax(flags), "c");
					if( len > 0 )
					{
						copy(lang, len, "HUMANS");
					}
				}
			}
			case 'm':
			{
				if(equal(target[1], "me"))
				{
					player = id;
				}
			}
			case '!':
			{
				if(equal(target[1], "!me"))
				{
					if(cmd_flags & CMDTARGET_NO_MULTIPLE_TARGETS)
					{
						console_print(id, "%l", "CANT_PERF_MULTIPLE");
						return 0;
					}
					if(cmd_flags & CMDTARGET_SAFETY)
					{
						console_print(id, "%l", "CANT_PERF_SAFETY");
						return 0;
					}
					parse_players = true;
					ignored_player = id;
					if( len > 0 )
					{
						copy(lang, len, "NOT_ME");
					}
				}
			}
			case 'c':
			{
				if(equal(target[1], "ct"))
				{
					if(cmd_flags & CMDTARGET_NO_MULTIPLE_TARGETS)
					{
						console_print(id, "%l", "CANT_PERF_MULTIPLE");
						return 0;
					}
					if(cmd_flags & CMDTARGET_SAFETY)
					{
						console_print(id, "%l", "CANT_PERF_SAFETY");
						return 0;
					}
					parse_players = true;
					copy(flags, charsmax(flags), "e");
					copy(team, charsmax(team), "CT");
					if( len > 0 )
					{
						copy(lang, len, "CT_TEAM");
					}
				}
			}
			case 't':
			{
				if( equal(target[1], "t"))
				{
					if(cmd_flags & CMDTARGET_NO_MULTIPLE_TARGETS)
					{
						console_print(id, "%l", "CANT_PERF_MULTIPLE");
						return 0;
					}
					if(cmd_flags & CMDTARGET_SAFETY)
					{
						console_print(id, "%l", "CANT_PERF_SAFETY");
						return 0;
					}
					parse_players = true;
					copy(flags, charsmax(flags), "e");
					copy(team, charsmax(team), "TERRORIST");
					if( len > 0 )
					{
						copy(lang, len, "T_TEAM");
					}
				}
			}
			case 's':
			{
				if(!(cmd_flags & CMDTARGET_NO_SPEC) && equal(target[1], "spec"))
				{
					if(cmd_flags & CMDTARGET_NO_MULTIPLE_TARGETS)
					{
						console_print(id, "%l", "CANT_PERF_MULTIPLE");
						return 0;
					}
					parse_players = true;
					copy(flags, charsmax(flags), "e");
					copy(team, charsmax(team), "SPECTATOR");
					if( len > 0 )
					{
						copy(lang, len, "SPECTATORS");
					}
				}
				else if(equal(target[1], "spectating"))
				{
					new spectated_player = pev(id, pev_iuser2);
					if(1 <= spectated_player <= MaxClients)
					{
						player = spectated_player;
					}
				} 
			}
		}
	}
	
	if(parse_players)
	{
		//server_print("cmd_targetex : parseaza");
		if(flags[0] != EOS)
		{
			if(team[0] != EOS)
			{
				get_players(players, num, flags, team);
			}
			else 
			{
				get_players(players, num, flags);
			}
		}
		else 
		{
			get_players(players, num);
		}
		
		new entry;
		for(new i; i < num; i++)
		{
			player = players[i];

			if(ignored_player != player)
			{
				if(cmd_flags & CMDTARGET_OBEY_IMMUNITY)
				{
					if((get_user_flags(player) & ADMIN_IMMUNITY) && ((cmd_flags & CMDTARGET_ALLOW_SELF) ? (id != player) : true))
					{
						continue;
					}
				}
				
				if(entry < charsmax(players_list))
				{
					players_list[entry++] = player;
				}
			}
		}
		return entry;
	}
	else 
	{
		//server_print("cmd_targetex : nu parseaza");
		if(!player)
		{
			//server_print("cmd_targetex : cmd_target");
			player = cmd_target(id, target, cmd_flags);
			
			if(!player)
			{
				return 0;
			}
		}
		players_list[0] = player;
		return 1;
	}
}

/**
 * Standard method to show admin activity to clients connected to the server.
 * This depends on the amx_show_activity cvar. See documentation for more details.
 *
 * @param id     Client index performing the action
 * @param name   Name of client performing the action
 * @param fmt    Formatting rules
 * @param ...    Variable number of formatting parameters
 *
 * @noreturn
 */
stock show_activity(id, const name[], const fmt[], any:...)
{
	static __amx_show_activity;
	if (__amx_show_activity == 0)
	{
		__amx_show_activity = get_cvar_pointer("amx_show_activity");

		// if still not found, then register the cvar as a dummy
		if (__amx_show_activity == 0)
		{
			__amx_show_activity = register_cvar("amx_show_activity", "2", FCVAR_PROTECTED);
		}
	}

	new prefix[10];
	if (is_user_admin(id))
	{
		copy(prefix, charsmax(prefix), "ADMIN");
	}
	else
	{
		copy(prefix, charsmax(prefix), "PLAYER");
	}
	new buffer[512];
	vformat(buffer, charsmax(buffer), fmt, 4);

	switch (get_pcvar_num(__amx_show_activity))
	{
		case 5: // hide name only to admins, show nothing to normal users
		{
			for (new i = 1; i <= MaxClients; i++)
			{
				if (is_user_connected(i))
				{
					if (is_user_admin(i))
					{
						client_print(i, print_chat, "%L: %s", i, prefix, buffer);
					}
				}
			}
		}
		case 4: // show name only to admins, show nothing to normal users
		{
			for (new i = 1; i <= MaxClients; i++)
			{
				if (is_user_connected(i))
				{
					if (is_user_admin(i))
					{
						client_print(i, print_chat, "%L %s: %s", i, prefix, name, buffer);
					}
				}
			}
		}
		case 3: // show name only to admins, hide name from normal users
		{
			for (new i = 1; i <= MaxClients; i++)
			{
				if (is_user_connected(i))
				{
					if (is_user_admin(i))
					{
						client_print(i, print_chat, "%L %s: %s", i, prefix, name, buffer);
					}
					else
					{
						client_print(i, print_chat, "%L: %s", i, prefix, buffer);
					}
				}
			}
		}
		case 2: // show name to all
		{
			client_print(0, print_chat, "%L %s: %s", LANG_PLAYER, prefix , name , buffer);
		}
		case 1: // hide name to all
		{
			client_print(0, print_chat, "%L: %s", LANG_PLAYER, prefix, buffer);
		}
	}
}

/**
 * Standard method to show admin activity to a single client.
 * This depends on the amx_show_activity cvar. See documentation for more details.
 *
 * @param idtarget  Client index to display message to
 * @param id        Client index performing the action
 * @param name      Name of client performing the action
 * @param fmt       Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @noreturn
 */
stock show_activity_id(idtarget, idadmin, const name[], const fmt[], any:...)
{
	if (idtarget == 0 || !is_user_connected(idtarget))
	{
		return;
	}

	static __amx_show_activity;
	if (__amx_show_activity == 0)
	{
		__amx_show_activity = get_cvar_pointer("amx_show_activity");

		// if still not found, then register the cvar as a dummy
		if (__amx_show_activity == 0)
		{
			__amx_show_activity = register_cvar("amx_show_activity", "2", FCVAR_PROTECTED);
		}
	}

	static prefix[10];
	if (is_user_admin(idadmin))
	{
		copy(prefix, charsmax(prefix), "ADMIN");
	}
	else
	{
		copy(prefix, charsmax(prefix), "PLAYER");
	}

	static buffer[512];
	vformat(buffer, charsmax(buffer), fmt, 5);

	switch (get_pcvar_num(__amx_show_activity))
	{
		case 5: // hide name only to admins, show nothing to normal users
		{
			if (is_user_admin(idtarget))
			{
				client_print(idtarget, print_chat, "%L: %s", idtarget, prefix, buffer);
			}
		}
		case 4: // show name only to admins, show nothing to normal users
		{
			if (is_user_admin(idtarget))
			{
				client_print(idtarget, print_chat, "%L %s: %s", idtarget, prefix, name, buffer);
			}
		}
		case 3: // show name only to admins, hide name from normal users
		{
			if (is_user_admin(idtarget))
			{
				client_print(idtarget, print_chat, "%L %s: %s", idtarget, prefix, name, buffer);
			}
			else
			{
				client_print(idtarget, print_chat, "%L: %s", idtarget, prefix, buffer);
			}
		}
		case 2: // show name to all
		{
			client_print(idtarget, print_chat, "%L %s: %s", idtarget, prefix, name, buffer);
		}
		case 1: // hide name to all
		{
			client_print(idtarget, print_chat, "%L: %s", idtarget, prefix, buffer);
		}
	}
}

/**
 * Standard method to show activity to one single client with normal language keys.
 * These keys need to be in the format of standard AMXX keys:
 *   eg: ADMIN_KICK_1 = ADMIN: kick %s
 *       ADMIN_KICK_2 = ADMIN %s: kick %s
 * This depends on the amx_show_activity cvar.  See documentation for more details.
 *
 * @param KeyWithoutName   The language key that does not have the name field.
 * @param KeyWithName      The language key that does have the name field.
 * @param __AdminName      The name of the person doing the action.
 * @extra                  Pass any extra format arguments for the language key in the variable arguments list.
 *
 * @noreturn
 */
stock show_activity_key(const KeyWithoutName[], const KeyWithName[], const ___AdminName[], any:...)
{
// The variable gets used via vformat, but the compiler doesn't know that, so it still cries.
#pragma unused ___AdminName
	static __amx_show_activity;
	if (__amx_show_activity == 0)
	{
		__amx_show_activity = get_cvar_pointer("amx_show_activity");

		// if still not found, then register the cvar as a dummy
		if (__amx_show_activity == 0)
		{
			__amx_show_activity = register_cvar("amx_show_activity", "2", FCVAR_PROTECTED);
		}
	}

	new buffer[512];
	new keyfmt[256];
	new i;

	switch (get_pcvar_num(__amx_show_activity))
	{
		case 5: // hide name to admins, display nothing to normal players
		{
			while (i++ < MaxClients)
			{
				if (is_user_connected(i))
				{
					if (is_user_admin(i))
					{
						LookupLangKey(keyfmt, charsmax(keyfmt), KeyWithoutName, i);

						// skip the "adminname" argument if not showing name
						vformat(buffer, charsmax(buffer), keyfmt, 4);
						client_print(i, print_chat, "%s", buffer);
					}
				}
			}
		}
		case 4: // show name only to admins, display nothing to normal players
		{
			while (i++ < MaxClients)
			{
				if (is_user_connected(i))
				{
					if (is_user_admin(i))
					{
						LookupLangKey(keyfmt, charsmax(keyfmt), KeyWithName, i);
						vformat(buffer, charsmax(buffer), keyfmt, 3);
						client_print(i, print_chat, "%s", buffer);
					}
				}
			}
		}
		case 3: // show name only to admins, hide name from normal users
		{
			while (i++ < MaxClients)
			{
				if (is_user_connected(i))
				{
					if (is_user_admin(i))
					{
						LookupLangKey(keyfmt, charsmax(keyfmt), KeyWithName, i);
						vformat(buffer, charsmax(buffer), keyfmt, 3);
					}
					else
					{
						LookupLangKey(keyfmt, charsmax(keyfmt), KeyWithoutName, i);

						// skip the "adminname" argument if not showing name
						vformat(buffer, charsmax(buffer), keyfmt, 4);
					}
					client_print(i, print_chat, "%s", buffer);
				}
			}
		}
		case 2: // show name to all users
		{
			while (i++ < MaxClients)
			{
				if (is_user_connected(i))
				{
					LookupLangKey(keyfmt, charsmax(keyfmt), KeyWithName, i);
					vformat(buffer, charsmax(buffer), keyfmt, 3);
					client_print(i, print_chat, "%s", buffer);
				}
			}
		}
		case 1: // hide name from all users
		{
			while (i++ < MaxClients)
			{
				if (is_user_connected(i))
				{
					LookupLangKey(keyfmt, charsmax(keyfmt), KeyWithoutName, i);

					// skip the "adminname" argument if not showing name
					vformat(buffer, charsmax(buffer), keyfmt, 4);
					client_print(i, print_chat, "%s", buffer);
				}
			}
		}
	}
}

/**
 * Returns if the mod running on the server supports colored menus.
 *
 * @note The full list of mods supporting colored menus:
 *       Counter-Strike, Counter-Strike: Condition Zero, Deathmatch Classic,
 *       Day of Defeat, Team Fortress Classic and Half-Life: Deathmatch.
 * @note Since this is a stock and compiled into the plugin, the list of
 *       supported mods will not update and require recompilation of the plugin
 *       if the list ever changed.
 *
 * @return  1 if colored menus are supported, 0 otherwise
 */
stock colored_menus()
{
	static ColoredMenus = -1;

	if (ColoredMenus == -1)
	{
		new const ModNames[][] = { "cstrike", "czero", "dmc", "dod", "tfc", "valve" };
		new ModName[32];

		get_modname(ModName, charsmax(ModName));

		for (new Iterator = 0; Iterator < sizeof(ModNames); Iterator++)
		{
			if (equal(ModName, ModNames[Iterator]))
			{
				ColoredMenus = 1;

				break;
			}
		}

		if (ColoredMenus == -1)
			ColoredMenus = 0;
	}

	return ColoredMenus;
}

/**
 * Returns if the mod running on the server is a version of Counter-Strike.
 *
 * @return  1 if mod is Counter-Strike, 0 otherwise
 */
stock cstrike_running()
{
	new mod_name[32];
	get_modname(mod_name, charsmax(mod_name));

	return (equal(mod_name, "cstrike") || equal(mod_name, "czero") || equal(mod_name, "csv15") || equal(mod_name, "cs13"));
}

/**
 * Returns if the server is running a specific mod.
 *
 * @param mod   Mod name to check for
 *
 * @return      1 if mod name matches, 0 otherwise
 */
stock is_running(const mod[])
{
	new mod_name[32];
	get_modname(mod_name, charsmax(mod_name));

	return equal(mod_name, mod);
}

/**
 * Retrieves the path to the AMXX base directory.
 *
 * @param name  Buffer to copy path to
 * @param len   Maximum buffer size
 *
 * @return      Number of cells written to buffer
 */
stock get_basedir(name[], len)
{
	return get_localinfo("amxx_basedir", name, len);
}

/**
 * Retrieves the path to the AMXX configs directory.
 *
 * @param name  Buffer to copy path to
 * @param len   Maximum buffer size
 *
 * @return      Number of cells written to buffer
 */
stock get_configsdir(name[], len)
{
	return get_localinfo("amxx_configsdir", name, len);
}

/**
 * Retrieves the path to the AMXX data directory.
 *
 * @param name  Buffer to copy path to
 * @param len   Maximum buffer size
 *
 * @return      Number of cells written to buffer
 */
stock get_datadir(name[], len)
{
	return get_localinfo("amxx_datadir", name, len);
}

/**
 * Provides a shorthand to register a working menu.
 *
 * @note Combines the necessary calls to register_menuid() and
 *       register_menucmd() into a single function.
 *
 * @param title     Menu name
 * @param keys      Key flags
 * @param function  Callback function
 * @param outside   Catch menus outside the calling plugin
 *
 * @noreturn
 * @error           If an invalid callback function is specified, an error will
 *                  be thrown.
 */
stock register_menu(const title[], keys, const function[], outside = 0)
{
	register_menucmd(register_menuid(title, outside), keys, function);
}

/**
 * Alias to get_configsdir provided for backwards compatibility. Originally
 * intended to retrieve the AMXX custom directory.
 *
 * @deprecated Should not be used as the concept of a custom directory does no
 *             longer exists in AMXX.
 *
 * @param name  Buffer to copy path to
 * @param len   Maximum buffer size
 *
 * @return      Number of cells written to buffer
 */
#pragma deprecated The concept of a custom directory no longer exists in AMXX. Do not use.
stock get_customdir(name[], len)
{
	return get_configsdir(name, len);
}

/**
 * Adds a menu item/command to the admin menu (amxmodmenu) handled by the
 * "Menus Front-End" plugin, if it is loaded.
 *
 * @param MENU_TEXT     Item text that will be displayed in the menu
 * @param MENU_CMD      Command that will be executed on the client
 * @param MENU_ACCESS   Admin access required for menu command
 * @param MENU_PLUGIN   Case-insensitive name or filename of plugin providing
 *                      the menu command
 *
 * @noreturn
 */
stock AddMenuItem(const MENU_TEXT[], const MENU_CMD[], const MENU_ACCESS, const MENU_PLUGIN[])
{
	AddMenuItem_call(MENU_TEXT, MENU_CMD, MENU_ACCESS, MENU_PLUGIN, false);
}

/**
 * Adds a menu item/command to the client menu (amx_menu) handled by the
 * "Menus Front-End" plugin, if it is loaded. Items should be accessible by
 * non-admins.
 *
 * @param MENU_TEXT     Item text that will be displayed in the menu
 * @param MENU_CMD      Command that will be executed on the client
 * @param MENU_ACCESS   Admin access required for menu command
 * @param MENU_PLUGIN   Case-insensitive name or filename of plugin providing
 *                      the menu command
 *
 * @noreturn
 */
stock AddClientMenuItem(const MENU_TEXT[], const MENU_CMD[], const MENU_ACCESS, const MENU_PLUGIN[])
{
	AddMenuItem_call(MENU_TEXT, MENU_CMD, MENU_ACCESS, MENU_PLUGIN, true);
}

/**
 * Helper function used by AddMenuItem() and AddClientMenuItem()
 *
 * @param MENU_TEXT             Item text that will be displayed in the menu
 * @param MENU_CMD              Command that will be executed on the client
 * @param MENU_ACCESS           Admin access required for menu command
 * @param MENU_PLUGIN           Case-insensitive name or filename of plugin
 *                              providing the menu command
 * @param ADD_TO_CLIENT_MENU    If true adds command to client menu, false adds
 *                              to admin menu
 *
 * @noreturn
 */
stock AddMenuItem_call(const MENU_TEXT[], const MENU_CMD[], const MENU_ACCESS, const MENU_PLUGIN[], const bool:ADD_TO_CLIENT_MENU)
{
	new pluginid = is_plugin_loaded("Menus Front-End");
	if (pluginid == -1)
	{
		log_amx("Can't add menu item ^"%s^" from plugin ^"%s^" to menu set because the Menus Front-End plugin itself is not loaded!", MENU_TEXT, MENU_PLUGIN);
		return; // Menus Front-End doesn't exist, return.
	}

	new filename[64], b[1];
	get_plugin(pluginid, filename, charsmax(filename), b, charsmax(b), b, charsmax(b), b, charsmax(b), b, charsmax(b));

	new status = callfunc_begin(ADD_TO_CLIENT_MENU ? "AddClientMenu" : "AddMenu", filename);
	new bool:failed = true;
	switch (status)
	{
		case 1:
		{
			failed = false;
		}
		case 0:
		{
			log_amx("Run time error! (AddMenuItem_call failed)");
		}
		case -2:
		{
			log_amx("Function not found! (AddMenuItem_call failed)");
		}
		case -1:
		{
			log_amx("Plugin not found! (AddMenuItem_call failed)");
		}
	}
	if (failed)
	{
		return;
	}
	// Item text
	callfunc_push_str(MENU_TEXT);
	// Cmd
	callfunc_push_str(MENU_CMD);
	// Access
	callfunc_push_int(MENU_ACCESS);
	// Menu exists in this plugin
	callfunc_push_str(MENU_PLUGIN);

	callfunc_end();
}

/**
 * Computes an offset from a given value while constraining it between the
 * specified bounds, rolling over if necessary.
 *
 * @note Example: The range is 1-5 and the base value (seed) is 3, the offset
 *       that the value should be moved by is also 3. Offsetting the value by 3
 *       would result in 6, but it is to be constrained between 1 and 5. With
 *       clamp() this would result in 5, but this function rolls the value over
 *       and returns 1 instead.
 *
 * @param low       Lower bound
 * @param high      Higher bound
 * @param seed      Base value
 * @param offset    Offset to move
 *
 * @return          Computed offset value between specified bounds
 */
stock constraint_offset(low, high, seed, offset)
{
	new numElements = high - low + 1;
	offset += seed - low;

	if (offset >= 0)
	{
		return low + (offset % numElements);
	}
	else
	{
		return high - (abs(offset) % numElements) + 1;
	}

	return 0;	// Makes the compiler happy -_-
}

/**
 * Returns if the client has any of the specified admin flags.
 *
 * @param id      Client index
 * @param flags   Flag string
 *
 * @return        1 if the user has any of the specified flags, 0 otherwise
 */
stock has_flag(id, const flags[])
{
	return (get_user_flags(id) & read_flags(flags));
}

/**
 * Returns if the client has all of the specified admin flags.
 *
 * @param id      Client index
 * @param flags   Flag string
 *
 * @return        1 if the user has all of the specified flags, 0 otherwise
 */
stock has_all_flags(id, const flags[])
{
	new FlagsNumber = read_flags(flags);
	return ((get_user_flags(id) & FlagsNumber) == FlagsNumber);
}

/**
 * Resets the client's menu.
 *
 * @note This is a wrapper around show_menu() for the sake of readability.
 *
 * @param index     Client to reset menu of, 0 to reset all clients
 *
 * @noreturn
 */
stock reset_menu(index)
{
	show_menu(index, 0, "", 0);
}
