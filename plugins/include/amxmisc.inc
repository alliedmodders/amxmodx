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

#if !defined _amxmodx_included
	#include <amxmodx>
#endif

static bool:activity_color, activity_sender;

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
		new hcmd[32], hinfo[128], hflag, bool:info_ml;
		get_concmd(cid, hcmd, charsmax(hcmd), hflag, hinfo, charsmax(hinfo), level, _, info_ml);

		if (info_ml)
		{
			LookupLangKey(hinfo, charsmax(hinfo), hinfo, id);
		}

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
#define CMDTARGET_OBEY_IMMUNITY (1<<0) // Obey immunity
#define CMDTARGET_ALLOW_SELF    (1<<1) // Allow self targeting
#define CMDTARGET_ONLY_ALIVE    (1<<2) // Target must be alive
#define CMDTARGET_NO_BOTS       (1<<3) // Target can't be a bot

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
	if(!get_activity_value())
		return;

	static players[MAX_PLAYERS], pnum;
	get_players(players, pnum, "ch");

	if(!pnum)
		return;

	static buffer[192], prefix[10], player;
	vformat(buffer, charsmax(buffer), fmt, 4);
	get_activity_prefix(id, prefix, charsmax(prefix));

	for(new i; i < pnum; i++)
	{
		player = players[i];

		switch(can_see_admin_name(player))
		{
			case 0: send_activity_message(player, "%L: %s", LANG_PLAYER, prefix, buffer);
			case 1: send_activity_message(player, "%L %s: %s", LANG_PLAYER, prefix, name, buffer);
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
	if(!is_user_connected(idtarget) || !get_activity_value())
		return;

	static buffer[192], prefix[10];
	vformat(buffer, charsmax(buffer), fmt, 5);
	get_activity_prefix(idadmin, prefix, charsmax(prefix));

	switch(can_see_admin_name(idtarget))
	{
		case 0: send_activity_message(idtarget, "%L: %s", idtarget, prefix, buffer);
		case 1: send_activity_message(idtarget, "%L %s: %s", idtarget, prefix, name, buffer);
	}
}

/**
 * Standard method to show activity to all clients with normal language keys.
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
	#pragma unused ___AdminName

	if(!get_activity_value())
		return;

	static players[MAX_PLAYERS], pnum;
	get_players(players, pnum, "ch");

	if(!pnum)
		return;

	static buffer[192], key[192], player;

	for(new i; i < pnum; i++)
	{
		player = players[i];

		switch(can_see_admin_name(player))
		{
			case 0:
			{
				LookupLangKey(key, charsmax(key), KeyWithoutName, player);
				vformat(buffer, charsmax(buffer), key, 4);
				send_activity_message(player, buffer);
			}
			case 1:
			{
				LookupLangKey(key, charsmax(key), KeyWithName, player);
				vformat(buffer, charsmax(buffer), key, 3);
				send_activity_message(player, buffer);
			}
		}
	}
}

/**
 * Displays a chat message that obeys the amx_show_activity cvar to a single client or everybody.
 *
 * @note The admin name needs to be marked with "$an" in the message in
 *       order for it to get replaced. The name will get replaced with
 *       a blank string ("") if the client isn't supposed to see it.
 *
 * @param id        Client index, use 0 to display to all clients
 * @param name      Admin name
 * @param fmt       Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          
 */
stock show_activity_custom(id, const name[], const fmt[], any:...)
{
	if(!get_activity_value())
		return;

	static buffer[192];
	vformat(buffer, charsmax(buffer), fmt, 4);

	if(!id)
	{
		static players[MAX_PLAYERS], pnum;
		get_players(players, pnum);

		if(!pnum)
			return

		static player;

		for(new i; i < pnum; i++)
		{
			player = players[i];

			if(can_see_admin_name(player) != -1)
			{
				replace_activity_data(player, name, buffer, charsmax(buffer));
				send_activity_message(player, buffer);
			}
		}
	}
	else
	{
		if(can_see_admin_name(id) != -1)
		{
			replace_activity_data(id, name, buffer, charsmax(buffer));
			send_activity_message(id, buffer);
		}
	}
}

/**
 * Sends an amx_show_activity based message that obeys the colored chat
 * message rules set by the set_activity_color() function.
 *
 * @param id        Client index to display message to
 * @param fmt       Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @noreturn
 */
stock send_activity_message(id, const fmt[], any:...)
{
	new buffer[192];
	vformat(buffer, charsmax(buffer), fmt, 3)

	if(activity_color)
		client_print_color(id, activity_sender, buffer);
	else
		client_print(id, print_chat, buffer);
}

/**
 * Checks whether or not the client can see the admin name according to the amx_show_activity cvar.
 *
 * @note The valid values of amx_show_activity are:
 *       0 = don't display the message at all
 *       1 = don't show the admin name to anyone
 *       2 = show the admin name to everyone
 *       3 = show the admin name only to admins, hide it from normal users
 *       4 = show the admin name only to admins, show nothing to normal users
 *       5 = hide the admin name only from admins, show nothing to normal users
 *
 * @param id        Client index
 *
 * @return          1 if the client should see the admin name.
 *                  0 if the client shouldn't see the admin name.
 *                  -1 if the client shouldn't see the message at all.
 */
stock can_see_admin_name(id)
{
	switch(get_activity_value())
	{
		case 0: return -1;
		case 1: return 0;
		case 2: return 1;
		case 3: return is_user_admin(id) ? 1 : 0;
		case 4: return is_user_admin(id) ? 1 : -1;
		case 5: return is_user_admin(id) ? 0 : -1;
	}

	return -1;
}

/**
 * Replaces the admin name and client prefix placeholders for show_activity() functions inside a string.
 *
 * @note If the client isn't supposed to see the admin name, the name
 *       will simply get removed by the string (replace with "").
 *
 * @param id            Client index
 * @param name          Admin name that will be replaced
 * @param buffer        Buffer where the name will be replaced
 * @param len           Maximum buffer length
 * @param str_name      String that holds the admin name
 * @param str_prefix    String that holds the client prefix
 *
 * @noreturn
 */
stock replace_activity_data(id, const name[], buffer[], len, str_name[] = ACTIVITY_STR_NAME, str_prefix[] = ACTIVITY_STR_PREFIX)
{
	if(contain(buffer, str_prefix) != -1)
	{
		static prefix[10];
		get_activity_prefix(id, prefix, charsmax(prefix));
		replace_all(buffer, len, str_prefix, prefix);
	}

	static cansee; cansee = can_see_admin_name(id);

	if(cansee != -1)
		replace_all(buffer, len, str_name, cansee ? name : "");
}

/**
 * Sets chat color rules for show_activity() functions.
 *
 * @note The use method of this function is similar to set_hudmessage()
 *       where you need to use the function before using show_hudmessage().
 *       In this case, you need to use set_activity_color() before using any
 *       of the show_activity() functions in order for it to have an effect.
 * @note If you plan to use the same rules all the time, you can use this
 *       function inside of plugin_init() or some other forward. This way you
 *       won't have to write set_activity_color() before every show_activity() function.
 *
 * @param color     Set this to true if you want to use colored chat messages
 * @param sender    Sender id or print_team_* constant
 *
 * @noreturn
 */
stock set_activity_color(bool:color, sender = print_team_default)
{
	activity_color = color;
	activity_sender = sender;
}

/**
 * Returns the integer value of the amx_show_activity cvar.
 * Also prepares the cvar for use with show_activity() functions.
 *
 * @note If the amx_show_activity cvar doesn't exist, it will create a dummy cvar.
 *
 * @return          Cvar value in the range from 0 to 5.
 */
stock get_activity_value()
{
	static activity_pointer;

	if(!activity_pointer)
	{
		activity_pointer = get_cvar_pointer("amx_show_activity");

		if(!activity_pointer)
			activity_pointer = register_cvar("amx_show_activity", "2");
	}

	return clamp(get_pcvar_num(activity_pointer), 0, 5);
}

/**
 * Returns the language key for PLAYER or ADMIN depending on the client's admin status.
 *
 * @param id        Client index
 * @param prefix    Buffer to store the prefix in
 * @param len       Maximum buffer length
 *
 * @noreturn
 */
stock get_activity_prefix(id, prefix[], len)
	copy(prefix, len, is_user_admin(id) ? "ADMIN" : "PLAYER");

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

/**
 * Calls a function after a specified time has elapsed.
 *
 * @param time          Time interval to assign
 * @param function      Function to execute
 * @param id            Task id to assign
 * @param parameter     Data to pass through to callback
 * @param len           Size of data
 * @param flags         Optional flags (enum SetTaskFlags); valid flags are:
 *                        SetTask_Once - Execute callback once (Default)
 *                        SetTask_RepeatTimes - repeat timer a set amount of times
 *                        SetTask_Repeat - loop indefinitely until timer is stopped
 *                        SetTask_AfterMapStart - time interval is treated as absolute
 *                            time after map start
 *                        SetTask_BeforeMapChange - time interval is treated as absolute
 *                            time before map change
 * @param repeat        If the SetTask_RepeatTimes flag is set, the task will be repeated this
 *                      many times
 *
 * @noreturn
 * @error               If an invalid callback function is provided, an error is
 *                      thrown.
 */
stock set_task_ex(Float:time, const function[], id = 0, const any:parameter[] = "", len = 0, SetTaskFlags:flags = SetTask_Once, repeat = 0)
{
	new strFlags[2]; // There should never be a need to set more than 1 flag
	get_flags(_:flags, strFlags, charsmax(strFlags));
	set_task(time, function, id, parameter, len, strFlags, repeat);
}

/**
 * Stores a filtered list of client indexes to an array.
 *
 * @note Example retrieving all alive CTs:
 *       get_players_ex(players, num, GetPlayers_ExcludeDead | GetPlayers_MatchTeam, "CT")
 *
 * @param players   Array to store indexes to
 * @param num       Variable to store number of indexes to
 * @param flags     Optional filtering flags (enum GetPlayersFlags); valid flags are:
 *                    GetPlayers_None - No filter (Default)
 *                    GetPlayers_ExcludeDead - do not include dead clients
 *                    GetPlayers_ExcludeAlive - do not include alive clients
 *                    GetPlayers_ExcludeBots - do not include bots
 *                    GetPlayers_ExcludeHuman - do not include human clients
 *                    GetPlayers_MatchTeam - match with team
 *                    GetPlayers_MatchNameSubstring - match with part of name
 *                    GetPlayers_CaseInsensitive - match case insensitive
 *                    GetPlayers_ExcludeHLTV - do not include HLTV proxies
 *                    GetPlayers_IncludeConnecting - include connecting clients
 * @param team      String to match against if the "e" or "f" flag is specified
 *
 * @noreturn
 */
stock get_players_ex(players[MAX_PLAYERS], &num, GetPlayersFlags:flags = GetPlayers_None, const team[] = "")
{
	new strFlags[10];
	get_flags(_:flags, strFlags, charsmax(strFlags));
	get_players(players, num, strFlags, team);
}
