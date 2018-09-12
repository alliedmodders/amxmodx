// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Plugin Cvar and Command Menu
//

#include <amxmodx>
#include <amxmisc>

new g_disabled_callback;
new g_enabled_callback;

// pcvar that the client is currently modifying
new g_current_cvar[MAX_PLAYERS + 1];

// Name of the cvar being modified
new g_current_cvar_name[MAX_PLAYERS + 1][32];

// Plugin ID that the client is modifying
new g_current_player_id[MAX_PLAYERS + 1];

// Page that the client is currently on
new g_current_page[MAX_PLAYERS + 1];

// Menu function ID that the client is in
new g_current_menu_function[MAX_PLAYERS + 1] = { -1, ... };

new g_current_command[MAX_PLAYERS + 1][32];
new g_cvarmenu_cmdid;
new g_cmdmenu_cmdid;

new g_explicit_plugin[MAX_PLAYERS + 1];

public plugin_init()
{
	register_plugin("Plugin Menu", AMXX_VERSION_STR, "AMXX Dev Team");
	
	register_dictionary("common.txt");
	register_dictionary("pausecfg.txt"); // Needed for PAUSE_COULDNT_FIND
	
	g_cvarmenu_cmdid = register_clcmd("amx_plugincvarmenu", "CvarMenuCommand", ADMIN_CVAR, " - displays the plugin cvar menu");
	g_cmdmenu_cmdid = register_clcmd("amx_plugincmdmenu", "CommandMenuCommand", ADMIN_MENU, " - displays the plugin command menu");
	
	register_clcmd("amx_changecvar", "CommandChangeCvar");
	register_clcmd("amx_executecmd", "CommandExecuteCommand");
	
	// Register global menu callbacks.
	g_disabled_callback = menu_makecallback("AlwaysDisableCallback");
	g_enabled_callback = menu_makecallback("AlwaysEnableCallback");
}

// Add these menus to the amxmodmenu
public plugin_cfg()
{
	set_task(0.1, "addToMenuFront");
}
public addToMenuFront()
{
	new plugin_filename[64];
	
	get_plugin(-1, plugin_filename, charsmax(plugin_filename));
	new cvarflags;
	new cmdflags;
	new garbage[1];
	new cmd[32];

	get_concmd(g_cmdmenu_cmdid, cmd, charsmax(cmd), cmdflags, garbage, charsmax(garbage), -1);

	if (strcmp(cmd, "amx_plugincmdmenu") != 0)
	{
		// this should never happen, but just incase!
		cmdflags = ADMIN_MENU;
	}
	get_concmd(g_cvarmenu_cmdid, cmd, charsmax(cmd), cvarflags, garbage, charsmax(garbage), -1);

	if (strcmp(cmd, "amx_plugincvarmenu") != 0)
	{
		// this should never happen, but just incase!
		cvarflags = ADMIN_CVAR;
	}

	AddMenuItem("Plugin Cvars", "amx_plugincvarmenu", cvarflags, plugin_filename);
	AddMenuItem("Plugin Commands", "amx_plugincmdmenu", cmdflags, plugin_filename);
}

// Reset all fields for each client as they connect.
public client_connect(id)
{
	g_current_cvar[id] = 0;
	g_current_player_id[id] = 0;
	g_current_menu_function[id] = -1;
	g_current_cvar_name[id][0] = 0;
	g_current_command[id][0] = 0;
	g_explicit_plugin[id] = -1;
	
}

/**
 * Creates a plugin list menu.
 *
 * @param menu_text		The text to display as the title.
 * @param Handler		The function to call when an item is selected.
 * @param Command		The function to pass to the handler.  It will be passed as "PLID Command".
 * @param Callback		Function to call for each plugin to be listed.  Displays a number next to it (how many cvars, etc.)
 */
stock DisplayPluginMenu(id, const menu_text[], const Handler[], const Command[], const Callback[])
{
	new menu=menu_create(menu_text, Handler);
	
	
	new plugin_state[32];
	new plugin_name[64];
	new func = get_func_id(Callback);
	new tally;
	new plugincmd[64];
	new menu_text[64];
	for (new i = 0, max = get_pluginsnum();
		 i < max;
		 i++)
	{
		if (callfunc_begin_i(func, -1) == 1)
		{
			callfunc_push_int(i); // push the plid
			if ((tally = callfunc_end()) > 0)
			{
				get_plugin(i, "", 0, plugin_name, charsmax(plugin_name), "", 0, "", 0, plugin_state, charsmax(plugin_state));
						
				// Command syntax is: "# Function", # being plugin ID, function being public function to call.
				formatex(plugincmd, charsmax(plugincmd), "%d %s", i, Command);
				formatex(menu_text, charsmax(menu_text), "%s - %d", plugin_name, tally);
				// If the plugin is running, add this as an activated menu item.
				if (strcmp(plugin_state, "running", true) == 0 ||
					strcmp(plugin_state, "debug", true) == 0)
				{
					menu_additem(menu, menu_text, plugincmd, g_enabled_callback);
				}
				else
				{
					menu_additem(menu, menu_text, "", _, g_disabled_callback);
				}
			}
		}
	}

	menu_setprop(menu, MPROP_NUMBER_COLOR, "\y");
	menu_setprop(menu, MPROP_EXIT, MEXIT_ALL);
	menu_display(id, menu, 0);

}

/**
 * Byrefs the plugin id of a target plugin (passed by argv(1)), but only if it's valid.
 *
 * @param id			id of the display messages to upon failure.
 * @param plid			Variable to byref the plugin id.
 * @return				True on successful lookup, false on failure.
 */
stock bool:GetPlidForValidPlugins(id, &plid)
{
	// If arguments have been passed, then we were given
	// a specific plugin to examine.
	if (read_argc() > 1)
	{ 
		// Yes, we were provided a plugin.
		new target_plugin[64];
		read_argv(1, target_plugin, charsmax(target_plugin));
		
		new buffer_name[64];
		new buffer_file[64];
		new buffer_state[64];
		// Scan for the plugin ID.
		for (new i = 0, max = get_pluginsnum();
			 i < max;
			 i++)
		{
			get_plugin(i, buffer_file, charsmax(buffer_file), buffer_name, charsmax(buffer_name), "", 0, "", 0, buffer_state, charsmax(buffer_state));
			
			if (strcmp(buffer_file, target_plugin, true) != 0 ||
				strcmp(buffer_name, target_plugin, true) != 0)
			{
				// We have a match.
				
				// Check the status of the plugin.  If it's anything other than "running" or "debug" fail.
				if (strcmp(buffer_state, "running") != 0 &&
					strcmp(buffer_state, "debug")   != 0)
				{
					// TODO: ML This
					console_print(id, "Plugin ^"%s^" is not running.", buffer_file);
					// Return a failed state.
					return false;
				}
				plid = i;
				break;
			}
		}
		
		// If the plugin was not found, then tell them there was an error.
		if (plid == -1)
		{
			console_print(id, "%L", id, "PAUSE_COULDNT_FIND", target_plugin);
			
			// return a failure state
			return false;
		}
	}
	
	return true;
}

/**
 * Returns the number of cvars available for a plugin by plid. (Callback for the plugin menu.)
 *
 * @return 				number of cvars in the plugin.
 */
public GetNumberOfCvarsForPlid(plid)
{
	new count = 0;
	new cvar_plid;
	for (new i = 0, max = get_plugins_cvarsnum();
		 i < max;
		 i++)
	{
		get_plugins_cvar(i, "", 0, _, cvar_plid, _);
		
		if (cvar_plid == plid)
		{
			count++;
		}
	}
	
	return count;
}
/**
 * Returns the number of commands available for a plugin by plid. (Callback for the plugin menu.)
 *
 * @return				Number of valid commands in the plugin.
 */
public GetNumberOfCmdsForPlid(plid)
{
	new count = 0;
	
	for (new i = 0, max = get_concmdsnum(-1,-1);
		 i < max;
		 i++)
	{
		if (get_concmd_plid(i, -1, -1) == plid)
		{
			count++;
		}
	}
	
	return count;
}

/**
 * Whether or not the client has access to modify this cvar.
 *
 * @param id			The admin id.
 * @param cvar			The name of the cvar to be checked.
 * @return				True if the client has access, false otherwise.
 */
stock bool:CanIModifyCvar(id, const cvar[])
{
	new user_flags = get_user_flags(id);
	// If the user has rcon access don't bother checking anything.
	if (user_flags & ADMIN_RCON)
	{
		return true;
	}
	
	// If the cvar is "sv_password" (somehow), then check access.
	if (equali(cvar, "sv_password") && user_flags & ADMIN_PASSWORD)
	{
		return true;
	}
	
	// Check to see if the cvar is flagged as protected.
	if (get_cvar_flags(cvar) & FCVAR_PROTECTED)
	{
		// non-rcon user trying to modify a protected cvar.
		return false;
	}
	
	// All known checks done, they can change this cvar if they
	// were able to open the menu.
	return true;
}

/**
 * Simple function to ensure that a menu item is always disabled.
 *
 * All parameters are dummy, nothing is used.
 */
public AlwaysDisableCallback(playerid, menuid, itemid)
{
	return ITEM_DISABLED;
}
/**
 * Simple function to ensure that a menu item is always enabled.
 *
 * All parameters are dummy, nothing is used.
 */
public AlwaysEnableCallback(playerid, menuid, itemid)
{
	return ITEM_ENABLED;
}
/**
 * Handler for the plugin menu.
 *
 * @param id		The client selecting an item.
 * @param menu		The menu handle.
 * @param item		The item number that was selected.
 */
public PluginMenuSelection(id, menu, item)
{
	if (item == MENU_EXIT)
	{
		menu_destroy(menu);
	}
	if (item < 0)
	{
		return PLUGIN_HANDLED;
	}
	
	new command[64];
	new dummy[1];
	
	// All of the commands set for each item is the public
	// function that we want to call after the item is selected.
	// The parameters are: function(idPlayer,itemnumber)
	// Note the menu is destroyed BEFORE the command
	// gets executed.
	// The command retrieved is in the format: "PLID Command"
	menu_item_getinfo(menu, item, dummy[0], command, charsmax(command), dummy, 0, dummy[0]);
	
	
	new plid = str_to_num(command);
	new function[32];
	
	for (new i = 0; i < charsmax(command); i++)
	{
		if (command[i] == ' ')
		{
			// we're at the break. move up one space.
			i++;
			copy(function, charsmax(function), command[i]);
			break;
		}
	}
	
	menu_destroy(menu);
	
	new funcid = get_func_id(function);
	if (funcid != -1 && callfunc_begin_i(funcid) == 1)
	{
		g_current_page[id] = 0;
		g_current_player_id[id] = plid;
		g_current_menu_function[id] = funcid;
		callfunc_push_int(id);
		callfunc_push_int(plid);
		callfunc_push_int(0);
		callfunc_end();
		
	}
	return PLUGIN_HANDLED;
}

/**
 * The command to change a cvar has been called.
 *
 * @param id		The client who is changing the cvar.
 */
public CommandChangeCvar(id)
{
	// All access checks are done before this command is called.
	// So if the client has no pcvar pointer in his array slot
	// then just ignore the command.
	if (g_current_cvar[id] == 0)
	{
		return PLUGIN_CONTINUE;
	}
	
	new args[256];
	
	read_args(args, charsmax(args));
	
	remove_quotes(args);
	
	if (equali(args, "!cancel", 7))
	{
		// The client didn't want to change this cvar.
		client_print(id, print_chat, "[AMXX] Cvar not changed.");
	}
	else
	{
		// Changed to set_cvar_* for 1.76 tests
		
		new pointer = g_current_cvar[id];
		set_pcvar_string(g_current_cvar[id], args);
		
		client_print(id, print_chat, "[AMXX] Cvar ^"%s^" changed to ^"%s^"", g_current_cvar_name[id], args);
		
		// Copy of admincmd's global output.
		
		new name[MAX_NAME_LENGTH];
		new authid[40];
		
		get_user_name(id, name, charsmax(name));
		get_user_authid(id, authid, charsmax(authid));
		
		log_amx("Cmd: ^"%s<%d><%s><>^" set cvar (name ^"%s^") (value ^"%s^")", name, get_user_userid(id), authid, g_current_cvar_name[id], args);
	
	
		new cvar_val[64];
		for (new i = 1; i <= MaxClients; i++)
		{
			if (is_user_connected(i) && !is_user_bot(i))
			{
				if (get_pcvar_flags(pointer) & FCVAR_PROTECTED || equali(args, "rcon_password"))
				{
					formatex(cvar_val, charsmax(cvar_val), "*** %L ***", i, "PROTECTED");
				}
				else
				{
					copy(cvar_val, charsmax(cvar_val), args);
				}
				show_activity_id(i, id, name, "%L", i, "SET_CVAR_TO", "", g_current_cvar_name[id], cvar_val);
			}
		}
		console_print(id, "[AMXX] %L", id, "CVAR_CHANGED", g_current_cvar_name[id], args);
		
	}
	
	// Now redraw the menu for the client
	if (g_current_menu_function[id] != -1 && callfunc_begin_i(g_current_menu_function[id]) == 1)
	{
		callfunc_push_int(id);
		callfunc_push_int(g_current_player_id[id]);
		callfunc_push_int(g_current_page[id]);
		callfunc_end();
	}
	
	return PLUGIN_HANDLED;
}

/**
 * Process a selection from the cvar menu.
 *
 * @param id		The client who chose an item.
 * @param menu		The menu handle.
 * @param item		The item that has been selected.
 */
public CvarMenuSelection(id, menu, item)
{
	
	if (item == MENU_EXIT)
	{
		menu_destroy(menu);
		
		if (g_explicit_plugin[id] == -1)
		{
			DisplayPluginMenuDefault(id);
		}
	}
	else if (item == MENU_BACK)
	{
		--g_current_page[id];
		client_print(id, print_chat, "MENU_BACK");
	}
	else if (item == MENU_MORE)
	{
		++g_current_page[id];
		client_print(id, print_chat, "MENU_MORE");
	}
	else
	{
		new cvar_name[64];
		new command[32];
		new dummy[1];
		// pcvar pointer is stored in command, extract the name of the cvar from the name field.
		menu_item_getinfo(menu, item, dummy[0], command, charsmax(command), cvar_name, charsmax(cvar_name), dummy[0]);
		
		g_current_cvar[id] = str_to_num(command);
		
		if (g_current_cvar[id] == 0) // This should never happen, but just incase..
		{
			client_print(id, print_chat, "[AMXX] There was an error extracting the cvar pointer. (Name=^"%s^")", cvar_name);
			return PLUGIN_HANDLED;
		}
		// TODO: ML this
		
		// Scan up "cvar_name" and stop at the first space
		for (new i = 0; i < charsmax(cvar_name); i++)
		{
			if (cvar_name[i] == ' ')
			{
				cvar_name[i]= '^0';
				break;
			}
		}
		copy(g_current_cvar_name[id], charsmax(g_current_cvar_name[]), cvar_name);
		client_print(id, print_chat, "[AMXX] Type in the new value for %s, or !cancel to cancel.", cvar_name);
		client_cmd(id, "messagemode amx_changecvar");
		
		menu_destroy(menu);
	}
	
	return PLUGIN_HANDLED;
}
/**
 * Displays the cvar menu to a client.
 *
 * @param id		id of the client.
 * @param plid		Plugin ID to display cvars from.
 * @param page		Page of the menu to start at.
 */
public DisplayCvarMenu(id, plid, page)
{
	new plugin_name[32];
	new menu_title[64];
	get_plugin(plid, "", 0, plugin_name, charsmax(plugin_name), "", 0, "", 0, "", 0);
	
	formatex(menu_title, charsmax(menu_title), "%s Cvars:", plugin_name);
	
	new menu = menu_create(menu_title, "CvarMenuSelection");
	
	new cvar[64];
	new cvar_plid;
	new cvar_text[64];
	new cvar_data[32];
	new cvar_pointer;
	
	for (new i = 0, max = get_plugins_cvarsnum();
		 i < max;
		 i++)
	{
		get_plugins_cvar(i, cvar, charsmax(cvar), _, cvar_plid, cvar_pointer);
		
		if (cvar_plid == plid)
		{
			if (CanIModifyCvar(id, cvar))
			{
				get_pcvar_string(cvar_pointer, cvar_data, charsmax(cvar_data));
				formatex(cvar_text, charsmax(cvar_text), "%s - %s", cvar, cvar_data);
				
				// Now store the pcvar data in cvar
				num_to_str(cvar_pointer, cvar, charsmax(cvar));
				menu_additem(menu, cvar_text, cvar, _ , g_enabled_callback);
			}
			else
			{
				menu_additem(menu, cvar, "", _, g_disabled_callback);
			}
			
		}
	}
	
	menu_setprop(menu, MPROP_EXIT, MEXIT_ALL);
	menu_setprop(menu, MPROP_NUMBER_COLOR, "\y");
	menu_display(id, menu, page);

}
/**
 * Process the "amx_plugincvarmenu" command.
 *
 * @param id		id of the client that is calling the command.
 * @param level		Access level required by the command.
 * @param cid		Command ID.
 */
public CvarMenuCommand(id, level, cid)
{
	if (!cmd_access(id, level, cid, 0))
	{
		return PLUGIN_HANDLED;
	}
	
	// This is which plugin to display.  -1 means display all plugins in a list.
	new plid = -1;
	
	if (GetPlidForValidPlugins(id, plid) != true)
	{
		// If GetPlidForValidPlugins returns false then it failed to find the plugin.
		return PLUGIN_HANDLED;
	}
	
	// Check if we were passed a specific plugin to display or not.
	if (plid == -1)
	{
		g_explicit_plugin[id] = -1;
		// We need to display a list of the plugins, instead of a specific plugin.
		DisplayPluginMenu(id, "Plugin Cvar Menu:", "PluginMenuSelection", "DisplayCvarMenu", "GetNumberOfCvarsForPlid");
	}
	else
	{
		g_explicit_plugin[id] = plid;
		g_current_player_id[id] = plid;
		g_current_page[id] = 0;
		DisplayCvarMenu(id, plid, 0);
	}
	return PLUGIN_HANDLED;
}
/**
 * Handler for the menu that displays a single command ("Execute with no params", etc).
 *
 * @param id		Id of the client.
 * @param menu		Menu handle.
 * @param item		Item that was selected.
 */
public SpecificCommandHandler(id, menu, item)
{
	// Exit was called, return to the previous menu.
	if (item < 0)
	{	
		if (g_current_menu_function[id] != -1 && callfunc_begin_i(g_current_menu_function[id]) == 1)
		{
			callfunc_push_int(id);
			callfunc_push_int(g_current_player_id[id]);
			callfunc_push_int(g_current_page[id]);
			callfunc_end();
		}
		menu_destroy(menu);
		
		return PLUGIN_HANDLED;
	}
	
	new dummy[1];
	if (item == 0)  // "With params"
	{
		menu_item_getinfo(menu, item, dummy[0], g_current_command[id], charsmax(g_current_command[]), "", 0, dummy[0]);
		if (g_current_command[id][0] == 0) // This should never happen, but just incase..
		{
			client_print(id, print_chat, "[AMXX] There was an error extracting the command name.");
			return PLUGIN_HANDLED;
		}
		// TODO: ML this
		
		client_print(id, print_chat, "[AMXX] Type in the parameters for %s, or !cancel to cancel.", g_current_command[id]);
		client_cmd(id, "messagemode amx_executecmd");
		
		menu_destroy(menu);
		
		return PLUGIN_HANDLED; // Don't return to original menu immediately!
	}
	else if (item == 1) // "No params"
	{
		menu_item_getinfo(menu, item, dummy[0], g_current_command[id], charsmax(g_current_command[]), "", 0, dummy[0]);
		if (g_current_command[id][0] == 0) // This should never happen, but just incase..
		{
			client_print(id, print_chat, "[AMXX] There was an error extracting the command name.");
			return PLUGIN_HANDLED;
		}
		// TODO: ML this

		// Now redraw the menu for the client BEFORE the command is executed, incase
		// that menu brings up a menu of its own.
		if (g_current_menu_function[id] != -1 && callfunc_begin_i(g_current_menu_function[id]) == 1)
		{
			callfunc_push_int(id);
			callfunc_push_int(g_current_player_id[id]);
			callfunc_push_int(g_current_page[id]);
			callfunc_end();
		}
		menu_destroy(menu);
		
		client_cmd(id, "%s", g_current_command[id]);
		client_print(id, print_chat, "[AMXX] Command ^"%s^" executed with no parameters", g_current_command[id]);
		
		return PLUGIN_HANDLED;
	}
	
	// We should never get here, but just incase..
	menu_destroy(menu);
	
	return PLUGIN_HANDLED;
}

/**
 * Generates and displays a menu to the client for a specific command.
 *
 * @param id		The client to display the menu to.
 * @param cid		The command id to display.
 */
stock DisplaySpecificCommand(id, cid)
{
	new command_name[64];
	new command_desc[128];
	new command_title[256];
	new command_access;
	new menu;
	
	get_concmd(cid, command_name, charsmax(command_name), command_access, command_desc, charsmax(command_desc), -1, -1);
	
	if (command_desc[0] != '^0')
	{
		formatex(command_title, charsmax(command_title), "%s^n%s", command_name, command_desc);
		menu=menu_create(command_title, "SpecificCommandHandler");
	}
	else
	{
		menu=menu_create(command_name, "SpecificCommandHandler");
	}
	menu_additem(menu, "Execute with parameters.", command_name, _, g_enabled_callback);
	menu_additem(menu, "Execute with no parameters.", command_name, _, g_enabled_callback);
	
	menu_setprop(menu, MPROP_NUMBER_COLOR, "\y");
	menu_display(id, menu, 0);
}

/**
 * Handles the executed command (via "amx_executecmd").
 *
 * @param id		The id of the client who executed this.
 */
public CommandExecuteCommand(id)
{
	// If they had no command stored, then just ignore it entirely.
	if (g_current_command[id][0] == '^0')
	{
		return PLUGIN_CONTINUE;
	}
	
	new args[256];
	
	read_args(args, charsmax(args));
	
	remove_quotes(args);
	
	if (equali(args, "!cancel", 7))
	{
		// The client didn't want to execute this command.
		client_print(id, print_chat, "[AMXX] Command not executed.");
		
		// Now redraw the menu for the client
		if (g_current_menu_function[id] != -1 && callfunc_begin_i(g_current_menu_function[id]) == 1)
		{
			callfunc_push_int(id);
			callfunc_push_int(g_current_player_id[id]);
			callfunc_push_int(g_current_page[id]);
			callfunc_end();
		}

	}
	else
	{
		// TODO: ML
		client_print(id, print_chat, "[AMXX] Command ^"%s^" executed with ^"%s^"", g_current_command[id], args);

		// Now redraw the menu for the client
		if (g_current_menu_function[id] != -1 && callfunc_begin_i(g_current_menu_function[id]) == 1)
		{
			callfunc_push_int(id);
			callfunc_push_int(g_current_player_id[id]);
			callfunc_push_int(g_current_page[id]);
			callfunc_end();
		}
		
		// Execute the command on the client.
		client_cmd(id, "%s %s", g_current_command[id], args);
	}
	
	
	return PLUGIN_HANDLED;
	
}

/**
 * Handle a specific selection from the command menu.
 *
 * @param id		id of the client who made the selection.
 * @param menu		The menu handle.
 * @param item		The item that was selected.
 */
public CommandMenuSelection(id, menu, item)
{
	if (item == MENU_EXIT)
	{
		menu_destroy(menu);

		// If the player did not explicitly specify a plugin, return them to the 
		// plugin selection menu.
		
		if (g_explicit_plugin[id] == -1)
		{
			client_cmd(id, "amx_plugincmdmenu");
		}
	}
	else if (item == MENU_BACK)
	{
		--g_current_page[id];
		client_print(id, print_chat, "MENU_BACK");
	}
	else if (item == MENU_MORE)
	{
		++g_current_page[id];
		client_print(id, print_chat, "MENU_MORE");
	}
	else
	{
		new command[32];
		new dummy[1];
		// pcvar pointer is stored in command, extract the name of the cvar from the name field.
		menu_item_getinfo(menu, item, dummy[0], command, charsmax(command), "", 0, dummy[0]);
		
		menu_destroy(menu);
		
		DisplaySpecificCommand(id, str_to_num(command));
	}
	
	return PLUGIN_HANDLED;
}
/**
 * This blocks "say" and "say_team" commands. 
 * Other commands that shouldn't be displayed (eg: amxauth<stuff>) should be filtered out already.
 *
 * @param command	The command that is being checked.
 */
stock bool:IsDisplayableCmd(const command[])
{
	// Block "say" and "say_team"
	if (equal(command, "say", 3))
	{
		return false;
	}
	
	return true;
}
/**
 * Displays a command list for the specified plugin.
 *
 * @param id		Id of the client that's being displayed to.
 * @param plid		Plugin ID of the plugin that is to be displayed.
 * @param page		The page to start at.
 */
public DisplayCmdMenu(id, plid, page)
{
	new plugin_name[32];
	new menu_title[64];
	get_plugin(plid, "", 0, plugin_name, charsmax(plugin_name), "", 0, "", 0, "", 0);
	
	formatex(menu_title, charsmax(menu_title), "%s Commands:", plugin_name);
	
	new menu = menu_create(menu_title, "CommandMenuSelection");
	
	new command[64];
	new cid_string[32];
	new command_access;
	new userflags = get_user_flags(id);
	new bool:isadmin = bool:is_user_admin(id);
	
	
	for (new i = 0, max = get_concmdsnum(-1, -1);
		 i < max;
		 i++)
	{
		if (get_concmd_plid(i, -1, -1) == plid)
		{
			get_concmd(i, command, charsmax(command), command_access, "", 0, -1, -1);
			
			if (IsDisplayableCmd(command))
			{
				if (userflags & command_access || 
					(command_access == ADMIN_ADMIN && isadmin) ||
					 command_access == ADMIN_USER ||
					 command_access == ADMIN_ALL)
				{
					num_to_str(i, cid_string, charsmax(cid_string));
					menu_additem(menu, command, cid_string, 0, g_enabled_callback);
				}
				else
				{
					menu_additem(menu, command, "", 0, g_disabled_callback);
				}
			}
		}
	}
	menu_setprop(menu, MPROP_NUMBER_COLOR, "\y");
	menu_display(id, menu, page);

}
/**
 * Handles the "amx_plugincmdmenu" command.
 *
 * @param id		Id of the client that's being checked.
 * @param level		Access level of the command.
 * @param cid		Command ID of the command that was executed.
 */
public CommandMenuCommand(id, level, cid)
{
	if (!cmd_access(id, level, cid, 0))
	{
		return PLUGIN_HANDLED;
	}
	
	// This is which plugin to display.  -1 means display all plugins in a list.
	new plid = -1;
	
	if (GetPlidForValidPlugins(id, plid) != true)
	{
		// If GetPlidForValidPlugins returns false then it failed to find the plugin.
		return PLUGIN_HANDLED;
	}
	
	// Check if we were passed a specific plugin to display or not.
	if (plid == -1)
	{
		// We need to display a list of the plugins, instead of a specific plugin.
		g_explicit_plugin[id] = -1;
		DisplayPluginMenuDefault(id);
	}
	else
	{
		g_explicit_plugin[id] = plid;
		g_current_player_id[id] = plid;
		g_current_page[id] = 0;
		DisplayCmdMenu(id, plid, 0);
	}
	return PLUGIN_HANDLED;
}

DisplayPluginMenuDefault(id)
{
	DisplayPluginMenu(id, "Plugin Command Menu:", "PluginMenuSelection", "DisplayCmdMenu", "GetNumberOfCmdsForPlid");
}
