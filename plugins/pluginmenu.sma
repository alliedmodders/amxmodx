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



new DisabledCallback;
new EnabledCallback;

// pcvar that the client is currently modifying
new CurrentCvar[MAX_PLAYERS + 1];

// Name of the cvar being modified
new CurrentCvarName[MAX_PLAYERS + 1][32];

// Plugin ID that the client is modifying
new CurrentPlid[MAX_PLAYERS + 1];

// Page that the client is currently on
new CurrentPage[MAX_PLAYERS + 1];

// Menu function ID that the client is in
new CurrentMenuFunction[MAX_PLAYERS + 1] = { -1,... };

new CurrentCommand[MAX_PLAYERS + 1][32];
new cvarmenu_cmdid;
new cmdmenu_cmdid;

new ExplicitPlugin[MAX_PLAYERS + 1];

public plugin_init()
{
	register_plugin("Plugin Menu",AMXX_VERSION_STR,"AMXX Dev Team");
	
	register_dictionary("common.txt");
	register_dictionary("pausecfg.txt"); // Needed for PAUSE_COULDNT_FIND
	
	cvarmenu_cmdid=register_clcmd("amx_plugincvarmenu", "CvarMenuCommand", ADMIN_CVAR, " - displays the plugin cvar menu");
	cmdmenu_cmdid=register_clcmd("amx_plugincmdmenu", "CommandMenuCommand", ADMIN_MENU, " - displays the plugin command menu");
	
	register_clcmd("amx_changecvar","CommandChangeCvar");
	register_clcmd("amx_executecmd","CommandExecuteCommand");
	
	// Register global menu callbacks.
	DisabledCallback=menu_makecallback("AlwaysDisableCallback");
	EnabledCallback=menu_makecallback("AlwaysEnableCallback");
}

// Add these menus to the amxmodmenu
public plugin_cfg()
{
	set_task(0.1, "addToMenuFront");
}
public addToMenuFront()
{
	new PluginFileName[64];
	
	get_plugin(-1, PluginFileName, charsmax(PluginFileName));
	new cvarflags;
	new cmdflags;
	new garbage[1];
	new cmd[32];

	get_concmd(cmdmenu_cmdid, cmd, charsmax(cmd), cmdflags, garbage, charsmax(garbage), -1);

	if (strcmp(cmd, "amx_plugincmdmenu") != 0)
	{
		// this should never happen, but just incase!
		cmdflags = ADMIN_MENU;
	}
	get_concmd(cvarmenu_cmdid, cmd, charsmax(cmd), cvarflags, garbage, charsmax(garbage), -1);

	if (strcmp(cmd, "amx_plugincvarmenu") != 0)
	{
		// this should never happen, but just incase!
		cvarflags = ADMIN_CVAR;
	}

	AddMenuItem("Plugin Cvars", "amx_plugincvarmenu", cvarflags, PluginFileName);
	AddMenuItem("Plugin Commands", "amx_plugincmdmenu", cmdflags, PluginFileName);
}

// Reset all fields for each client as they connect.
public client_connect(id)
{
	CurrentCvar[id]=0;
	CurrentPlid[id]=0;
	CurrentMenuFunction[id]=-1;
	CurrentCvarName[id][0]=0;
	CurrentCommand[id][0]=0;
	ExplicitPlugin[id]=-1;
	
}

/**
 * Creates a plugin list menu.
 *
 * @param MenuText		The text to display as the title.
 * @param Handler		The function to call when an item is selected.
 * @param Command		The function to pass to the handler.  It will be passed as "PLID Command".
 * @param Callback		Function to call for each plugin to be listed.  Displays a number next to it (how many cvars, etc.)
 */
stock DisplayPluginMenu(id,const MenuText[], const Handler[], const Command[], const Callback[])
{
	new Menu=menu_create(MenuText,Handler);
	
	
	new PluginState[32];
	new PluginName[64];
	new func=get_func_id(Callback);
	new tally;
	new PluginCmd[64];
	new MenuText[64];
	for (new i=0, max=get_pluginsnum();
		 i<max;
		 i++)
	{
		if (callfunc_begin_i(func,-1)==1)
		{
			callfunc_push_int(i); // push the plid
			if ((tally=callfunc_end())>0)
			{
				get_plugin(i,"",0,PluginName,charsmax(PluginName),"",0,"",0,PluginState,charsmax(PluginState));
						
				// Command syntax is: "# Function", # being plugin ID, function being public function to call.
				formatex(PluginCmd,charsmax(PluginCmd),"%d %s",i,Command);
				formatex(MenuText,charsmax(MenuText),"%s - %d",PluginName,tally);
				// If the plugin is running, add this as an activated menu item.
				if (strcmp(PluginState,"running",true)==0 ||
					strcmp(PluginState,"debug",  true)==0)
				{
					menu_additem(Menu,MenuText,PluginCmd,EnabledCallback);
				}
				else
				{
					menu_additem(Menu,MenuText,"",_,DisabledCallback);
				}
			}
		}
	}

	menu_setprop(Menu,MPROP_BACKNAME,fmt("%L", id, "BACK"));
	menu_setprop(Menu,MPROP_NEXTNAME,fmt("%L", id, "MORE"));
	menu_setprop(Menu,MPROP_EXITNAME,fmt("%L", id, "EXIT"));
	menu_setprop(Menu,MPROP_NUMBER_COLOR,"\y");
	menu_setprop(Menu,MPROP_EXIT,MEXIT_ALL);
	menu_display(id,Menu,0);

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
	if (read_argc()>1)
	{ 
		// Yes, we were provided a plugin.
		new TargetPlugin[64];
		read_argv(1,TargetPlugin,charsmax(TargetPlugin));
		
		new BufferName[64];
		new BufferFile[64];
		new BufferState[64];
		// Scan for the plugin ID.
		for (new i=0, max=get_pluginsnum();
			 i<max;
			 i++)
		{
			get_plugin(i,BufferFile,charsmax(BufferFile),BufferName,charsmax(BufferName),"",0,"",0,BufferState,charsmax(BufferState));
			
			if (strcmp(BufferFile,TargetPlugin,true) != 0||
				strcmp(BufferName,TargetPlugin,true) != 0)
			{
				// We have a match.
				
				// Check the status of the plugin.  If it's anything other than "running" or "debug" fail.
				if (strcmp(BufferState,"running") != 0 &&
					strcmp(BufferState,"debug")   != 0)
				{
					// TODO: ML This
					console_print(id,"Plugin ^"%s^" is not running.",BufferFile);
					// Return a failed state.
					return false;
				}
				plid=i;
				break;
			}
		}
		
		// If the plugin was not found, then tell them there was an error.
		if (plid==-1)
		{
			console_print(id, "%L", id, "PAUSE_COULDNT_FIND", TargetPlugin);
			
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
	new count=0;
	new CvarPlid;
	for (new i=0, max=get_plugins_cvarsnum();
		 i<max;
		 i++)
	{
		get_plugins_cvar(i, "", 0,_, CvarPlid, _);
		
		if (CvarPlid==plid)
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
	new count=0;
	
	for (new i=0, max=get_concmdsnum(-1,-1);
		 i<max;
		 i++)
	{
		if (get_concmd_plid(i,-1,-1)==plid)
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
 * @param Cvar			The name of the cvar to be checked.
 * @return				True if the client has access, false otherwise.
 */
stock bool:CanIModifyCvar(id, const Cvar[])
{
	new UserFlags=get_user_flags(id);
	// If the user has rcon access don't bother checking anything.
	if (UserFlags & ADMIN_RCON)
	{
		return true;
	}
	
	// If the cvar is "sv_password" (somehow), then check access.
	if (equali(Cvar,"sv_password") && UserFlags & ADMIN_PASSWORD)
	{
		return true;
	}
	
	// Check to see if the cvar is flagged as protected.
	if (get_cvar_flags(Cvar) & FCVAR_PROTECTED)
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
	if (item==MENU_EXIT)
	{
		menu_destroy(menu);
	}
	if (item<0)
	{
		return PLUGIN_HANDLED;
	}
	
	new Command[64];
	new Dummy[1];
	
	// All of the commands set for each item is the public
	// function that we want to call after the item is selected.
	// The parameters are: function(idPlayer,itemnumber)
	// Note the menu is destroyed BEFORE the command
	// gets executed.
	// The command retrieved is in the format: "PLID Command"
	menu_item_getinfo(menu, item, Dummy[0], Command, charsmax(Command),Dummy,0,Dummy[0]);
	
	
	new plid=str_to_num(Command);
	new Function[32];
	
	for (new i=0;i<charsmax(Command);i++)
	{
		if (Command[i]==' ')
		{
			// we're at the break. move up one space.
			i++;
			copy(Function,charsmax(Function),Command[i]);
			break;
		}
	}
	
	menu_destroy(menu);
	
	new funcid=get_func_id(Function);
	if (funcid != -1 && callfunc_begin_i(funcid)==1)
	{
		CurrentPage[id]=0;
		CurrentPlid[id]=plid;
		CurrentMenuFunction[id]=funcid;
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
	if (CurrentCvar[id]==0)
	{
		return PLUGIN_CONTINUE;
	}
	
	new Args[256];
	
	read_args(Args,charsmax(Args));
	
	remove_quotes(Args);
	
	if (equali(Args,"!cancel",7))
	{
		// The client didn't want to change this cvar.
		client_print(id,print_chat,"[AMXX] Cvar not changed.");
	}
	else
	{
		// Changed to set_cvar_* for 1.76 tests
		
		new pointer=CurrentCvar[id];
		set_pcvar_string(CurrentCvar[id],Args);
		
		client_print(id,print_chat,"[AMXX] Cvar ^"%s^" changed to ^"%s^"",CurrentCvarName[id],Args);
		
		// Copy of admincmd's global output.
		
		new Name[MAX_NAME_LENGTH];
		new AuthID[40];
		
		get_user_name(id,Name,charsmax(Name));
		get_user_authid(id,AuthID,charsmax(AuthID));
		
		log_amx("Cmd: ^"%s<%d><%s><>^" set cvar (name ^"%s^") (value ^"%s^")", Name, get_user_userid(id), AuthID, CurrentCvarName[id], Args);
	
	
		new cvar_val[64];
		for (new i = 1; i <= MaxClients; i++)
		{
			if (is_user_connected(i) && !is_user_bot(i))
			{
				if (get_pcvar_flags(pointer) & FCVAR_PROTECTED || equali(Args, "rcon_password"))
				{
					formatex(cvar_val, charsmax(cvar_val), "*** %L ***", i, "PROTECTED");
				}
				else
				{
					copy(cvar_val, charsmax(cvar_val), Args);
				}
				show_activity_id(i, id, Name, "%L", i, "SET_CVAR_TO", "", CurrentCvarName[id], cvar_val);
			}
		}
		console_print(id, "[AMXX] %L", id, "CVAR_CHANGED", CurrentCvarName[id], Args);
		
	}
	
	// Now redraw the menu for the client
	if (CurrentMenuFunction[id]!=-1 && callfunc_begin_i(CurrentMenuFunction[id])==1)
	{
		callfunc_push_int(id);
		callfunc_push_int(CurrentPlid[id]);
		callfunc_push_int(CurrentPage[id]);
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
	
	if (item==MENU_EXIT)
	{
		menu_destroy(menu);
		
		if (ExplicitPlugin[id]==-1)
		{
			DisplayPluginMenu(id,"Plugin Cvar Menu:", "PluginMenuSelection","DisplayCvarMenu","GetNumberOfCvarsForPlid");
		}
	}
	else if (item==MENU_BACK)
	{
		--CurrentPage[id];
		client_print(id,print_chat,"MENU_BACK");
	}
	else if (item==MENU_MORE)
	{
		++CurrentPage[id];
		client_print(id,print_chat,"MENU_MORE");
	}
	else
	{
		new CvarName[64];
		new Command[32];
		new Dummy[1];
		// pcvar pointer is stored in command, extract the name of the cvar from the name field.
		menu_item_getinfo(menu, item, Dummy[0], Command, charsmax(Command),CvarName,charsmax(CvarName),Dummy[0]);
		
		CurrentCvar[id]=str_to_num(Command);
		
		if (CurrentCvar[id]==0) // This should never happen, but just incase..
		{
			client_print(id,print_chat,"[AMXX] There was an error extracting the cvar pointer. (Name=^"%s^")",CvarName);
			return PLUGIN_HANDLED;
		}
		// TODO: ML this
		
		// Scan up "CvarName" and stop at the first space
		for (new i=0;i<charsmax(CvarName);i++)
		{
			if (CvarName[i]==' ')
			{
				CvarName[i]='^0';
				break;
			}
		}
		copy(CurrentCvarName[id],charsmax(CurrentCvarName[]),CvarName);
		client_print(id,print_chat,"[AMXX] Type in the new value for %s, or !cancel to cancel.",CvarName);
		client_cmd(id,"messagemode amx_changecvar");
		
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
	new PluginName[32];
	new MenuTitle[64];
	get_plugin(plid,"",0,PluginName,charsmax(PluginName),"",0,"",0,"",0);
	
	formatex(MenuTitle,charsmax(MenuTitle),"%s Cvars:",PluginName);
	
	new Menu=menu_create(MenuTitle,"CvarMenuSelection");
	
	new Cvar[64];
	new CvarPlid;
	new CvarText[64];
	new CvarData[32];
	new CvarPtr;
	
	for (new i=0, max=get_plugins_cvarsnum();
		 i<max;
		 i++)
	{
		get_plugins_cvar(i, Cvar, charsmax(Cvar),_, CvarPlid, CvarPtr);
		
		if (CvarPlid==plid)
		{
			if (CanIModifyCvar(id,Cvar))
			{
				get_pcvar_string(CvarPtr,CvarData,charsmax(CvarData));
				formatex(CvarText,charsmax(CvarText),"%s - %s",Cvar,CvarData);
				
				// Now store the pcvar data in Cvar
				num_to_str(CvarPtr,Cvar,charsmax(Cvar));
				menu_additem(Menu,CvarText,Cvar,_,EnabledCallback);
			}
			else
			{
				menu_additem(Menu,Cvar,"",_,DisabledCallback);
			}
			
		}
	}
	
	menu_setprop(Menu,MPROP_BACKNAME,fmt("%L", id, "BACK"));
	menu_setprop(Menu,MPROP_NEXTNAME,fmt("%L", id, "MORE"));
	menu_setprop(Menu,MPROP_EXITNAME,fmt("%L", id, "EXIT"));
	menu_setprop(Menu,MPROP_EXIT,MEXIT_ALL);
	menu_setprop(Menu,MPROP_NUMBER_COLOR,"\y");
	menu_display(id,Menu,page);

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
	if (!cmd_access(id,level,cid,0))
	{
		return PLUGIN_HANDLED;
	}
	
	// This is which plugin to display.  -1 means display all plugins in a list.
	new plid=-1;
	
	if (GetPlidForValidPlugins(id,plid)!=true)
	{
		// If GetPlidForValidPlugins returns false then it failed to find the plugin.
		return PLUGIN_HANDLED;
	}
	
	// Check if we were passed a specific plugin to display or not.
	if (plid==-1)
	{
		ExplicitPlugin[id]=-1;
		// We need to display a list of the plugins, instead of a specific plugin.
		DisplayPluginMenu(id,"Plugin Cvar Menu:", "PluginMenuSelection","DisplayCvarMenu","GetNumberOfCvarsForPlid");
	}
	else
	{
		ExplicitPlugin[id]=plid;
		CurrentPlid[id]=plid;
		CurrentPage[id]=0;
		DisplayCvarMenu(id,plid,0);
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
public SpecificCommandHandler(id,menu,item)
{
	// Exit was called, return to the previous menu.
	if (item<0)
	{	
		if (CurrentMenuFunction[id]!=-1 && callfunc_begin_i(CurrentMenuFunction[id])==1)
		{
			callfunc_push_int(id);
			callfunc_push_int(CurrentPlid[id]);
			callfunc_push_int(CurrentPage[id]);
			callfunc_end();
		}
		menu_destroy(menu);
		
		return PLUGIN_HANDLED;
	}
	
	new Dummy[1];
	if (item==0)  // "With params"
	{
		menu_item_getinfo(menu, item, Dummy[0], CurrentCommand[id], charsmax(CurrentCommand[]),"",0,Dummy[0]);
		if (CurrentCommand[id][0]==0) // This should never happen, but just incase..
		{
			client_print(id,print_chat,"[AMXX] There was an error extracting the command name.");
			return PLUGIN_HANDLED;
		}
		// TODO: ML this
		
		client_print(id,print_chat,"[AMXX] Type in the parameters for %s, or !cancel to cancel.",CurrentCommand[id]);
		client_cmd(id,"messagemode amx_executecmd");
		
		menu_destroy(menu);
		
		return PLUGIN_HANDLED; // Don't return to original menu immediately!
	}
	else if (item==1) // "No params"
	{
		menu_item_getinfo(menu, item, Dummy[0], CurrentCommand[id], charsmax(CurrentCommand[]),"",0,Dummy[0]);
		if (CurrentCommand[id][0]==0) // This should never happen, but just incase..
		{
			client_print(id,print_chat,"[AMXX] There was an error extracting the command name.");
			return PLUGIN_HANDLED;
		}
		// TODO: ML this

		// Now redraw the menu for the client BEFORE the command is executed, incase
		// that menu brings up a menu of its own.
		if (CurrentMenuFunction[id]!=-1 && callfunc_begin_i(CurrentMenuFunction[id])==1)
		{
			callfunc_push_int(id);
			callfunc_push_int(CurrentPlid[id]);
			callfunc_push_int(CurrentPage[id]);
			callfunc_end();
		}
		menu_destroy(menu);
		
		client_cmd(id,"%s",CurrentCommand[id]);
		client_print(id,print_chat,"[AMXX] Command ^"%s^" executed with no parameters",CurrentCommand[id]);
		
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
stock DisplaySpecificCommand(id,cid)
{
	new CommandName[64];
	new CommandDesc[128];
	new CommandTitle[256];
	new CommandAccess;
	new Menu;
	
	get_concmd(cid,CommandName,charsmax(CommandName),CommandAccess, CommandDesc, charsmax(CommandDesc), -1, -1);
	
	if (CommandDesc[0]!='^0')
	{
		formatex(CommandTitle,charsmax(CommandTitle),"%s^n%s",CommandName,CommandDesc);
		Menu=menu_create(CommandTitle,"SpecificCommandHandler");
	}
	else
	{
		Menu=menu_create(CommandName,"SpecificCommandHandler");
	}
	menu_additem(Menu,"Execute with parameters.",CommandName,_,EnabledCallback);
	menu_additem(Menu,"Execute with no parameters.",CommandName,_,EnabledCallback);
	
	menu_setprop(Menu,MPROP_NUMBER_COLOR,"\y");
	menu_display(id,Menu,0);
}

/**
 * Handles the executed command (via "amx_executecmd").
 *
 * @param id		The id of the client who executed this.
 */
public CommandExecuteCommand(id)
{
	// If they had no command stored, then just ignore it entirely.
	if (CurrentCommand[id][0]=='^0')
	{
		return PLUGIN_CONTINUE;
	}
	
	new Args[256];
	
	read_args(Args,charsmax(Args));
	
	remove_quotes(Args);
	
	if (equali(Args,"!cancel",7))
	{
		// The client didn't want to execute this command.
		client_print(id,print_chat,"[AMXX] Command not executed.");
		
		// Now redraw the menu for the client
		if (CurrentMenuFunction[id]!=-1 && callfunc_begin_i(CurrentMenuFunction[id])==1)
		{
			callfunc_push_int(id);
			callfunc_push_int(CurrentPlid[id]);
			callfunc_push_int(CurrentPage[id]);
			callfunc_end();
		}

	}
	else
	{
		// TODO: ML
		client_print(id,print_chat,"[AMXX] Command ^"%s^" executed with ^"%s^"",CurrentCommand[id],Args);

		// Now redraw the menu for the client
		if (CurrentMenuFunction[id]!=-1 && callfunc_begin_i(CurrentMenuFunction[id])==1)
		{
			callfunc_push_int(id);
			callfunc_push_int(CurrentPlid[id]);
			callfunc_push_int(CurrentPage[id]);
			callfunc_end();
		}
		
		// Execute the command on the client.
		client_cmd(id,"%s %s",CurrentCommand[id],Args);
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
	if (item==MENU_EXIT)
	{
		menu_destroy(menu);

		// If the player did not explicitly specify a plugin, return them to the 
		// plugin selection menu.
		
		if (ExplicitPlugin[id]==-1)
		{
			DisplayPluginMenu(id,"Plugin Command Menu:", "PluginMenuSelection","DisplayCmdMenu","GetNumberOfCmdsForPlid");
		}
	}
	else if (item==MENU_BACK)
	{
		--CurrentPage[id];
		client_print(id,print_chat,"MENU_BACK");
	}
	else if (item==MENU_MORE)
	{
		++CurrentPage[id];
		client_print(id,print_chat,"MENU_MORE");
	}
	else
	{
		new Command[32];
		new Dummy[1];
		// pcvar pointer is stored in command, extract the name of the cvar from the name field.
		menu_item_getinfo(menu, item, Dummy[0], Command, charsmax(Command),"",0,Dummy[0]);
		
		menu_destroy(menu);
		
		DisplaySpecificCommand(id,str_to_num(Command));
	}
	
	return PLUGIN_HANDLED;
}
/**
 * This blocks "say" and "say_team" commands. 
 * Other commands that shouldn't be displayed (eg: amxauth<stuff>) should be filtered out already.
 *
 * @param Command	The command that is being checked.
 */
stock bool:IsDisplayableCmd(const Command[])
{
	// Block "say" and "say_team"
	if (equal(Command,"say",3))
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
	new PluginName[32];
	new MenuTitle[64];
	get_plugin(plid,"",0,PluginName,charsmax(PluginName),"",0,"",0,"",0);
	
	formatex(MenuTitle,charsmax(MenuTitle),"%s Commands:",PluginName);
	
	new Menu=menu_create(MenuTitle,"CommandMenuSelection");
	
	new Command[64];
	new CidString[32];
	new CommandAccess;
	new userflags=get_user_flags(id);
	new bool:isadmin=bool:is_user_admin(id);
	
	
	for (new i=0, max=get_concmdsnum(-1,-1);
		 i<max;
		 i++)
	{
		if (get_concmd_plid(i,-1,-1)==plid)
		{
			get_concmd(i,Command,charsmax(Command),CommandAccess, "",0, -1, -1);
			
			if (IsDisplayableCmd(Command))
			{
				if ( userflags & CommandAccess || 
					(CommandAccess==ADMIN_ADMIN && isadmin) ||
					 CommandAccess==ADMIN_USER ||
					 CommandAccess==ADMIN_ALL)
				{
					num_to_str(i,CidString,charsmax(CidString));
					menu_additem(Menu,Command,CidString,0,EnabledCallback);
				}
				else
				{
					menu_additem(Menu,Command,"",0,DisabledCallback);
				}
			}
		}
	}
 
	menu_setprop(Menu,MPROP_BACKNAME,fmt("%L", id, "BACK"));
	menu_setprop(Menu,MPROP_NEXTNAME,fmt("%L", id, "MORE"));
	menu_setprop(Menu,MPROP_EXITNAME,fmt("%L", id, "EXIT"));
	menu_setprop(Menu,MPROP_NUMBER_COLOR,"\y");
	menu_display(id,Menu,page);

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
	if (!cmd_access(id,level,cid,0))
	{
		return PLUGIN_HANDLED;
	}
	
	// This is which plugin to display.  -1 means display all plugins in a list.
	new plid=-1;
	
	if (GetPlidForValidPlugins(id,plid)!=true)
	{
		// If GetPlidForValidPlugins returns false then it failed to find the plugin.
		return PLUGIN_HANDLED;
	}
	
	// Check if we were passed a specific plugin to display or not.
	if (plid==-1)
	{
		// We need to display a list of the plugins, instead of a specific plugin.
		ExplicitPlugin[id]=-1;
		DisplayPluginMenu(id,"Plugin Command Menu:", "PluginMenuSelection","DisplayCmdMenu","GetNumberOfCmdsForPlid");
	}
	else
	{
		ExplicitPlugin[id]=plid;
		CurrentPlid[id]=plid;
		CurrentPage[id]=0;
		DisplayCmdMenu(id,plid,0);
	}
	return PLUGIN_HANDLED;
}
