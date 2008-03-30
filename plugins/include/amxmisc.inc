/* AMX Mod X misc.
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
* This file is provided as is (no warranties).
*/

#if defined _amxmisc_included
  #endinput
#endif
#define _amxmisc_included

#if !defined _amxmodx_included
	#if defined AMXMOD_BCOMPAT
		#include <amxmod>
	#else
		#include <amxmodx>
	#endif
#endif

#if defined AMXMOD_BCOMPAT
#if defined _translator_included
#define SIMPLE_T(%1)	_T(%1)
#else
#define SIMPLE_T(%1)	%1
#endif
#endif

stock is_user_admin(id)
{
	new __flags=get_user_flags(id);
	return (__flags>0 && !(__flags&ADMIN_USER));
}

stock cmd_access(id, level, cid, num, bool:accesssilent = false) 
{
	new has_access = 0;
	if ( id==(is_dedicated_server()?0:1) ) 
	{
		has_access = 1;
	}
	else if ( level==ADMIN_ADMIN )
	{
		if ( is_user_admin(id) )
		{
			has_access = 1;
		}
	}
	else if ( get_user_flags(id) & level )
	{
		has_access = 1;
	}
	else if (level == ADMIN_ALL) 
	{
		has_access = 1;
	}

	if ( has_access==0 ) 
	{
		if (!accesssilent)
		{
#if defined AMXMOD_BCOMPAT
			console_print(id, SIMPLE_T("You have no access to that command."));
#else
			console_print(id,"%L",id,"NO_ACC_COM");
#endif
		}
		return 0;
	}
	if (read_argc() < num) 
	{
		new hcmd[32], hinfo[128], hflag;
		get_concmd(cid,hcmd,31,hflag,hinfo,127,level);
#if defined AMXMOD_BCOMPAT
		console_print(id, SIMPLE_T("Usage:  %s %s"), hcmd, SIMPLE_T(hinfo));
#else
		console_print(id,"%L:  %s %s",id,"USAGE",hcmd,hinfo);
#endif
		return 0;
	}
	
	return 1;
}

stock access(id,level) 
{
	if (level==ADMIN_ADMIN)
	{
		return is_user_admin(id);
	}
	else if (level==ADMIN_ALL)
	{
		return 1;
	}

	return (get_user_flags(id) & level);
}

/* Flags:
*  1 - obey immunity
*  2 - allow yourself
*  4 - must be alive
*  8 - can't be bot */
#define CMDTARGET_OBEY_IMMUNITY (1<<0)
#define CMDTARGET_ALLOW_SELF	(1<<1)
#define CMDTARGET_ONLY_ALIVE	(1<<2)
#define CMDTARGET_NO_BOTS		(1<<3)
stock cmd_target(id,const arg[],flags = CMDTARGET_OBEY_IMMUNITY) 
{
	new player = find_player("bl",arg);
	if (player) 
	{
		if ( player != find_player("blj",arg) ) 
		{
#if defined AMXMOD_BCOMPAT
			console_print(id, SIMPLE_T("There are more clients matching to your argument"));
#else
			console_print(id,"%L",id,"MORE_CL_MATCHT");
#endif
			return 0;
		}
	}
	else if ( ( player = find_player("c",arg) )==0 && arg[0]=='#' && arg[1] )
	{
		player = find_player("k",str_to_num(arg[1]));
	}
	if (!player) 
	{
#if defined AMXMOD_BCOMPAT
		console_print(id, SIMPLE_T("Client with that name or userid not found"));
#else
		console_print(id,"%L",id,"CL_NOT_FOUND");
#endif
		return 0;
	}
	if (flags & CMDTARGET_OBEY_IMMUNITY) 
	{
		if ((get_user_flags(player) & ADMIN_IMMUNITY) && 
			((flags & CMDTARGET_ALLOW_SELF) ? (id != player) : true) ) 
		{
			new imname[32];
			get_user_name(player,imname,31);
#if defined AMXMOD_BCOMPAT
			console_print(id, SIMPLE_T("Client ^"%s^" has immunity"), imname);
#else
			console_print(id,"%L",id,"CLIENT_IMM",imname);
#endif
			return 0;
		}
	}
	if (flags & CMDTARGET_ONLY_ALIVE) 
	{
		if (!is_user_alive(player)) 
		{
			new imname[32];
			get_user_name(player,imname,31);
#if defined AMXMOD_BCOMPAT
			console_print(id, SIMPLE_T("That action can't be performed on dead client ^"%s^""), imname);
#else
			console_print(id,"%L",id,"CANT_PERF_DEAD",imname);
#endif
			return 0;
		}
	}
	if (flags & CMDTARGET_NO_BOTS) 
	{
		if (is_user_bot(player)) 
		{
			new imname[32];
			get_user_name(player,imname,31);
#if defined AMXMOD_BCOMPAT
			console_print(id, SIMPLE_T("That action can't be performed on bot ^"%s^""), imname);
#else
			console_print(id,"%L",id,"CANT_PERF_BOT",imname);
#endif
			return 0;
		}
	}
	return player;
}

/**
 * Standard method to show activity to clients connected to the server.
 * This depends on the amx_show_activity cvar.  See documentation for more details.
 *
 * @param id		The user id of the person doing the action.
 * @param name		The name of the person doing the action.
 * @param fmt		The format string to display.  Do not put the "ADMIN:" prefix in this.
 */
stock show_activity( id, const name[], const fmt[], any:... ) 
{
	static __amx_show_activity;
	if (__amx_show_activity == 0)
	{
		__amx_show_activity = get_cvar_pointer("amx_show_activity");
	
		// if still not found, then register the cvar as a dummy
		if (__amx_show_activity == 0)
		{
			__amx_show_activity = register_cvar("amx_show_activity", "2");
		}
	}
#if defined AMXMOD_BCOMPAT
	new buffer[128];
	format_args( buffer , 127 , 2 );
#else
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
#endif
	switch(get_pcvar_num(__amx_show_activity))
	{
#if defined AMXMOD_BCOMPAT
		case 2: // show name to all
		{
			client_print(0, print_chat, "%s %s: %s", is_user_admin(id) ? SIMPLE_T("ADMIN") : SIMPLE_T("PLAYER"), name, buffer);
		}
		case 1: // hide name to all
		{
			client_print(0, print_chat, "%s: %s", is_user_admin(id) ? SIMPLE_T("ADMIN") : SIMPLE_T("PLAYER"), buffer);
		}
#else
		case 5: // hide name only to admins, show nothing to normal users
		{
			new __maxclients=get_maxplayers();
			
			
			for (new i=1; i<=__maxclients; i++)
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
			new __maxclients=get_maxplayers();
			
			for (new i=1; i<=__maxclients; i++)
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
			new __maxclients=get_maxplayers();
			
			for (new i=1; i<=__maxclients; i++)
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
			client_print(0, print_chat, "%L %s: %s", LANG_PLAYER, prefix , name , buffer );
		}
		case 1: // hide name to all
		{
			client_print(0, print_chat, "%L: %s", LANG_PLAYER, prefix, buffer );
		}
#endif
	}
}

/**
 * Standard method to show activity to one single client. 
 * This is useful for messages that get pieced together by many language keys.
 * This depends on the amx_show_activity cvar.  See documentation for more details.
 *
 * @param idtarget	The user id of the person to display to.  0 is invalid.
 * @param idadmin	The user id of the person doing the action.
 * @param name		The name of the person doing the action.
 * @param fmt		The format string to display.  Do not put the "ADMIN:" prefix in this.
 */
stock show_activity_id(idtarget, idadmin, const name[], const fmt[], any:...)
{
	if (idtarget == 0 ||
		!is_user_connected(idtarget) )
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
			__amx_show_activity = register_cvar("amx_show_activity", "2");
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
	
	
	switch(get_pcvar_num(__amx_show_activity))
	{
		case 5: // hide name only to admins, show nothing to normal users
		{
			if ( is_user_admin(idtarget) )
			{
				client_print(idtarget, print_chat, "%L: %s", idtarget, prefix, buffer);
			}
		}
		case 4: // show name only to admins, show nothing to normal users
		{
			if ( is_user_admin(idtarget) )
			{
				client_print(idtarget, print_chat, "%L %s: %s", idtarget, prefix, name, buffer);
			}
		}
		case 3: // show name only to admins, hide name from normal users
		{
			if ( is_user_admin(idtarget) )
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
 * @param KeyWithoutName	The language key that does not have the name field.
 * @param KeyWithName		The language key that does have the name field.
 * @param __AdminName		The name of the person doing the action.
 * @extra					Pass any extra format arguments for the language key in the variable arguments list. 
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
			__amx_show_activity = register_cvar("amx_show_activity", "2");
		}
	}
	
	new buffer[512];
	new keyfmt[256];
	new i;
	
	new __maxclients=get_maxplayers();
	
	switch( get_pcvar_num(__amx_show_activity) )
	{
	case 5: // hide name to admins, display nothing to normal players
		while (i++ < __maxclients)
		{
			if ( is_user_connected(i) )
			{
				if ( is_user_admin(i) )
				{
					LookupLangKey(keyfmt, charsmax(keyfmt), KeyWithoutName, i);

					// skip the "adminname" argument if not showing name
					vformat(buffer, charsmax(buffer), keyfmt, 4);
					client_print(i, print_chat, "%s", buffer);
				}
			}
		}
	case 4: // show name only to admins, display nothing to normal players
		while (i++ < __maxclients)
		{
			if ( is_user_connected(i) )
			{
				if ( is_user_admin(i) )
				{
					LookupLangKey(keyfmt, charsmax(keyfmt), KeyWithName, i);
					vformat(buffer, charsmax(buffer), keyfmt, 3);
					client_print(i, print_chat, "%s", buffer);
				}
			}
		}
	case 3: // show name only to admins, hide name from normal users
		while (i++ < __maxclients)
		{
			if ( is_user_connected(i) )
			{
				if ( is_user_admin(i) )
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
	case 2: // show name to all users
		while (i++ < __maxclients)
		{
			if ( is_user_connected(i) )
			{
				LookupLangKey(keyfmt, charsmax(keyfmt), KeyWithName, i);
				vformat(buffer, charsmax(buffer), keyfmt, 3);
				client_print(i, print_chat, "%s", buffer);
			}
		}
	case 1: // hide name from all users
		while (i++ < __maxclients)
		{
			if ( is_user_connected(i) )
			{
				LookupLangKey(keyfmt, charsmax(keyfmt), KeyWithoutName, i);

				// skip the "adminname" argument if not showing name
				vformat(buffer, charsmax(buffer), keyfmt, 4);
				client_print(i, print_chat, "%s", buffer);
			}
		}
		
	}
}

stock colored_menus() 
{
	new mod_name[32];
	get_modname(mod_name,31);

	return ( equal(mod_name,"cstrike") || equal(mod_name,"czero") || equal(mod_name,"dod") );
}

stock cstrike_running() 
{
	new mod_name[32];
	get_modname(mod_name,31);

	return ( equal(mod_name,"cstrike") || equal(mod_name,"czero") || equal(mod_name,"csv15") || equal(mod_name,"cs13") );
}

stock is_running(const mod[]) 
{
	new mod_name[32];
	get_modname(mod_name,31);

	return equal(mod_name,mod);
}

stock get_basedir(name[],len)
{
	return get_localinfo("amxx_basedir",name,len);
}

stock get_configsdir(name[],len)
{
	return get_localinfo("amxx_configsdir",name,len);
}

stock get_datadir(name[],len)
{
	return get_localinfo("amxx_datadir",name,len);
}

stock register_menu(const title[],keys,const function[],outside=0)
{
	register_menucmd(register_menuid(title,outside),keys,function);
}

/* Backwards Compatibility
* don't use it! */
stock get_customdir(name[],len)
{
	return get_localinfo("amxx_configsdir",name,len);
}

/* Add a menu item to Menus Front-End plugin ("amxmodmenu"):
* MENU_TEXT: Text that will be shown for this item in menu
* MENU_CMD: Command that should be executed to start menu
* MENU_ACCESS: Access required for menu
* MENU_PLUGIN: The exact case-insensitive name of plugin holding the menu command
*/
stock AddMenuItem(const MENU_TEXT[], const MENU_CMD[], const MENU_ACCESS, const MENU_PLUGIN[]) 
{
	AddMenuItem_call(MENU_TEXT, MENU_CMD, MENU_ACCESS, MENU_PLUGIN, false);
}
/* Just like above, but add menu item to "amx_menu", that should also be accessible by non-admins.
*/
stock AddClientMenuItem(const MENU_TEXT[], const MENU_CMD[], const MENU_ACCESS, const MENU_PLUGIN[]) 
{
	AddMenuItem_call(MENU_TEXT, MENU_CMD, MENU_ACCESS, MENU_PLUGIN, true);
}

// Internal function used by above stocks.
stock AddMenuItem_call(const MENU_TEXT[], const MENU_CMD[], const MENU_ACCESS, const MENU_PLUGIN[], const bool:ADD_TO_CLIENT_MENU) 
{
	new pluginid = is_plugin_loaded("Menus Front-End");
	if (pluginid == -1) {
		log_amx("Can't add menu item ^"%s^" from plugin ^"%s^" to menu set because the Menus Front-End plugin itself is not loaded!", MENU_TEXT, MENU_PLUGIN);
		return; // Menus Front-End doesn't exist, return.
	}

	new filename[64], b[1];
	get_plugin(pluginid, filename, 63, b, 0, b, 0, b, 0, b, 0);

	new status = callfunc_begin(ADD_TO_CLIENT_MENU ? "AddClientMenu" : "AddMenu", filename);
	new bool:failed = true;
	switch (status) 
	{
		case 1: failed = false;
		case 0: log_amx("Run time error! (AddMenuItem_call failed)");
		case -2: log_amx("Function not found! (AddMenuItem_call failed)");
		case -1: log_amx("Plugin not found! (AddMenuItem_call failed)");
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

/* Returns true if the user has ANY of the provided flags
 * false if they have none 
 */
stock has_flag(id, const flags[]) 
{
	return (get_user_flags(id) & read_flags(flags));
}
/* Returns true if the user has ALL of the provided flags
 * false otherwise
 */
stock has_all_flags(id, const flags[]) 
{
	new FlagsNumber=read_flags(flags);
	return ((get_user_flags(id) & FlagsNumber)==FlagsNumber);
}
