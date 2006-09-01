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

#if defined AMXMOD_BCOMPAT
#if defined _translator_included
#define SIMPLE_T(%1)	_T(%1)
#else
#define SIMPLE_T(%1)	%1
#endif
#endif

stock is_user_admin(id)
{
  return ( get_user_flags(id)>0 && !(get_user_flags(id)&ADMIN_USER) )
}

stock cmd_access(id,level,cid,num) {
  new has_access = 0
  if ( id==(is_dedicated_server()?0:1) ) {
    has_access = 1
  } else if ( level==ADMIN_ADMIN ) {
    if ( is_user_admin(id) )
      has_access = 1
  } else if ( get_user_flags(id) & level ) {
    has_access = 1
  } else if (level == ADMIN_ALL) {
	has_access = 1
  }

  if ( has_access==0 ) {
#if defined AMXMOD_BCOMPAT
    console_print(id, SIMPLE_T("You have no access to that command."))
#else
    console_print(id,"%L",id,"NO_ACC_COM")
#endif
    return 0
  }
  if (read_argc() < num) {
    new hcmd[32], hinfo[128], hflag
    get_concmd(cid,hcmd,31,hflag,hinfo,127,level)
#if defined AMXMOD_BCOMPAT
    console_print(id, SIMPLE_T("Usage:  %s %s"), hcmd, SIMPLE_T(hinfo))
#else
    console_print(id,"%L:  %s %s",id,"USAGE",hcmd,hinfo)
#endif
    return 0
  }
  return 1
}

stock access(id,level) {
  if ( level==ADMIN_ADMIN )
    return is_user_admin(id)

  return (get_user_flags(id) & level)
}

/* Flags:
*  1 - obey immunity
*  2 - allow yourself
*  4 - must be alive
*  8 - can't be bot */
stock cmd_target(id,const arg[],flags = 1) {
  new player = find_player("bl",arg)
  if (player) {
    if ( player != find_player("blj",arg) ) {
#if defined AMXMOD_BCOMPAT
      console_print(id, SIMPLE_T("There are more clients matching to your argument"))
#else
      console_print(id,"%L",id,"MORE_CL_MATCHT")
#endif
      return 0
    }
  }
  else if ( ( player = find_player("c",arg) )==0 && arg[0]=='#' && arg[1] )
    player = find_player("k",str_to_num(arg[1]))
  if (!player) {
#if defined AMXMOD_BCOMPAT
    console_print(id, SIMPLE_T("Client with that name or userid not found"))
#else
    console_print(id,"%L",id,"CL_NOT_FOUND")
#endif
    return 0
  }
  if (flags & 1) {
    if ((get_user_flags(player)&ADMIN_IMMUNITY) && ((flags&2)?(id!=player):true) ) {
      new imname[32]
      get_user_name(player,imname,31)
#if defined AMXMOD_BCOMPAT
      console_print(id, SIMPLE_T("Client ^"%s^" has immunity"), imname)
#else
      console_print(id,"%L",id,"CLIENT_IMM",imname)
#endif
      return 0
    }
  }
  if (flags & 4) {
    if (!is_user_alive(player)) {
      new imname[32]
      get_user_name(player,imname,31)
#if defined AMXMOD_BCOMPAT
      console_print(id, SIMPLE_T("That action can't be performed on dead client ^"%s^""), imname)
#else
      console_print(id,"%L",id,"CANT_PERF_DEAD",imname)
#endif
      return 0
    }
  }
  if (flags & 8) {
    if (is_user_bot(player)) {
      new imname[32]
      get_user_name(player,imname,31)
#if defined AMXMOD_BCOMPAT
      console_print(id, SIMPLE_T("That action can't be performed on bot ^"%s^""), imname)
#else
      console_print(id,"%L",id,"CANT_PERF_BOT",imname)
#endif
      return 0
    }
  }
  return player
}

stock show_activity( id, const name[], {Float,_}: ... ) {
  new buffer[128]
  format_args( buffer , 127 , 2 )
  switch(get_cvar_num("amx_show_activity"))
  {
    case 2:
#if defined AMXMOD_BCOMPAT
       client_print(0, print_chat, "%s %s: %s", is_user_admin(id) ? SIMPLE_T("ADMIN") : SIMPLE_T("PLAYER"), name, buffer)
#else
       client_print(0, print_chat, "%L %s: %s", id, is_user_admin(id) ? "ADMIN" : "PLAYER" , name , buffer )
#endif
    case 1: 
#if defined AMXMOD_BCOMPAT
       client_print(0, print_chat, "%s: %s", is_user_admin(id) ? SIMPLE_T("ADMIN") : SIMPLE_T("PLAYER"), buffer)
#else
       client_print(0, print_chat, "%L: %s", id, is_user_admin(id) ? "ADMIN" : "PLAYER", buffer )
#endif
  }
}

stock colored_menus() {
  new mod_name[32]
  get_modname(mod_name,31)

  return ( equal(mod_name,"cstrike") || equal(mod_name,"czero") || equal(mod_name,"dod") )
}

stock cstrike_running() {
  new mod_name[32]
  get_modname(mod_name,31)

  return ( equal(mod_name,"cstrike") || equal(mod_name,"czero") || equal(mod_name,"csv15") || equal(mod_name,"cs13") )
}

stock is_running(const mod[]) {
  new mod_name[32]
  get_modname(mod_name,31)

  return equal(mod_name,mod)
}

stock get_basedir(name[],len)
  return get_localinfo("amxx_basedir",name,len)

stock get_configsdir(name[],len)
  return get_localinfo("amxx_configsdir",name,len)

stock get_datadir(name[],len)
  return get_localinfo("amxx_datadir",name,len)

stock register_menu(title[],keys,function[],outside=0)
  register_menucmd(register_menuid(title,outside),keys,function)

/* Backwards Compatibility
* don't use it! */
stock get_customdir(name[],len)
  return get_localinfo("amxx_configsdir",name,len)

/* Add a menu item to Menus Front-End plugin ("amxmodmenu"):
* MENU_TEXT: Text that will be shown for this item in menu
* MENU_CMD: Command that should be executed to start menu
* MENU_ACCESS: Access required for menu
* MENU_PLUGIN: The exact case-insensitive name of plugin holding the menu command
*/
stock AddMenuItem(const MENU_TEXT[], const MENU_CMD[], const MENU_ACCESS, const MENU_PLUGIN[]) {
	AddMenuItem_call(MENU_TEXT, MENU_CMD, MENU_ACCESS, MENU_PLUGIN, false)
}
/* Just like above, but add menu item to "amx_menu", that should also be accessible by non-admins.
*/
stock AddClientMenuItem(const MENU_TEXT[], const MENU_CMD[], const MENU_ACCESS, const MENU_PLUGIN[]) {
	AddMenuItem_call(MENU_TEXT, MENU_CMD, MENU_ACCESS, MENU_PLUGIN, true)
}

// Internal function used by above stocks.
stock AddMenuItem_call(const MENU_TEXT[], const MENU_CMD[], const MENU_ACCESS, const MENU_PLUGIN[], const bool:ADD_TO_CLIENT_MENU) {
	new pluginid = is_plugin_loaded("Menus Front-End")
	if (pluginid == -1) {
		log_amx("Can't add menu item ^"%s^" from plugin ^"%s^" to menu set because the Menus Front-End plugin itself is not loaded!", MENU_TEXT, MENU_PLUGIN)
		return // Menus Front-End doesn't exist, return.
	}

	new filename[64], b[1]
	get_plugin(pluginid, filename, 63, b, 0, b, 0, b, 0, b, 0)

	new status = callfunc_begin(ADD_TO_CLIENT_MENU ? "AddClientMenu" : "AddMenu", filename)
	new bool:failed = true
	switch (status) {
		case 1: failed = false
		case 0: log_amx("Run time error! (AddMenuItem_call failed)")
		case -2: log_amx("Function not found! (AddMenuItem_call failed)")
		case -1: log_amx("Plugin not found! (AddMenuItem_call failed)")
	}
	if (failed)
		return
	// Item text
	callfunc_push_str(MENU_TEXT)
	// Cmd
	callfunc_push_str(MENU_CMD)
	// Access
	callfunc_push_int(MENU_ACCESS)
	// Menu exists in this plugin
	callfunc_push_str(MENU_PLUGIN)

	callfunc_end()
}


stock constraint_offset(low, high, seed, offset)
{
   new numElements = high - low + 1;
   offset += seed - low;
   
   if (offset >= 0)
      return low + (offset % numElements);
   else
      return high - (abs(offset) % numElements) + 1;
   return 0;	// Makes the compiler happy -_-
}
