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

stock cmd_access(id,level,cid,num) {
  if ( ((get_user_flags(id)&level)!=level) && (id!=(is_dedicated_server()?0:1)) ) {
    console_print(id,"You have no access to that command")
    return 0
  }
  if (read_argc() < num) {
    new hcmd[32], hinfo[128], hflag
    get_concmd(cid,hcmd,31,hflag,hinfo,127,level)
    console_print(id,"Usage:  %s %s",hcmd,hinfo)
    return 0
  }
  return 1
}

stock access(id,level)
  return (get_user_flags(id) & level)

/* Flags:
*  1 - obey immunity
*  2 - allow yourself
*  4 - must be alive
*  8 - can't be bot */
stock cmd_target(id,const arg[],flags = 1) {
  new player = find_player("bl",arg)
  if (player) {
    if ( player != find_player("blj",arg) ) {
      console_print(id,"There are more clients matching to your argument")
      return 0    
    }
  }
  else if ( ( player = find_player("c",arg) )==0 && arg[0]=='#' && arg[1] )
    player = find_player("k",str_to_num(arg[1]))    
  if (!player) {
    console_print(id,"Client with that name or userid not found")
    return 0
  }
  if (flags & 1) {
    if ((get_user_flags(player)&ADMIN_IMMUNITY) && ((flags&2)?(id!=player):true) ) {
      new imname[32]
      get_user_name(player,imname,31)
      console_print(id,"Client ^"%s^" has immunity",imname)
      return 0
    }
  }
  if (flags & 4) {
    if (!is_user_alive(player)) {
      new imname[32]
      get_user_name(player,imname,31) 
      console_print(id,"That action can't be performed on dead client ^"%s^"",imname)
      return 0
    }
  }
  if (flags & 8) {
    if (is_user_bot(player)) {
      new imname[32]
      get_user_name(player,imname,31) 
      console_print(id,"That action can't be performed on bot ^"%s^"",imname)
      return 0
    } 
  }
  return player
}

stock show_activity( id, const name[], {Float,_}: ... ) {
  new buffer[128]
  format_args( buffer , 127 , 2 )
  switch(get_cvar_num("amx_show_activity")) {
    case 2: client_print(0,print_chat,"%s %s: %s",
      (get_user_flags(id) & ADMIN_USER) ? "PLAYER" : "ADMIN" , name , buffer )
    case 1: client_print(0,print_chat,"%s: %s",
      (get_user_flags(id) & ADMIN_USER) ? "PLAYER" : "ADMIN", buffer )
  }
}

stock is_running(const arg[]) {
  new mod_name[32]
  get_modname(mod_name,31)
  return equal(mod_name,arg)
}

stock get_basedir(name[],len)
  return get_localinfo("amxx_basedir",name,len)

stock get_configsdir(name[],len)
  return get_localinfo("amxx_configsdir",name,len)

stock get_customdir(name[],len)
  return get_localinfo("amxx_customdir",name,len)

#if defined NO_STEAM
stock get_user_wonid(index)
{
  new authid[32]
  get_user_authid(index,authid,31)
  return str_to_num(authid)
}
#endif