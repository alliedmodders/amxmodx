/* AMX Mod misc.
 *
 * (c) 2002-2003, OLO
 * This file is provided as is (no warranties).
 */

#if defined _amxmisc_included
  #endinput
#endif
#define _amxmisc_included

stock cmd_access(a,b,c,d){
  if ( ((get_user_flags(a)&b)!=b) && (a!=(is_dedicated_server()?0:1)) ){
    console_print(a,"You have no access to that command")
    return 0
  }
  if (read_argc() < d){
    new hcmd[32], hinfo[128], hflag
    get_concmd(c,hcmd,31,hflag,hinfo,127,b)
    console_print(a,"Usage:  %s %s",hcmd,hinfo)
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
*  8 - can't be bot
*/
stock cmd_target(id,const arg[],flags = 1) {
  new player = find_player("bl",arg)
  if (player){
    if ( player != find_player("blj",arg) ){
      console_print(id,"There are more clients matching to your argument")
      return 0    
    }
  }
  else if ( ( player = find_player("c",arg) )==0 && arg[0]=='#' && arg[1] )
    player = find_player("k",strtonum(arg[1]))    
  if (!player){
    console_print(id,"Client with that name or userid not found")
    return 0
  }
  if (flags & 1){
    if ((get_user_flags(player)&ADMIN_IMMUNITY) && ((flags&2)?(id!=player):true) ){
      new imname[32]
      get_user_name(player,imname,31)
      console_print(id,"Client ^"%s^" has immunity",imname)
      return 0
    }
  }
  if (flags & 4){
    if (!is_user_alive(player)){
      new imname[32]
      get_user_name(player,imname,31) 
      console_print(id,"That action can't be performed on dead client ^"%s^"",imname)
      return 0
    }
  }
  if (flags & 8){
    if (is_user_bot(player)){
      new imname[32]
      get_user_name(player,imname,31) 
      console_print(id,"That action can't be performed on bot ^"%s^"",imname)
      return 0
    } 
  }
  return player
}

stock show_activity( id, const name[], {Float,_}: ... ){
  new buffer[128]
  format_args( buffer , 127 , 2  )
  switch(get_cvar_num("amx_show_activity")) {
    case 2: client_print(0,print_chat,"%s %s: %s",
      (get_user_flags(id) & ADMIN_USER) ? "PLAYER" : "ADMIN" , name , buffer )
    case 1: client_print(0,print_chat,"%s: %s",
      (get_user_flags(id) & ADMIN_USER) ? "PLAYER" : "ADMIN", buffer )
  }
}

stock is_running(const arg[]){
  new mod_name[32]
  get_modname(mod_name,31)
  return equal(mod_name,arg)
}

stock build_path( path[] , len , {Float,_}:... ) {
  new basedir[32]
  get_localinfo( "amx_basedir", basedir , 31 )
  format_args( path , len , 2 )
  return replace( path , len , "$basedir", basedir  )
}

stock get_basedir( name[], len  )
  return get_localinfo( "amx_basedir", name , len )

stock get_logfile( name[], len  )
  return get_time("admin%m%d.log",name,len)