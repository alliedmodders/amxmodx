/* AMX Mod X script.
*
* (c) 2002-2004, OLO
*  modified by BAILOPAN,Manip,PM,SniperBeamer
*
* This file is provided as is (no warranties).
*/

#include <amxmod>
#include <amxmisc>

#define MAX_ADMINS  64

new g_aPassword[MAX_ADMINS][32]
new g_aName[MAX_ADMINS][32]
new g_aFlags[MAX_ADMINS]
new g_aAccess[MAX_ADMINS]
new g_aNum
new g_logFile[16]
#if !defined NO_STEAM
new g_cmdLoopback[16]
#endif

public plugin_init()
{
  register_plugin("Admin Base","0.1","default")
  register_cvar("amx_mode","2.0")
  register_cvar("amx_password_field","_pw")
  register_cvar("amx_default_access","")
  
  get_logfile(g_logFile,15)
  
#if !defined NO_STEAM  
  format( g_cmdLoopback, 15, "amxauth%c%c%c%c" , 
  	random_num('A','Z') , random_num('A','Z') ,random_num('A','Z'),random_num('A','Z')  )
  	
  register_clcmd( g_cmdLoopback, "ackSignal" )
#endif

  remove_user_flags(0,read_flags("z")) // Remove 'user' flag from server rights
  
  new filename[64]
  get_basedir( filename , 31 )
  server_cmd("exec %s/amx.cfg" , filename ) // Execute main configuration file
  format( filename, 63 , "%s/users.ini" , filename )
  loadSettings(  filename ) // Load admins accounts
  
}

loadSettings(szFilename[])
{
  if (!file_exists(szFilename)) return 0
    
  new szText[256], szFlags[32], szAccess[32]
  new a, pos = 0
  
  while ( g_aNum < MAX_ADMINS && read_file(szFilename,pos++,szText,255,a) ) 
  {         
    if ( szText[0] == ';' ) continue
    
    if ( parse(szText, g_aName[ g_aNum ] ,31, 
      g_aPassword[ g_aNum ], 31, szAccess,31,szFlags,31 ) < 2 ) continue
      
    g_aAccess[ g_aNum ] = read_flags( szAccess )
    g_aFlags[ g_aNum ] = read_flags( szFlags )  
    ++g_aNum
  }
  
  return 1
}

getAccess(id,name[],authid[],ip[], password[])
{
  new index = -1
  new result = 0
  for(new i = 0; i < g_aNum; ++i) {
    if (g_aFlags[i] & FLAG_AUTHID) {
      if (equal(authid,g_aName[i])) {
        index = i
        break
      }
    }
    else if (g_aFlags[i] & FLAG_IP) {
      new c = strlen( g_aName[i] )
      if ( g_aName[i][ c - 1 ] == '.' ) { /* check if this is not a xxx.xxx. format */
        if (  equal( g_aName[i] , ip , c ) ) {
          index = i
          break
        }
      } /* in other case an IP must just match */
      else  if ( equal(ip,g_aName[i]) ){
        index = i
        break
      }
    }
    else {
      if (g_aFlags[i] & FLAG_TAG) {
        if (contain(name,g_aName[i])!=-1){
          index = i
          break
        }
      }
      else if (equal(name,g_aName[i])) {
        index = i
        break
      }
    }
  }
  if (index != -1) {
    if (g_aFlags[index] & FLAG_NOPASS){
      result |= 8
      new sflags[32]
      get_flags(g_aAccess[index],sflags,31)
      set_user_flags(id,g_aAccess[index])
      log_to_file(g_logFile,"Login: ^"%s<%d><%s><>^" become an admin (account ^"%s^") (access ^"%s^") (address ^"%s^")",
        name,get_user_userid(id),authid,g_aName[index] ,sflags,ip)
    }
    else if (equal(password,g_aPassword[index])) {
      result |= 12
      set_user_flags(id,g_aAccess[index])
      new sflags[32]
      get_flags(g_aAccess[index],sflags,31)
      log_to_file(g_logFile,"Login: ^"%s<%d><%s><>^" become an admin (account ^"%s^") (access ^"%s^") (address ^"%s^")",
        name,get_user_userid(id),authid,g_aName[index] ,sflags,ip)
    }
    else {
      result |= 1
      if (g_aFlags[index] & FLAG_KICK){
        result |= 2
        log_to_file(g_logFile,"Login: ^"%s<%d><%s><>^" kicked due to invalid password (account ^"%s^") (address ^"%s^")",
          name,get_user_userid(id),authid,g_aName[index],ip)
      }
    }
  }
  else if (get_cvar_float("amx_mode")==2.0) {
    result |= 2
  }
  else {
    new defaccess[32]
    get_cvar_string("amx_default_access",defaccess,31)
    new idefaccess = read_flags(defaccess)
    if (idefaccess){
      result |= 8
      set_user_flags(id,idefaccess)
    }
  }   
  
  return result
}

accessUser( id, name[] = "" )
{
  remove_user_flags(id)
  new userip[32],userauthid[32],password[32],passfield[32],username[32] 
  get_user_ip(id,userip,31,1) 
  get_user_authid(id,userauthid,31)
  if ( name[0] ) copy( username , 31, name)
  else get_user_name(id,username,31 )
  get_cvar_string("amx_password_field",passfield,31) 
  get_user_info(id,passfield,password,31) 
  new result = getAccess(id,username,userauthid,userip,password) 
  if (result & 1) client_cmd(id,"echo ^"* Invalid Password!^"")
  if (result & 2) {
  
#if !defined NO_STEAM
	client_cmd(id,"echo ^"* You have no entry to the server...^";%s",g_cmdLoopback)
	
#else
    client_cmd(id,"echo ^"* You have no entry to the server...^";disconnect") 
#endif    
    
    return PLUGIN_HANDLED
  }
  if (result & 4) client_cmd(id,"echo ^"* Password accepted^"")
  if (result & 8) client_cmd(id,"echo ^"* Privileges set^"")
  return PLUGIN_CONTINUE
}

public client_infochanged(id)
{ 
  if ( !is_user_connected(id) || !get_cvar_num("amx_mode") ) 
    return PLUGIN_CONTINUE
  
  new newname[32], oldname[32]
  get_user_name(id,oldname,31) 
  get_user_info(id,"name",newname,31) 
  
  if ( !equal(newname,oldname) )
    accessUser( id, newname )
  
  return PLUGIN_CONTINUE
}

#if !defined NO_STEAM

public ackSignal(id)
	server_cmd("kick #%d", get_user_userid(id)  )

public client_authorized(id)
#else
public client_connect(id)
#endif

  return get_cvar_num( "amx_mode" ) ? accessUser( id ) : PLUGIN_CONTINUE
