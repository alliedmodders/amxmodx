/* AMX Mod X script.
*
* (c) 2002-2004, dJeyL
*  modified by BAILOPAN,Manip,PM,SniperBeamer
*
* This file is provided as is (no warranties).
*
* This AMX plugin requires MySQL module.
*
* IMPORTANT:
* o Check $moddir/addons/amx/mysql.cfg for MySQL access configuration
* o Disable admin.amx plugin if you use admin_mysql.amx
*
*/

#include <amxmod>
#include <amxmisc>
#include <mysql>

#define MAX_ADMINS 64

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
  register_plugin("Admin Base for MySQL","0.1","default")

  register_cvar("amx_mode","2.0")
  register_cvar("amx_password_field","_pw")
  register_cvar("amx_default_access","")
  register_srvcmd("amx_sqladmins","adminSql")
  register_cvar("amx_mysql_host","127.0.0.1")
  register_cvar("amx_mysql_user","root")
  register_cvar("amx_mysql_pass","")
  register_cvar("amx_mysql_db","amx")  

#if !defined NO_STEAM   
  format( g_cmdLoopback, 15, "amxauth%c%c%c%c" , 
  	random_num('A','Z') , random_num('A','Z') ,random_num('A','Z'),random_num('A','Z')  )
  register_clcmd( g_cmdLoopback, "ackSignal" )  
#endif


  remove_user_flags(0,read_flags("z")) // remove 'user' flag from server rights

  get_logfile(g_logFile,15)  

  new filename[32]
  get_basedir( filename , 31 )
  server_cmd("exec %s/amx.cfg" , filename)
  server_cmd("exec %s/mysql.cfg;amx_sqladmins" , filename)
}

public adminSql(){
  new host[64],user[32],pass[32],db[32],error[128]
  get_cvar_string("amx_mysql_host",host,63)
  get_cvar_string("amx_mysql_user",user,31)
  get_cvar_string("amx_mysql_pass",pass,31)
  get_cvar_string("amx_mysql_db",db,31)
  
  new mysql = mysql_connect(host,user,pass,db,error,127)
  if(mysql < 1){
    server_print("MySQL error: can't connect: '%s'",error)
    return PLUGIN_HANDLED 
  }

  mysql_query(mysql,"CREATE TABLE IF NOT EXISTS admins ( auth varchar(32) NOT NULL default '', password varchar(32) NOT NULL default '', access varchar(32) NOT NULL default '', flags varchar(32) NOT NULL default '' ) TYPE=MyISAM")

  if(mysql_query(mysql,"SELECT auth,password,access,flags FROM admins") < 1)  {
    mysql_error(mysql,error,127)
    server_print("MySQL error: can't load admins: '%s'",error)
    return PLUGIN_HANDLED
  }

  new szFlags[32], szAccess[32]
  g_aNum = 0 /* reset admins settings */
  while( mysql_nextrow(mysql) > 0 )
  {
    mysql_getfield(mysql, 1, g_aName[ g_aNum ] ,31)
    mysql_getfield(mysql, 2, g_aPassword[ g_aNum ] ,31)
    mysql_getfield(mysql, 3, szAccess,31)
    mysql_getfield(mysql, 4, szFlags,31)
    g_aAccess[ g_aNum ] = read_flags( szAccess )
    g_aFlags[ g_aNum ] = read_flags( szFlags )
    ++g_aNum
  }

  server_print("Loaded %d admin%s from database",g_aNum, (g_aNum == 1) ? "" : "s" )
  mysql_close(mysql)
  return PLUGIN_HANDLED
}

getAccess(id,name[],authid[],ip[], password[]){
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

accessUser( id, name[]="" )
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
    accessUser( id , newname )

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