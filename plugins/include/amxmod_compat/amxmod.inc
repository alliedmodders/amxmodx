/* AMX Mod X Backwards Compatibility
 *
 * by the AMX Mod X Development Team
 *
 * This file is provided as is (no warranties).
 */

#if defined _amxmod_included
  #endinput
#endif
#define _amxmod_included

#if !defined AMXMOD_BCOMPAT
	#define AMXMOD_BCOMPAT
#endif

#include <amxmodx>
#include <cstrike>
#include <engine>
#include <fun>
#include <maths>

stock AMX_VERSION[] = 	"1.76-BC"

#define ADMIN_PERMBAN       ADMIN_BAN		//AMX Mod admin flag for permanent ban
#define ADMIN_UNBAN         ADMIN_BAN		//AMX Mod admin flag for unbanning
#define ADMIN_SUPREME       ADMIN_IMMUNITY	//AMX Mod admin flag for "super admin" (can kick, slap, ban, slay admins with Immunity)

/* Core will identify us as an "old plugin" this way. */
public __b_old_plugin = 1;

public __b_ident_vers()
{
	return __b_old_plugin;
}

stock user_spawn(index)
  return spawn(index);

stock get_logfile( name[], len )
  return get_time("admin%m%d.log",name,len);

stock get_user_money(index)
  return cs_get_user_money(index);

stock set_user_money(index,money,flash=1)
  return cs_set_user_money(index,money,flash);

stock numtostr(num,string[],len)
  return num_to_str(num,string,len);

stock strtonum(const string[])
  return str_to_num(string);
  
stock build_path(path[], len, {Float,_}:... ) {
  format_args(path, len, 2)
  new pathlen = strlen(path)
  new basedir[32]
  if(containi(path, "$basedir") != -1) {
    get_localinfo("amxx_basedir", basedir, 31)
    if(!basedir[0]) copy(basedir, 31, "addons/amxmodx")
    if((pathlen+strlen(basedir)-strlen("$basedir")) < len) {
      replace(path, len, "$basedir", basedir)
    }
  }
  new dir[64], subdir[63]
  if(containi(path, "$configdir") != -1) {
    get_localinfo("amxx_configsdir", dir, 63)
    if(!dir[0]) format(dir, 63, "%s/configs", basedir)
    if((pathlen+strlen(basedir)-strlen("$configdir")) < len) {
      replace(path, len, "$configdir", dir)
    }
    dir[0] = '^0'
  }
  if(containi(path, "$langdir") != -1) {
    get_localinfo("amxx_datadir", subdir, 63)
    if(!subdir[0]) format(subdir, 63, "%s/data", basedir)
    format(dir, 63, "%s/amxmod-lang", subdir)
    if((pathlen+strlen(basedir)-strlen("$langdir")) < len) {
      replace(path, len, "$langdir", dir)
    }
    dir[0] = '^0'
  }
  if(containi(path, "$modulesdir") != -1) {
    get_localinfo("amxx_modules", dir, 63)
    if(!dir[0]) format(dir, 63, "%s/modules", basedir)
    if((pathlen+strlen(basedir)-strlen("$modulesdir")) < len) {
      replace(path, len, "$modulesdir", dir)
    }
    dir[0] = '^0'
  }
  if(containi(path, "$pluginsdir") != -1) {
    get_localinfo("amx_pluginsdir", dir, 63)
    if(!dir[0]) format(dir, 63, "%s/plugins", basedir)
    if((pathlen+strlen(basedir)-strlen("$pluginsdir")) < len) {
      replace(path, len, "$pluginsdir", dir)
    }
    dir[0] = '^0'
  }
  if(containi(path, "$logdir") != -1) {
    get_localinfo("amx_logs", dir, 63)
    if(!dir[0]) format(dir, 63, "%s/logs", basedir)
    if((pathlen+strlen(basedir)-strlen("$logdir")) < len) {
      replace(path, len, "$logdir", dir)
    }
  }
  return 1
}

stock is_user_authorized(id)
{
	static auth[32];
	
	get_user_authid(id, auth, 31);
	if (auth[0] == 0 || equali(auth, "STEAM_ID_PENDING"))
	{
		return 0;
	}
	
	return 1;
}

/* Vector AMX Mod compatibility */
#define ANGLEVECTORS_FORWARD      1
#define ANGLEVECTORS_RIGHT        2
#define ANGLEVECTORS_UP           3

stock angle_to_vector(Float:vector[3], FRU, Float:ret[3])
{
	return angle_vector(vector, FRU, ret)
}
