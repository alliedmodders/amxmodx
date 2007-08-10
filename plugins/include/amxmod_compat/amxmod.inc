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

stock AMX_VERSION[] = 	"1.76-BC";

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
  
stock build_path(path[], len, {Float,_}:... )
{
  format_args(path, len, 2);
  new pathlen = strlen(path);
  new basedir[32];
  if (containi(path, "$basedir") != -1)
  {
    get_localinfo("amxx_basedir", basedir, 31);
    if (!basedir[0])
    {
	    copy(basedir, 31, "addons/amxmodx");
    }
    if ((pathlen+strlen(basedir)-strlen("$basedir")) < len)
    {
    	replace(path, len, "$basedir", basedir);
    }
  }
  new dir[64], subdir[63];
  if (containi(path, "$configdir") != -1)
  {
    get_localinfo("amxx_configsdir", dir, 63);
    if (!dir[0])
    {
	    format(dir, 63, "%s/configs", basedir);
    }
    if ((pathlen+strlen(basedir)-strlen("$configdir")) < len)
    {
    	replace(path, len, "$configdir", dir);
    }
    dir[0] = '^0';
  }
  if (containi(path, "$langdir") != -1)
  {
    get_localinfo("amxx_datadir", subdir, 63);
    if (!subdir[0])
    {
	    format(subdir, 63, "%s/data", basedir);
    }
    format(dir, 63, "%s/amxmod-lang", subdir);
    if ((pathlen+strlen(basedir)-strlen("$langdir")) < len)
    {
      replace(path, len, "$langdir", dir);
    }
    dir[0] = '^0';
  }
  if (containi(path, "$modulesdir") != -1)
  {
    get_localinfo("amxx_modules", dir, 63);
    if (!dir[0])
    {
	    format(dir, 63, "%s/modules", basedir);
    }
    if ((pathlen+strlen(basedir)-strlen("$modulesdir")) < len)
    {
		replace(path, len, "$modulesdir", dir);
    }
    dir[0] = '^0';
  }
  if (containi(path, "$pluginsdir") != -1)
  {
    get_localinfo("amx_pluginsdir", dir, 63);
    if( !dir[0])
    {
	    format(dir, 63, "%s/plugins", basedir);
    }
    if ((pathlen+strlen(basedir)-strlen("$pluginsdir")) < len)
    {
    	replace(path, len, "$pluginsdir", dir);
    }
    dir[0] = '^0';
  }
  if (containi(path, "$logdir") != -1)
  {
    get_localinfo("amx_logs", dir, 63);
    if (!dir[0])
    {
	    format(dir, 63, "%s/logs", basedir);
    }
    if ((pathlen+strlen(basedir)-strlen("$logdir")) < len)
    {
    	replace(path, len, "$logdir", dir);
    }
  }
  return 1;
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
	return angle_vector(vector, FRU, ret);
}

stock get_cmdaccess(cmd[], accessflags[], len)
{
	new num = get_concmdsnum(-1);
	new command[32], info[3];
	new flags;
	
	for (new i=0; i<num; i++)
	{
		get_concmd(i, command, 31, flags, info, 2, -1);
		if (equal(command, cmd))
		{
			get_flags(flags, accessflags, len);
			return 1;
		}
	}
	
	return 0;
}

stock is_translated(const sentence[])
{
	return (GetLangTransKey(sentence) != TransKey_Bad);
}

stock get_plugincmdsnum(plugin[], type=7)
{
	new plid = find_plugin_byfile(plugin);
	new our_type;

	/** 
	 * Whoever wrote this was a bit confused about the type stuff...
	 */	
	if (type == 1) {
		our_type = 1;
	} else if (type == 4) {
		our_type = 0;
	} else {
		our_type = -1;
	}
	
	new found = 0;
	new total = get_concmdsnum(-1, our_type);
	for (new i=0; i<total; i++)
	{
		if (plid == get_concmd_plid(i, -1, our_type))
		{
			found++;
		}
	}
	
	return found;
}

stock get_plugincmd(plugin[], index, cmd[], len1, accessflags[], len2, info[], len3, destid=-1, type=7)
{
	new plid = find_plugin_byfile(plugin);
	new our_type;

	/** 
	 * Whoever wrote this was a bit confused about the type stuff...
	 */	
	if (type == 1) {
		our_type = 1;
	} else if (type == 4) {
		our_type = 0;
	} else {
		our_type = -1;
	}
	
	new found_iter = 0;
	new total = get_concmdsnum(-1, our_type);
	for (new i=0; i<total; i++)
	{
		if (plid == get_concmd_plid(i, -1, our_type))
		{
			if (found_iter == index)
			{
				new flags, result;
				result = get_concmd(i, cmd, len1, flags, info, len3, -1, our_type);
				get_flags(flags, accessflags, len2);
				return result;
			}
			found_iter++;
		}
	}
	
	/* get rid of a compiler warning */
	destid = -1;
	return (++destid);
}

stock get_plugincvar(plugin[], index, cvar[], len1, value[], len2, flags=0)
{
	new plid = find_plugin_byfile(plugin);
	
	new total = get_plugins_cvarsnum();
	new cvar_flags, plugin_id, pcvar_handle;
	new iter_id = 0;
	for (new i=0; i<total; i++)
	{
		get_plugins_cvar(i, cvar, len1, cvar_flags, plugin_id, pcvar_handle);
		if ((plugin_id == plid)
			&& (!flags || (cvar_flags & flags)))
		{
			if (iter_id == index)
			{
				get_pcvar_string(pcvar_handle, value, len2);
				return 1;
			}
			iter_id++;
		}
	}
	
	return 0;
}

stock get_plugincvarsnum(plugin[], flags=0)
{
	new plid = find_plugin_byfile(plugin);
	
	new total = get_plugins_cvarsnum();
	new cvar_flags, plugin_id;
	new cvars_total = 0;
	for (new i=0; i<total; i++)
	{
		get_plugins_cvar(i, "", 0, cvar_flags, plugin_id);
		if ((plugin_id == plid)
			&& (!flags || (cvar_flags & flags)))
		{
			cvars_total++;
		}
	}
	
	return cvars_total;
}

stock is_module_running(const module[])
{
	if (strcmp(module, "MySQL Access") == 0)
		return LibraryExists("sqlx", LibType_Class);
	
	return is_module_loaded(module) == -1 ? 0 : 1;
}

stock is_plugin_running(const plugin[])
{
	new status[8];
	new id, filename[1], name[1], version[1], author[1];
	
	id = is_plugin_loaded(plugin);
	
	get_plugin(id, filename, 0, name, 0, version, 0, author, 0, status, 7);
	
	return strcmp(status, "running") == 0 ? id + 1 : 0;
}
