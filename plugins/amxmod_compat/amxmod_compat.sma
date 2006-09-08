/**
 * AMX Mod Compatibility engine
 *  by the AMX Mod X Development Team
 */

#include <amxmodx>
#include <fun>			//we want fun running for extra compatibility
#include <engine>		//we want engine running for extra compatibility
#include <fakemeta>
#include <translator>
#define AMXMODX_NOAUTOLOAD
#include <cstrike>
#include <sqlx>

#define MOD_NORMAL	0
#define MOD_CSTRIKE	1

new g_ModType = MOD_NORMAL
new g_MaxPlayers

#include "core.sma"
#include "vexdum.sma"
#include "mysql.sma"

public plugin_init()
{
	register_plugin("AMX Mod Compat Engine", AMXX_VERSION_STR, "AMXX Dev Team")
	
	g_MaxPlayers = get_maxplayers()
	
	VexdUM_Register()
}

public plugin_natives()
{
	set_module_filter("Plugin_ModuleFilter")
	
	new modname[32]
	get_modname(modname, 31)
	if (equali(modname, "cstrike") || equali(modname, "czero"))
	{
		g_ModType = MOD_CSTRIKE
	}
	
	Core_Natives()
	VexdUM_Natives()
	MySQL_Natives()
}

public Plugin_ModuleFilter(const module[])
{
	if (equali(module, "sqlx"))
	{
		return PLUGIN_HANDLED
	}
	
	return PLUGIN_CONTINUE
}

public client_connect(id)
{
	VexdUM_ClientConnect(id)
}

