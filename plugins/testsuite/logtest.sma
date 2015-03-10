// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx>

new g_BlockLog

public plugin_init()
{
	register_plugin("Log Tester", "1.0", "BAILOPAN")
	register_srvcmd("log_addlogevent", "Command_AddLogEvent")
	register_srvcmd("log_setblock", "Command_LogSetBlock")
}

public event_round_start()
{

}

public Command_LogSetBlock()
{
	if (read_argc() < 2)
	{
		server_print("Specify 1 or 0.")
		return PLUGIN_HANDLED
	}
	
	new temp[12]
	read_argv(1, temp, 11)
	
	g_BlockLog = str_to_num(temp) ? true : false
	
	return PLUGIN_HANDLED
}

public plugin_log()
{
	server_print("Got log event!  Blocking: %d", g_BlockLog)
	
	return g_BlockLog ? PLUGIN_HANDLED : PLUGIN_CONTINUE
}

public Command_AddLogEvent(id)
{
	register_logevent("event_round_start", 2, "0=World triggered", "1=Round_Start")
	
	return PLUGIN_HANDLED
}
