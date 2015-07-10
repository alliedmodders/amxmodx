// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Info. Messages Plugin
//

#include <amxmodx>
#include <amxmisc>

#define X_POS         -1.0
#define Y_POS         0.20
#define HOLD_TIME     12.0

new Array:g_Values
new Array:g_Messages
new g_MessagesNum
new g_Current

new amx_freq_imessage;

public plugin_init()
{
	g_Messages=ArrayCreate(384);
	g_Values=ArrayCreate(3);
	register_plugin("Info. Messages", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("imessage.txt")
	register_dictionary("common.txt")
	register_srvcmd("amx_imessage", "setMessage")
	amx_freq_imessage=register_cvar("amx_freq_imessage", "10")
	
	new lastinfo[8]
	get_localinfo("lastinfomsg", lastinfo, charsmax(lastinfo))
	g_Current = str_to_num(lastinfo)
	set_localinfo("lastinfomsg", "")
}

public infoMessage()
{
	if (g_Current >= g_MessagesNum)
		g_Current = 0
		
	// No messages, just get out of here
	if (g_MessagesNum==0)
	{
		return;
	}
	
	new values[3];
	new Message[384];
	
	ArrayGetString(g_Messages, g_Current, Message, charsmax(Message));
	ArrayGetArray(g_Values, g_Current, values);
	
	new hostname[64];
	
	get_cvar_string("hostname", hostname, charsmax(hostname));
	replace(Message, charsmax(Message), "%hostname%", hostname);
	
	set_hudmessage(values[0], values[1], values[2], X_POS, Y_POS, 0, 0.5, HOLD_TIME, 2.0, 2.0, -1);
	
	show_hudmessage(0, "%s", Message);
	
	client_print(0, print_console, "%s", Message);
	++g_Current;
	
	new Float:freq_im = get_pcvar_float(amx_freq_imessage);
	
	if (freq_im > 0.0)
		set_task(freq_im, "infoMessage", 12345);
}

public setMessage()
{

	new Message[384];
	
	remove_task(12345)
	read_argv(1, Message, charsmax(Message))
	
	while (replace(Message, charsmax(Message), "\n", "^n")) {}
	
	new mycol[12]
	new vals[3];
	
	read_argv(2, mycol, charsmax(mycol))		// RRRGGGBBB
	vals[2] = str_to_num(mycol[6])
	
	mycol[6] = 0
	vals[1] = str_to_num(mycol[3])
	
	mycol[3] = 0
	vals[0] = str_to_num(mycol[0])
	
	g_MessagesNum++
	
	new Float:freq_im = get_pcvar_float(amx_freq_imessage)
	
	ArrayPushString(g_Messages, Message);
	ArrayPushArray(g_Values, vals);
	
	if (freq_im > 0.0)
		set_task(freq_im, "infoMessage", 12345)
	
	return PLUGIN_HANDLED
}

public plugin_end()
{
	new lastinfo[8]

	num_to_str(g_Current, lastinfo, charsmax(lastinfo))
	set_localinfo("lastinfomsg", lastinfo)

	ArrayDestroy(g_Messages)
	ArrayDestroy(g_Values)
}
