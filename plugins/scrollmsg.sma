// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Scrolling Message Plugin
//

#include <amxmodx>
#include <amxmisc>

#define SPEED 0.3
#define SCROLLMSG_SIZE	512

new g_startPos
new g_endPos
new g_scrollMsg[SCROLLMSG_SIZE]
new g_displayMsg[SCROLLMSG_SIZE]
new Float:g_xPos
new g_Length
new g_Frequency

public plugin_init()
{
	register_plugin("Scrolling Message", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("scrollmsg.txt")
	register_dictionary("common.txt")
	register_srvcmd("amx_scrollmsg", "setMessage")
}

public showMsg()
{
	new a = g_startPos, i = 0

	while (a < g_endPos)
		g_displayMsg[i++] = g_scrollMsg[a++]

	g_displayMsg[i] = 0

	if (g_endPos < g_Length)
		g_endPos++

	if (g_xPos > 0.35)
		g_xPos -= 0.0063
	else
	{
		g_startPos++
		g_xPos = 0.35
	}

	set_hudmessage(200, 100, 0, g_xPos, 0.90, 0, SPEED, SPEED, 0.05, 0.05, 2)
	show_hudmessage(0, "%s", g_displayMsg)
}

public msgInit()
{
	g_endPos = 1
	g_startPos = 0
	g_xPos = 0.65
	
	new hostname[64]
	
	get_cvar_string("hostname", hostname, charsmax(hostname))
	replace(g_scrollMsg, charsmax(g_scrollMsg), "%hostname%", hostname)
	
	g_Length = strlen(g_scrollMsg)
	
	set_task(SPEED, "showMsg", 123, "", 0, "a", g_Length + 48)
	client_print(0, print_console, "%s", g_scrollMsg)
}

public setMessage()
{
	remove_task(123)		/* remove current messaging */
	read_argv(1, g_scrollMsg, charsmax(g_scrollMsg))
	
	g_Length = strlen(g_scrollMsg)
	
	new mytime[32]
	
	read_argv(2, mytime, charsmax(mytime))
	
	g_Frequency = str_to_num(mytime)
	
	if (g_Frequency > 0)
	{
		new minimal = floatround((g_Length + 48) * (SPEED + 0.1))
		
		if (g_Frequency < minimal)
		{
			server_print("%L", LANG_SERVER, "MIN_FREQ", minimal)
			g_Frequency = minimal
		}

		server_print("%L", LANG_SERVER, "MSG_FREQ", g_Frequency / 60, g_Frequency % 60)
		set_task(float(g_Frequency), "msgInit", 123, "", 0, "b")
	}
	else
		server_print("%L", LANG_SERVER, "MSG_DISABLED")
	
	return PLUGIN_HANDLED
}
