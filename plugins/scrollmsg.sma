/* AMX Mod X
*   Scrolling Message Plugin
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
* This file is part of AMX Mod X.
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

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
	
	get_cvar_string("hostname", hostname, 63)
	replace(g_scrollMsg, SCROLLMSG_SIZE-1, "%hostname%", hostname)
	
	g_Length = strlen(g_scrollMsg)
	
	set_task(SPEED, "showMsg", 123, "", 0, "a", g_Length + 48)
	client_print(0, print_console, "%s", g_scrollMsg)
}

public setMessage()
{
	remove_task(123)		/* remove current messaging */
	read_argv(1, g_scrollMsg, SCROLLMSG_SIZE-1)
	
	g_Length = strlen(g_scrollMsg)
	
	new mytime[32]
	
	read_argv(2, mytime, 31)
	
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
