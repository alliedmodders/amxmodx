/* AMX Mod X
*   Info. Messages Plugin
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

#define MAX_MESSAGES  6
#define X_POS         -1.0
#define Y_POS         0.30
#define HOLD_TIME     12.0

new g_Values[MAX_MESSAGES][3]
new g_Messages[MAX_MESSAGES][384]
new g_MessagesNum
new g_Current

public plugin_init()
{
	register_plugin("Info. Messages", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("imessage.txt")
	register_dictionary("common.txt")
	register_srvcmd("amx_imessage", "setMessage")
	register_cvar("amx_freq_imessage", "10")
	
	new lastinfo[8]
	get_localinfo("lastinfomsg", lastinfo, 7)
	g_Current = str_to_num(lastinfo)
	set_localinfo("lastinfomsg", "")
}

public infoMessage()
{
	if (g_Current >= g_MessagesNum)
		g_Current = 0
		
	new hostname[64]
	
	get_cvar_string("hostname", hostname, 63)
	replace(g_Messages[g_Current], 380, "%hostname%", hostname)
	
	set_hudmessage(g_Values[g_Current][0], g_Values[g_Current][1], g_Values[g_Current][2], X_POS, Y_POS, 0, 0.5, HOLD_TIME, 2.0, 2.0, -1)
	show_hudmessage(0, "%s", g_Messages[g_Current])
	client_print(0, print_console, "%s", g_Messages[g_Current])
	++g_Current
	
	new Float:freq_im = get_cvar_float("amx_freq_imessage")
	
	if (freq_im > 0.0)
		set_task(freq_im, "infoMessage", 12345)
}

public setMessage()
{
	if (g_MessagesNum >= MAX_MESSAGES)
	{
		server_print("%L", LANG_SERVER, "INF_REACH")
		return PLUGIN_HANDLED
	}

	remove_task(12345)
	read_argv(1, g_Messages[g_MessagesNum], 380)
	
	while (replace(g_Messages[g_MessagesNum], 380, "\n", "^n")) {}
	
	new mycol[12]
	
	read_argv(2, mycol, 11)		// RRRGGGBBB
	g_Values[g_MessagesNum][2] = str_to_num(mycol[6])
	
	mycol[6] = 0
	g_Values[g_MessagesNum][1] = str_to_num(mycol[3])
	
	mycol[3] = 0
	g_Values[g_MessagesNum][0] = str_to_num(mycol[0])
	
	g_MessagesNum++
	
	new Float:freq_im = get_cvar_float("amx_freq_imessage")
	
	if (freq_im > 0.0)
		set_task(freq_im, "infoMessage", 12345)
	
	return PLUGIN_HANDLED
}

public plugin_end()
{
	new lastinfo[8]

	num_to_str(g_Current, lastinfo, 7)
	set_localinfo("lastinfomsg", lastinfo)
}
