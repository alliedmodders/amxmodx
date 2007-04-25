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

#define X_POS         -1.0
#define Y_POS         0.20
#define HOLD_TIME     12.0

new Array:g_Values
new Array:g_Messages
new g_MessagesNum
new g_Current

#define charsof(%1) (sizeof(%1)-1)

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
	get_localinfo("lastinfomsg", lastinfo, 7)
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
	
	ArrayGetString(g_Messages, g_Current, Message, charsof(Message));
	ArrayGetArray(g_Values, g_Current, values);
	
	new hostname[64];
	
	get_cvar_string("hostname", hostname, 63);
	replace(Message, 380, "%hostname%", hostname);
	
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
	read_argv(1, Message, 380)
	
	while (replace(Message, 380, "\n", "^n")) {}
	
	new mycol[12]
	new vals[3];
	
	read_argv(2, mycol, 11)		// RRRGGGBBB
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

	num_to_str(g_Current, lastinfo, 7)
	set_localinfo("lastinfomsg", lastinfo)
}
