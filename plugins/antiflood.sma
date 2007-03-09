/* AMX Mod X
*   Anti Flood Plugin
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

new Float:g_Flooding[33] = {0.0, ...}
new g_Flood[33] = {0, ...}

new amx_flood_time;

public plugin_init()
{
	register_plugin("Anti Flood", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("antiflood.txt")
	register_clcmd("say", "chkFlood")
	register_clcmd("say_team", "chkFlood")
	amx_flood_time=register_cvar("amx_flood_time", "0.75")
}

public chkFlood(id)
{
	new Float:maxChat = get_pcvar_float(amx_flood_time)

	if (maxChat)
	{
		new Float:nexTime = get_gametime()
		
		if (g_Flooding[id] > nexTime)
		{
			if (g_Flood[id] >= 3)
			{
				client_print(id, print_notify, "** %L **", id, "STOP_FLOOD")
				g_Flooding[id] = nexTime + maxChat + 3.0
				return PLUGIN_HANDLED
			}
			g_Flood[id]++
		}
		else if (g_Flood[id])
		{
			g_Flood[id]--
		}
		
		g_Flooding[id] = nexTime + maxChat
	}

	return PLUGIN_CONTINUE
}
