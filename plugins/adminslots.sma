/* AMX Mod X
*   Slots Reservation Plugin
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

new g_ResPtr
new g_HidePtr
new g_sv_visiblemaxplayers
new g_maxplayers

public plugin_init()
{
	register_plugin("Slots Reservation", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("adminslots.txt")
	register_dictionary("common.txt")
	g_ResPtr = register_cvar("amx_reservation", "0")
	g_HidePtr = register_cvar("amx_hideslots", "0")
	g_sv_visiblemaxplayers = get_cvar_pointer("sv_visiblemaxplayers")
	g_maxplayers = get_maxplayers()
}

public plugin_cfg()
{
	set_task(3.0, "MapLoaded")
}

public MapLoaded()
{
	if (get_pcvar_num(g_HidePtr))
	{
		setVisibleSlots(get_playersnum(1), g_maxplayers - get_pcvar_num(g_ResPtr))
	}
}

public client_authorized(id)
{
	new players = get_playersnum(1)
	new limit = g_maxplayers - get_pcvar_num(g_ResPtr)

	if (access(id, ADMIN_RESERVATION) || (players <= limit))
	{
		if (get_pcvar_num(g_HidePtr))
			setVisibleSlots(players, limit)
		return
	}
	
 	server_cmd("kick #%d ^"%L^"", get_user_userid(id), id, "DROPPED_RES")
}

public client_disconnect(id)
{
	if (get_pcvar_num(g_HidePtr))
	{
		setVisibleSlots(get_playersnum(1) - 1, g_maxplayers - get_pcvar_num(g_ResPtr))
	}
}

setVisibleSlots(players, limit)
{
	new num = players + 1

	if (players == g_maxplayers)
		num = g_maxplayers
	else if (players < limit)
		num = limit
	
	set_pcvar_num(g_sv_visiblemaxplayers, num)
}
