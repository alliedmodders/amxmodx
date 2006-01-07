/* AMX Mod X
*   Slots Reservation Plugin
*
* by the AMX Mod X Development Team
*  originally developed by OLO
* Updated by Marticus
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
*
* Notes:
* sv_visiblemaxplayers is a steam cvar which hides given number of slots
* from clients. This is only useful to those who wish to have hidden reserved
* slots. With 1 reserved slot, max players 17, this cvar can be used to make
* the server appear to have only 16 available slots. The client can use the gui
* to "auto join" when a slot is available. Players who know they have access
* to that reserved slot can connect using the console command "connect".
*
* This plugin was designed to check players with reservation slots and kick
* those who do not have one. With the revised hidden slot feature, all other
* players can use their client's very useful auto join feature.
*
*/

#include <amxmodx>
#include <amxmisc>

new gPlayerLimit

public plugin_init()
{
	register_plugin("Slots Reservation", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("adminslots.txt")
	register_dictionary("common.txt")

	register_cvar("amx_reservation", "1")

	/* Provide server admin with cvar to hide slots, 0 or 1 -Marticus */
	register_cvar("amx_hideslots", "1")

	/* The maximum number of players after reserved slot(s) */
	gPlayerLimit = get_maxplayers() - get_cvar_num("amx_reservation")

	/* Set server cvar to new player limit to hide the reserved slot(s) */
	if (get_cvar_num("amx_hideslots") == 1)
		set_cvar_num("sv_visiblemaxplayers", gPlayerLimit)
}

public client_authorized(id)
{
	new userid = get_user_userid(id)
	new reason[64]
	format(reason, 63, "%L", id, "DROPPED_RES")

	new players = get_playersnum(1)

	/* Check if player took a reserved slot on a full 
	   server and kick if they have no slot reservation */
	if ((players > gPlayerLimit) && (!access(id, ADMIN_RESERVATION)))
	{
		/* In case player disconnected */
		if (!userid)
		{
			server_cmd("kick #%d ^"%s^"", userid, reason)
			return PLUGIN_CONTINUE
		}
	}
	return PLUGIN_CONTINUE
}
