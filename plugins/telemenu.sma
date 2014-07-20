/* AMX Mod X
*   Teleport Menu Plugin
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
#include <fakemeta>

#define MAX_PLAYERS 32 + 1
new g_menuPosition[MAX_PLAYERS]
new g_menuPlayers[MAX_PLAYERS][32]
new g_menuPlayersNum[MAX_PLAYERS]
new g_menuOption[MAX_PLAYERS] = {-1, ...}
new Float:g_menuOrigin[MAX_PLAYERS][3]
new Float:g_menuVAngle[MAX_PLAYERS][3]
new g_coloredMenus

new g_bDuckingStateSaved
#define SaveInDuckingState(%1)		g_bDuckingStateSaved |= 1<<(%1&31)
#define SaveNoDuckingState(%1)		g_bDuckingStateSaved &= ~(1<<(%1&31))
#define HasInDuckingStateSaved(%1)	g_bDuckingStateSaved & 1<<(%1&31)

public plugin_init()
{
	register_plugin("Teleport Menu", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("telemenu.txt")
	register_dictionary("common.txt")
	register_clcmd("amx_teleportmenu", "cmdTelMenu", ADMIN_CFG, "- displays teleport menu")
	register_menucmd(register_menuid("Teleport Menu"), 1023, "actionTelMenu")

	g_coloredMenus = colored_menus()
}

public actionTelMenu(id, key)
{
	switch (key)
	{
		case 6:
		{
			g_menuOption[id] = 1 - g_menuOption[id]
			displayTelMenu(id, g_menuPosition[id])
		}
		case 7:
		{
			if (g_menuOption[id] < 0)	/* unlocking position for the first time */
				g_menuOption[id] = 0

			getTeleportData(id, g_menuOrigin[id], g_menuVAngle[id])

			if (pev(id, pev_flags) & FL_DUCKING && is_user_alive(id))
				SaveInDuckingState(id)
			else
				SaveNoDuckingState(id)

			displayTelMenu(id, g_menuPosition[id])
		}
		case 8: displayTelMenu(id, ++g_menuPosition[id])
		case 9: displayTelMenu(id, --g_menuPosition[id])
		default:
		{
			new player = g_menuPlayers[id][g_menuPosition[id] * 6 + key]
			new name2[MAX_NAME_LENGTH]

			get_user_name(player, name2, charsmax(name2))

			if (!is_user_alive(player))
			{
				client_print(id, print_chat, "%L", id, "CANT_PERF_DEAD", name2)
				displayTelMenu(id, g_menuPosition[id])
				return PLUGIN_HANDLED
			}

			if (g_menuOption[id] > 0)
			{
				if (HasInDuckingStateSaved(id))
				{
					static Float:VEC_DUCK_VIEW[3] = {0.0, 0.0, -1.0}

					if (VEC_DUCK_VIEW[2] == -1.0)
					{
						new modname[16]

						get_modname(modname, charsmax(modname))
						if (equal(modname, "cstrike")	// Counter-Strike 1.6
						|| equal(modname, "czero")	// Counter-Strike: Condition Zero
						|| equal(modname, "valve")	// Half-Life
						|| equal(modname, "tfc")	// Team Fortress Classic
						|| equal(modname, "gearbox"))	// Half-Life: Opposing Force
							VEC_DUCK_VIEW[2] = 12.0
						else if (equal(modname, "dod"))	// Day of Defeat
							VEC_DUCK_VIEW[2] = 18.0
						else if (equal(modname, "ts"))	// The Specialists
							VEC_DUCK_VIEW[2] = 16.0
						else
							VEC_DUCK_VIEW[2] = 0.0
					}
					if (VEC_DUCK_VIEW[2] > 0.0)
					{
						set_pev(id, pev_flags, pev(id, pev_flags) | FL_DUCKING)
						set_pev(id, pev_view_ofs, VEC_DUCK_VIEW)
					}
				}
				doTeleport(player, g_menuOrigin[id], g_menuVAngle[id])
			} else {
				new Float:origin[3], Float:vAngle[3]

				getTeleportData(id, origin, vAngle)
				doTeleport(player, origin, vAngle)
			}

			new authid[32], authid2[32], name[MAX_NAME_LENGTH]

			get_user_authid(id, authid, charsmax(authid))
			get_user_authid(player, authid2, charsmax(authid2))
			get_user_name(id, name, charsmax(name))

			log_amx("Cmd: ^"%s<%d><%s><>^" teleport ^"%s<%d><%s><>^"", name, get_user_userid(id), authid, name2, get_user_userid(player), authid2)

			show_activity_key("ADMIN_TELEPORT_1", "ADMIN_TELEPORT_2", name, name2)

			displayTelMenu(id, g_menuPosition[id])
		}
	}

	return PLUGIN_HANDLED
}

getTeleportData(id, Float:origin[3], Float:vAngle[3])
{
	pev(id, pev_origin, origin)
	pev(id, pev_v_angle, vAngle)
}

doTeleport(id, const Float:origin[3], const Float:vAngle[3])
{
	engfunc(EngFunc_SetOrigin, id, origin)
	set_pev(id, pev_angles, vAngle)
	set_pev(id, pev_fixangle, 1)
}

displayTelMenu(id, pos)
{
	if (pos < 0)
		return

	get_players(g_menuPlayers[id], g_menuPlayersNum[id])

	new menuBody[512]
	new b = 0
	new i
	new name[MAX_NAME_LENGTH]
	new start = pos * 6
	new bool:blockMenu = (is_user_alive(id) && g_menuOption[id] < 1) ? true : false

	if (start >= g_menuPlayersNum[id])
		start = pos = g_menuPosition[id] = 0

	new len = formatex(menuBody, charsmax(menuBody), g_coloredMenus ? "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n", id, "TELE_MENU", pos + 1, (g_menuPlayersNum[id] / 6 + ((g_menuPlayersNum[id] % 6) ? 1 : 0)))
	new end = start + 6
	new keys = MENU_KEY_0|MENU_KEY_8

	if (end > g_menuPlayersNum[id])
		end = g_menuPlayersNum[id]

	for (new a = start; a < end; ++a)
	{
		i = g_menuPlayers[id][a]
		get_user_name(i, name, charsmax(name))

		if (blockMenu || !is_user_alive(i) || (id != i && get_user_flags(i) & ADMIN_IMMUNITY))
		{
			++b

			if (g_coloredMenus)
				len += formatex(menuBody[len], charsmax(menuBody)-len, "\d%d. %s^n\w", b, name)
			else
				len += formatex(menuBody[len], charsmax(menuBody)-len, "#. %s^n", name)
		} else {
			keys |= (1<<b)

			if (is_user_admin(i))
				len += formatex(menuBody[len], charsmax(menuBody)-len, g_coloredMenus ? "%d. %s \r*^n\w" : "%d. %s *^n", ++b, name)
			else
				len += formatex(menuBody[len], charsmax(menuBody)-len, "%d. %s^n", ++b, name)
		}
	}

	if (g_menuOption[id] > 0)	// 1
	{
		keys |= MENU_KEY_7
		len += formatex(menuBody[len], charsmax(menuBody)-len, "^n7. To location: %.0f %.0f %.0f^n", g_menuOrigin[id][0], g_menuOrigin[id][1], g_menuOrigin[id][2])
	}
	else if (g_menuOption[id])	// -1
	{
		if (g_coloredMenus)
			len += formatex(menuBody[len], charsmax(menuBody)-len, "^n\d7. %L^n\w", id, "CUR_LOC")
		else
			len += formatex(menuBody[len], charsmax(menuBody)-len, "^n#. %L^n", id, "CUR_LOC")
	} else {					// 0
		keys |= MENU_KEY_7
		len += formatex(menuBody[len], charsmax(menuBody)-len, "^n7. %L^n", id, "CUR_LOC")
	}

	len += formatex(menuBody[len], charsmax(menuBody)-len, "8. %L^n", id, "SAVE_LOC")

	if (end != g_menuPlayersNum[id])
	{
		formatex(menuBody[len], charsmax(menuBody)-len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT")
		keys |= MENU_KEY_9
	}
	else
		formatex(menuBody[len], charsmax(menuBody)-len, "^n0. %L", id, pos ? "BACK" : "EXIT")

	show_menu(id, keys, menuBody, -1, "Teleport Menu")
}

public cmdTelMenu(id, level, cid)
{
	if (cmd_access(id, level, cid, 1))
		displayTelMenu(id, g_menuPosition[id] = 0)

	return PLUGIN_HANDLED
}
