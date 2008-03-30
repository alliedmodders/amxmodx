/* AMX Mod X
*   Menus Front-End Plugin
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

#define MAXMENUS			128
#define STRINGSIZE			32
#define STRINGLENGTH 		STRINGSIZE - 1
#define MENUITEMSPERPAGE	8
//#define MENUS_NUMBER 16

new g_menuPosition[33]
new g_menusNumber = 0
new g_menuBody[MAXMENUS][STRINGSIZE]
new bool:g_menuBodyPhrase[MAXMENUS]
new g_menuCmd[MAXMENUS][STRINGSIZE]
new g_menuAccess[MAXMENUS]
new g_menuPlugin[MAXMENUS][STRINGSIZE]

new g_coloredMenus

new g_clientMenuPosition[33]
new g_clientMenusNumber = 0
new g_clientMenuBody[MAXMENUS][STRINGSIZE]
new bool:g_clientMenuBodyPhrase[MAXMENUS]
new g_clientMenuCmd[MAXMENUS][STRINGSIZE]
new g_clientMenuAccess[MAXMENUS]
new g_clientMenuPlugin[MAXMENUS][STRINGSIZE]

// menuBody: Text that will be shown for this item in menu
// menuCmd: Command that should be executed to start menu
// menuAccess: Access required for menu
// menuPlugin: The exact case-insensitive name of plugin holding the menu command
public AddMenu(const menuBody[], const menuCmd[], const menuAccess, const menuPlugin[])
{
	if (g_menusNumber + 1 == MAXMENUS)
	{
		log_amx("Error: Plugin ^"%s^" tried to add a menu item to Menu Front-End plugin with maximum menu items reached!", menuPlugin)
		return
	}

	copy(g_menuBody[g_menusNumber], STRINGLENGTH, menuBody)
	g_menuBodyPhrase[g_menusNumber] = false
	
	copy(g_menuCmd[g_menusNumber], STRINGLENGTH, menuCmd)
	g_menuAccess[g_menusNumber] = menuAccess
	
	copy(g_menuPlugin[g_menusNumber], STRINGLENGTH, menuPlugin)

	g_menusNumber++
	server_print("Menu item %d added to Menus Front-End: ^"%s^" from plugin ^"%s^"", g_menusNumber, menuBody, menuPlugin)
}

public AddMenuLang(const menuBody[], const menuCmd[], const menuAccess, const menuPlugin[])
{
	if (g_menusNumber + 1 == MAXMENUS)
	{
		log_amx("Error: Plugin ^"%s^" tried to add a menu item to Menu Front-End plugin with maximum menu items reached!", menuPlugin)
		return
	}

	copy(g_menuBody[g_menusNumber], STRINGLENGTH, menuBody)
	g_menuBodyPhrase[g_menusNumber] = true
	
	copy(g_menuCmd[g_menusNumber], STRINGLENGTH, menuCmd)
	g_menuAccess[g_menusNumber] = menuAccess
	
	copy(g_menuPlugin[g_menusNumber], STRINGLENGTH, menuPlugin)
	g_menusNumber++

	//server_print("Menu item %d added to Menus Front-End: ^"%s^" (LANG) from plugin ^"%s^"", g_menusNumber, menuBody, menuPlugin)
}

public AddClientMenu(const menuBody[], const menuCmd[], const menuAccess, const menuPlugin[])
{
	if (g_clientMenusNumber + 1 == MAXMENUS)
	{
		log_amx("Error: Plugin ^"%s^" tried to add a menu item to Menu Front-End plugin with maximum menu items reached!", menuPlugin)
		return
	}

	copy(g_clientMenuBody[g_clientMenusNumber], STRINGLENGTH, menuBody)
	g_clientMenuBodyPhrase[g_clientMenusNumber] = false
	
	copy(g_clientMenuCmd[g_clientMenusNumber], STRINGLENGTH, menuCmd)
	g_clientMenuAccess[g_clientMenusNumber] = menuAccess
	
	copy(g_clientMenuPlugin[g_clientMenusNumber], STRINGLENGTH, menuPlugin)

	g_clientMenusNumber++
	server_print("Client menu item %d added to Client Menus Front-End: ^"%s^" from plugin ^"%s^"", g_clientMenusNumber, menuBody, menuPlugin)
}

AddDefaultMenus()
{
	new flags;
	AddMenuLang("KICK_PLAYER", "amx_kickmenu", get_clcmd_flags("amx_kickmenu", flags) ? flags : ADMIN_KICK , "Players Menu")
	AddMenuLang("BAN_PLAYER", "amx_banmenu", get_clcmd_flags("amx_banmenu", flags) ? flags : ADMIN_BAN, "Players Menu")
	AddMenuLang("SLAP_SLAY", "amx_slapmenu", get_clcmd_flags("amx_slapmenu", flags) ? flags : ADMIN_SLAY, "Players Menu")
	AddMenuLang("TEAM_PLAYER", "amx_teammenu", get_clcmd_flags("amx_teammenu", flags) ? flags : ADMIN_LEVEL_A, "Players Menu")
	AddMenuLang("CHANGEL", "amx_mapmenu", get_clcmd_flags("amx_mapmenu", flags) ? flags : ADMIN_MAP, "Maps Menu")
	AddMenuLang("VOTE_MAPS", "amx_votemapmenu", get_clcmd_flags("amx_votemapmenu", flags) ? flags : ADMIN_VOTE, "Maps Menu")
	AddMenuLang("SPECH_STUFF", "amx_speechmenu", get_clcmd_flags("amx_speechmenu", flags) ? flags : ADMIN_MENU, "Commands Menu")
	AddMenuLang("CLIENT_COM", "amx_clcmdmenu", get_clcmd_flags("amx_clcmdmenu", flags) ? flags : ADMIN_LEVEL_A, "Players Menu")
	AddMenuLang("SERVER_COM", "amx_cmdmenu", get_clcmd_flags("amx_cmdmenu", flags) ? flags : ADMIN_MENU, "Commands Menu")
	AddMenuLang("CVARS_SET", "amx_cvarmenu", get_clcmd_flags("amx_cvarmenu", flags) ? flags : ADMIN_CVAR, "Commands Menu")
	AddMenuLang("CONFIG", "amx_cfgmenu", get_clcmd_flags("amx_cfgmenu", flags) ? flags : ADMIN_MENU, "Commands Menu")
	AddMenuLang("LANG_SET", "amx_langmenu", get_clcmd_flags("amx_langmenu", flags) ? flags : ADMIN_CFG, "Multi-Lingual System")
	AddMenuLang("STATS_SET", "amx_statscfgmenu", get_clcmd_flags("amx_statscfgmenu", flags) ? flags : ADMIN_CFG, "Stats Configuration")
	AddMenuLang("PAUSE_PLUG", "amx_pausecfgmenu", get_clcmd_flags("amx_pausecfgmenu", flags) ? flags : ADMIN_CFG, "Pause Plugins")
	AddMenuLang("RES_WEAP", "amx_restmenu", get_clcmd_flags("amx_restmenu", flags) ? flags : ADMIN_CFG, "Restrict Weapons")
	AddMenuLang("TELE_PLAYER", "amx_teleportmenu", get_clcmd_flags("amx_teleportmenu", flags) ? flags : ADMIN_CFG, "Teleport Menu")
}
stock bool:get_clcmd_flags(const search_command[], &flags)
{
	new count = get_clcmdsnum(-1);
	static cmd[128];
	static info[1];
	new _flags;

	for (new i = 0; i < count; i++)
	{
		get_clcmd(i, cmd, charsmax(cmd), _flags, info, charsmax(info), -1);

		if (strcmp(cmd, search_command) == 0)
		{
			flags = _flags;
			return true;
		}
	}

	return false;
}
public actionMenu(id, key)
{
	switch (key)
	{
		case 8: displayMenu(id, ++g_menuPosition[id])
		case 9: displayMenu(id, --g_menuPosition[id])
		default: client_cmd(id, "%s", g_menuCmd[g_menuPosition[id] * 8 + key])
	}
	
	return PLUGIN_HANDLED
}

public clientActionMenu(id, key)
{
	switch (key)
	{
		case 8: clientDisplayMenu(id, ++g_clientMenuPosition[id])
		case 9: clientDisplayMenu(id, --g_clientMenuPosition[id])
		default: client_cmd(id, "%s", g_clientMenuCmd[g_clientMenuPosition[id] * 8 + key])
	}
	
	return PLUGIN_HANDLED
}

displayMenu(id, pos)
{
	if (pos < 0)
		return

	new menuBody[512]
	new b = 0
	new start = pos * MENUITEMSPERPAGE

	if (start >= g_menusNumber)		// MENUS_NUMBER
		start = pos = g_menuPosition[id] = 0

	new len = format(menuBody, 511, 
	
	g_coloredMenus ? "\yAMX Mod X Menu\R%d/%d^n\w^n" : "AMX Mod X Menu %d/%d^n^n" , pos + 1, (g_menusNumber / MENUITEMSPERPAGE) + (((g_menusNumber % MENUITEMSPERPAGE) > 0) ? 1 : 0))

	new end = start + MENUITEMSPERPAGE
	new keys = MENU_KEY_0

	if (end > g_menusNumber)		// MENUS_NUMBER
		end = g_menusNumber			// MENUS_NUMBER

	for (new a = start; a < end; ++a)
	{
		if ( access(id, g_menuAccess[a]) && 
			((is_plugin_loaded(g_menuPlugin[a]) != -1) ||			// search plugins for registered name
			 (is_plugin_loaded(g_menuPlugin[a], true) != -1)))	// search plugins for filename
		{
			keys |= (1<<b)
			
			if (g_menuBodyPhrase[a])
				len += format(menuBody[len], 511-len, "%d. %L^n", ++b, id, g_menuBody[a])
			else
				len += format(menuBody[len], 511-len, "%d. %s^n", ++b, g_menuBody[a])
		} else {
			++b
			
			if (g_coloredMenus)
			{
				if (g_menuBodyPhrase[a])
					len += format(menuBody[len], 511-len, "\d%d. %L^n\w", b, id, g_menuBody[a])
				else
					len += format(menuBody[len], 511-len, "\d%d. %s^n\w", b, g_menuBody[a])
			} else {
				if (g_menuBodyPhrase[a])
					len += format(menuBody[len], 511-len, "#. %L^n", id, g_menuBody[a])
				else
					len += format(menuBody[len], 511-len, "#. %s^n", g_menuBody[a])
			}
		}
	}

	if (end != g_menusNumber)		// MENUS_NUMBER
	{
		format(menuBody[len], 511-len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT")
		keys |= MENU_KEY_9
	} else {
		format(menuBody[len], 511-len, "^n0. %L", id, pos ? "BACK" : "EXIT")
	}

	show_menu(id, keys, menuBody)
}

clientDisplayMenu(id, pos)
{
	if (pos < 0)
		return

	new menuBody[512]
	new b = 0
	new start = pos * MENUITEMSPERPAGE

	if (start >= g_clientMenusNumber)		// MENUS_NUMBER
		start = pos = g_clientMenuPosition[id] = 0

	new len = format(menuBody, 511, g_coloredMenus ? "\yAMX Mod X Client Menu\R%d/%d^n\w^n" : "AMX Mod X Client Menu %d/%d^n^n" , pos + 1, (g_clientMenusNumber / MENUITEMSPERPAGE) + (((g_clientMenusNumber % MENUITEMSPERPAGE) > 0) ? 1 : 0))

	new end = start + MENUITEMSPERPAGE
	new keys = MENU_KEY_0

	if (end > g_clientMenusNumber)			// MENUS_NUMBER
		end = g_clientMenusNumber			// MENUS_NUMBER

	for (new a = start; a < end; ++a)
	{
		if ( access(id, g_clientMenuAccess[a]) && 
			((is_plugin_loaded(g_clientMenuPlugin[a]) != -1) ||			// search plugins for registered name
			 (is_plugin_loaded(g_clientMenuPlugin[a], true) != -1)))		// search plugins for file name
		{
			keys |= (1<<b)
			
			if (g_clientMenuBodyPhrase[a])
				len += format(menuBody[len], 511-len, "%d. %L^n", ++b, id, g_clientMenuBody[a])
			else
				len += format(menuBody[len], 511-len, "%d. %s^n", ++b, g_clientMenuBody[a])
		} else {
			++b
			
			if (g_coloredMenus)
			{
				if (g_clientMenuBodyPhrase[a])
					len += format(menuBody[len], 511-len, "\d%d. %L^n\w", b, id, g_clientMenuBody[a])
				else
					len += format(menuBody[len], 511-len, "\d%d. %s^n\w", b, g_clientMenuBody[a])
			} else {
				if (g_clientMenuBodyPhrase[a])
					len += format(menuBody[len], 511-len, "#. %L^n", id, g_clientMenuBody[a])
				else
					len += format(menuBody[len], 511-len, "#. %s^n", g_clientMenuBody[a])
			}
		}
	}

	if (end != g_clientMenusNumber)			// MENUS_NUMBER
	{
		format(menuBody[len], 511-len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT")
		keys |= MENU_KEY_9
	}
	else {
		format(menuBody[len], 511-len, "^n0. %L", id, pos ? "BACK" : "EXIT")
	}

	show_menu(id, keys, menuBody)
}

public cmdMenu(id, level, cid)
{
	if (cmd_access(id, level, cid, 1))
		displayMenu(id, g_menuPosition[id] = 0)

	return PLUGIN_HANDLED
}
public clientCmdMenu(id, level, cid)
{
	if (cmd_access(id, level, cid, 1))
		clientDisplayMenu(id, g_clientMenuPosition[id] = 0)

	return PLUGIN_HANDLED
}

public addmenuitem_cmd(id, level, cid)
{
	if (!cmd_access(id, level, cid, 5))
		return PLUGIN_HANDLED

	// AddMenu(const menuBody[], const menuCmd[], const menuAccess, const menuPlugin[])
	new menuBody[STRINGSIZE], menuCmd[STRINGSIZE], flags[STRINGSIZE], menuAccess = 0, menuPlugin[STRINGSIZE]
	read_argv(1, menuBody, STRINGLENGTH)
	read_argv(2, menuCmd, STRINGLENGTH)
	read_argv(3, flags, STRINGLENGTH)
	menuAccess = read_flags(flags)
	read_argv(4, menuPlugin, STRINGLENGTH)

	AddMenu(menuBody, menuCmd, menuAccess, menuPlugin)

	return PLUGIN_HANDLED
}

public addclientmenuitem_cmd(id, level, cid)
{
	if (!cmd_access(id, level, cid, 5))
		return PLUGIN_HANDLED

	// AddMenu(const menuBody[], const menuCmd[], const menuAccess, const menuPlugin[])
	new menuBody[STRINGSIZE], menuCmd[STRINGSIZE], flags[STRINGSIZE], menuAccess = 0, menuPlugin[STRINGSIZE]
	read_argv(1, menuBody, STRINGLENGTH)
	read_argv(2, menuCmd, STRINGLENGTH)
	read_argv(3, flags, STRINGLENGTH)
	menuAccess = read_flags(flags)
	read_argv(4, menuPlugin, STRINGLENGTH)

	AddClientMenu(menuBody, menuCmd, menuAccess, menuPlugin)

	return PLUGIN_HANDLED
}

public plugin_init()
{
	register_plugin("Menus Front-End", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("menufront.txt")
	register_dictionary("common.txt")

	register_menucmd(register_menuid("AMX Mod X Menu"), 1023, "actionMenu")
	register_menucmd(register_menuid("AMX Mod X Client Menu"), 1023, "clientActionMenu")
	register_clcmd("amxmodmenu", "cmdMenu", ADMIN_MENU, "- displays menus")
	register_clcmd("amx_menu", "clientCmdMenu", 0, "- displays menus available to client")

	register_srvcmd("amx_addmenuitem", "addmenuitem_cmd", 0, "<menu text> <menu command> <access flags> <plugin name | plugin filename> - Add a menu item to Menus Front-End")
	register_srvcmd("amx_addclientmenuitem", "addclientmenuitem_cmd", 0, "<menu text> <menu command> <access flags> <plugin name | plugin filename> - Add a menu item to Client Menus Front-End")

	g_coloredMenus = colored_menus()


}
public plugin_cfg()
{
	AddDefaultMenus()

	new configs[128]
	get_configsdir(configs, 127)
	server_cmd("exec %s/custommenuitems.cfg", configs)
}
