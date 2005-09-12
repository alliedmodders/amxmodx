/* AMX Mod X script.
*   Multilingual System Plugin
*
* by the AMX Mod X Development Team
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

#define DISPLAY_MSG		// Comment to disable message on join

new g_menuLang[33][2]
new g_serverLang
new g_langNum
new g_coloredMenus

public plugin_init()
{
	register_plugin("Multi-Lingual System", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("multilingual.txt")
	register_dictionary("common.txt")
	register_dictionary("languages.txt")
	
	register_cvar("amx_language", "en", FCVAR_SERVER|FCVAR_EXTDLL|FCVAR_SPONLY)
	//Set to zero to disable client effects
	register_cvar("amx_client_languages", "1")
	register_concmd("amx_setlang", "cmdLang", ADMIN_CFG, "<language>")
	register_clcmd("amx_langmenu", "cmdLangMenu", ADMIN_ALL)
	register_menu("Language Menu", 1023, "actionMenu")

	new lang[3]
	
	if (vaultdata_exists("server_language"))
	{
		get_vaultdata("server_language", lang, 2)
	} else {
		copy(lang, 2, "en")
		set_vaultdata("server_language", lang)
	}
	
	set_cvar_string("amx_language", lang)

	g_langNum = get_langsnum()
	g_serverLang = get_lang_id(lang)
	g_coloredMenus = colored_menus()
}

#if defined DISPLAY_MSG
public client_putinserver(id)
{
	if (get_cvar_num("amx_client_languages") && !is_user_bot(id))
		set_task(10.0, "dispInfo", id)
}

public client_disconnect(id)
{
	remove_task(id)
}

public dispInfo(id)
{
	if (get_cvar_num("amx_client_languages"))
		client_print(id, print_chat, "%L", id, "TYPE_LANGMENU")
}
#endif

public cmdLang(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
		return PLUGIN_HANDLED

	new arg[3]
	read_argv(1, arg, 2)

	if (!lang_exists(arg))
	{
		console_print(id, "[AMXX] %L", id, "LANG_NOT_EXISTS")
		return PLUGIN_HANDLED
	}

	set_vaultdata("server_language", arg)
	set_cvar_string("amx_language", arg)
	g_serverLang = get_lang_id(arg)

	return PLUGIN_HANDLED
}

public cmdLangMenu(id, level, cid)
{
	new buffer[3]

	if (!get_cvar_num("amx_client_languages"))
	{
		client_print(id, print_console, "[AMXX] %L", LANG_SERVER, "LANG_MENU_DISABLED")
		return PLUGIN_HANDLED
	}

	get_user_info(id, "lang", buffer, 2)
	g_menuLang[id][0] = get_lang_id(buffer)
	g_menuLang[id][1] = g_serverLang

	showMenu(id)

	return PLUGIN_HANDLED
}

showMenu(id)
{
	if (!get_cvar_num("amx_client_languages"))
		return PLUGIN_HANDLED
	
	new menuBody[512], pLang[3]

	get_lang(g_menuLang[id][0], pLang)

	new len = format(menuBody, 511, (g_coloredMenus ? "\y%L\w^n^n" : "%L^n^n"), id, "LANG_MENU")
	
	len += format(menuBody[len], 511-len, (g_coloredMenus ? "1. %L\R\r%L\w^n" : "1. %L %L^n"), id, "PERSO_LANG", pLang, "LANG_NAME")

	if (access(id, ADMIN_CFG))
	{
		new sLang[3]
		
		get_lang(g_menuLang[id][1], sLang)
		len += format(menuBody[len], 511-len, (g_coloredMenus ? "2. %L\R\r%L\w^n^n" : "2. %L %L^n^n"), id, "SERVER_LANG", sLang, "LANG_NAME")
		len += format(menuBody[len], 511-len, "3. %L", id, "SAVE_LANG")
	} else {
		len += format(menuBody[len], 511-len, "^n2. %L", id, "SAVE_LANG")
	}
	
	format(menuBody[len], 511-len, "^n^n0. %L", id, "EXIT")

	show_menu(id, MENU_KEY_0|MENU_KEY_1|MENU_KEY_2|MENU_KEY_3, menuBody, -1, "Language Menu")

	return 1
}

public actionMenu(id, key)
{
	if (!get_cvar_num("amx_client_languages"))
		return 0

	new isAdmin = access(id, ADMIN_CFG)

	if (key == 0)
	{
		if (g_menuLang[id][0] < (g_langNum-1))
			g_menuLang[id][0]++
		else
			g_menuLang[id][0] = 0
		
		showMenu(id)
	}

	if (isAdmin && (key == 1))
	{
		if (g_menuLang[id][1] < (g_langNum - 1))
			g_menuLang[id][1]++
		else
			g_menuLang[id][1] = 0
		
		showMenu(id)
	}

	new pLang[3], pLang_old[3], sLang[3], sLang_old[3], lName[64]
	
	get_lang(g_menuLang[id][0], pLang)
	get_lang(g_menuLang[id][1], sLang)
	get_user_info(id, "lang", pLang_old, 2)
	get_lang(g_serverLang, sLang_old)

	if (isAdmin && (key == 2) && !equali(sLang, sLang_old))
	{
		set_vaultdata("server_language", sLang)
		set_cvar_string("amx_language", sLang)
		g_serverLang = g_menuLang[id][1]
		format(lName, 63, "%L", sLang, "LANG_NAME")
		client_print(id, print_chat, "%L", pLang, "SET_LANG_SERVER", lName)
	}

	if (!equali(pLang, pLang_old) && ((isAdmin && (key == 2)) || (!isAdmin && (key == 1))))
	{
		client_cmd(id, "setinfo ^"lang^" ^"%s^"", pLang)
		format(lName, 63, "%L", pLang, "LANG_NAME")
		client_print(id, print_chat, "%L", pLang, "SET_LANG_USER", lName)
	}

	return 0
}

get_lang_id(lang[])
{
	new tLang[3]
	
	for (new i = 0; i < g_langNum; i++)
	{
		get_lang(i, tLang)
		if (equali(tLang, lang))
			return i
	}

	return 0
}
