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

new g_menuLang[MAX_PLAYERS]
new g_langNum
new g_coloredMenus

new g_cvarDisplayClientMessage;
new g_cvarClientLanguages;

public plugin_init()
{
	register_plugin("Multi-Lingual System", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("multilingual.txt")
	register_dictionary("common.txt")
	register_dictionary("languages.txt")
	
	g_cvarClientLanguages = register_cvar("amx_client_languages", "1")
	g_cvarDisplayClientMessage = register_cvar("amx_language_display_msg", "1")
	register_clcmd("amx_langmenu", "cmdLangMenu", ADMIN_ALL)
	register_menu("Language Menu", 1023, "actionMenu")
	
	g_langNum = get_langsnum()
	g_coloredMenus = colored_menus()
}

public client_putinserver(id)
{
	if (get_pcvar_num(g_cvarDisplayClientMessage) && get_pcvar_num(g_cvarClientLanguages) && !is_user_bot(id))
	{
		set_task(10.0, "dispInfo", id)
	}
}

public client_disconnect(id)
{
	remove_task(id)
}

public dispInfo(id)
{
	client_print(id, print_chat, "%L", id, "TYPE_LANGMENU")
}

public cmdLangMenu(id, level, cid)
{
	if (!get_pcvar_num(g_cvarClientLanguages))
	{
		client_print(id, print_console, "[AMXX] %L", LANG_SERVER, "LANG_MENU_DISABLED")
		return PLUGIN_HANDLED
	}
	
	new buffer[3]
	get_user_info(id, "lang", buffer, charsmax(buffer))
	g_menuLang[id] = get_lang_id(buffer)
	
	showMenu(id)
	
	return PLUGIN_HANDLED
}

showMenu(id)
{
	if (!get_pcvar_num(g_cvarClientLanguages))
	{
		return
	}
	
	new menuBody[512], pLang[3]
	
	get_lang(g_menuLang[id], pLang)
	
	new len = formatex(menuBody, charsmax(menuBody), (g_coloredMenus ? "\y%L\w^n^n" : "%L^n^n"), id, "LANG_MENU")
	
	len += formatex(menuBody[len], charsmax(menuBody) - len, (g_coloredMenus ? "1. %L\R\r%L\w^n" : "1. %L %L^n"), id, "PERSO_LANG", pLang, "LANG_NAME")
	len += formatex(menuBody[len], charsmax(menuBody) - len, "^n2. %L", id, "SAVE_LANG")
	formatex(menuBody[len], charsmax(menuBody) - len, "^n^n0. %L", id, "EXIT")
	
	show_menu(id, MENU_KEY_0|MENU_KEY_1|MENU_KEY_2, menuBody, -1, "Language Menu")
}

public actionMenu(id, key)
{
	if (!get_pcvar_num(g_cvarClientLanguages))
	{
		return 0
	}

	if (key == 0)
	{
		if (g_menuLang[id] < (g_langNum - 1))
		{
			g_menuLang[id]++
		}
		else
		{
			g_menuLang[id] = 0
		}
		
		showMenu(id)
	}
	else if(key == 1)
	{
		new pLang[3], pLang_old[3]
		
		get_lang(g_menuLang[id], pLang)
		get_user_info(id, "lang", pLang_old, charsmax(pLang_old))
		
		if (!equali(pLang, pLang_old))
		{
			client_cmd(id, "setinfo ^"lang^" ^"%s^"", pLang)
			set_user_info(id, "lang", pLang); // In case setinfo breaks (slowhacking and all), this will at least be a fallback while the user is connect
			
			new lName[64]
			formatex(lName, charsmax(lName), "%L", pLang, "LANG_NAME")
			client_print(id, print_chat, "%L", pLang, "SET_LANG_USER", lName)
		}
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
		{
			return i
		}
	}

	return 0
}
