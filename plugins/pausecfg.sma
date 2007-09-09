/* AMX Mod X
*   Pause Plugins Plugin
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

// Uncomment if you want to have two new commands
// amx_off - pause plugins not marked as unpauseable
// amx_on - enable plugins not marked as unpauseable
#define DIRECT_ONOFF

#define MAX_SYSTEM 32
 
new g_menuPos[33]
new g_fileToSave[64]
new g_coloredMenus
new g_Modified
new g_addCmd[] = "amx_pausecfg add ^"%s^""
new g_system[MAX_SYSTEM]
new g_systemNum

public plugin_init()
{
	register_plugin("Pause Plugins", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("pausecfg.txt")
	register_dictionary("common.txt")
	register_dictionary("admincmd.txt")
	
	register_concmd("amx_pausecfg", "cmdPlugin", ADMIN_CFG, "- list commands for pause/unpause management")
	register_clcmd("amx_pausecfgmenu", "cmdMenu", ADMIN_CFG, "- pause/unpause plugins with menu")
#if defined DIRECT_ONOFF
	register_concmd("amx_off", "cmdOFF", ADMIN_CFG, "- pauses some plugins")
	register_concmd("amx_on", "cmdON", ADMIN_CFG, "- unpauses some plugins")
#endif
	register_menucmd(register_menuid("Pause/Unpause Plugins"), 1023, "actionMenu")
	
	g_coloredMenus = colored_menus()
	get_configsdir(g_fileToSave, 63);
	format(g_fileToSave, 63, "%s/pausecfg.ini", g_fileToSave);

	return PLUGIN_CONTINUE
}

#if defined DIRECT_ONOFF
public cmdOFF(id, level, cid)
{
	if (cmd_access(id, level, cid, 1))
		pausePlugins(id)
	
	return PLUGIN_HANDLED
}

public cmdON(id, level, cid)
{
	if (cmd_access(id, level, cid, 1))
		unpausePlugins(id)
	
	return PLUGIN_HANDLED
}
#endif

public plugin_cfg()
{
	loadSettings(g_fileToSave)

	// Put here titles of plugins which you don't want to pause
	server_cmd(g_addCmd, "Admin Base")
	server_cmd(g_addCmd, "Admin Base (SQL)")
	server_cmd(g_addCmd, "Pause Plugins")
	server_cmd(g_addCmd, "TimeLeft")
	server_cmd(g_addCmd, "NextMap")
	server_cmd(g_addCmd, "Slots Reservation")
}

public actionMenu(id, key)
{
	switch (key)
	{
		case 6:
		{
			if (file_exists(g_fileToSave))
			{
				delete_file(g_fileToSave)
				client_print(id, print_chat, "* %L", id, "PAUSE_CONF_CLEARED")
			}
			else
				client_print(id, print_chat, "* %L", id, "PAUSE_ALR_CLEARED")
			
			displayMenu(id, g_menuPos[id])
		}
		case 7:
		{
			if (saveSettings(g_fileToSave))
			{
				g_Modified = 0
				client_print(id, print_chat, "* %L", id, "PAUSE_CONF_SAVED")
			}
			else
				client_print(id, print_chat, "* %L", id, "PAUSE_SAVE_FAILED")
			
			displayMenu(id, g_menuPos[id])
		}
		case 8: displayMenu(id, ++g_menuPos[id])
		case 9: displayMenu(id, --g_menuPos[id])
		default:
		{
			new option = g_menuPos[id] * 6 + key
			new file[32], status[2]
			
			get_plugin(option, file, 31, status, 0, status, 0, status, 0, status, 1)
			
			switch (status[0])
			{
				// "running"
				case 'r': pause("ac", file)
				
				// "debug"
				case 'd': pause("ac", file)
				
				// "paused"
				case 'p':
				{
					g_Modified = 1
					unpause("ac", file)
				}
				
				// "stopped"
				case 's':
				{
					client_print(id, print_chat, "%L", id, "CANT_UNPAUSE_PLUGIN", file);
				}
			}
			
			displayMenu(id, g_menuPos[id])
		}
	}
	
	return PLUGIN_HANDLED
}

getStatus(id, code, &statusCode, lStatus[], lLen)
{
	switch (code)
	{
		// "running"
		case 'r':
		{
			statusCode = 'O'
			format(lStatus, lLen, "%L", id, "ON")
		}
		
		// "debug"
		case 'd':
		{
			statusCode = 'O'
			format(lStatus, lLen, "%L", id, "ON")
		}
		
		// "stopped"
		case 's':
		{
			statusCode = 'S'
			format(lStatus, lLen, "%L", id, "STOPPED")
		}	
		
		// "paused"
		case 'p':
		{
			statusCode = 'O'
			format(lStatus, lLen, "%L", id, "OFF")
		}
		
		// "bad load"
		case 'b':
		{
			statusCode = 'E'
			format(lStatus, lLen, "%L", id, "ERROR")
		}
		default:
		{
			statusCode = 'L'
			format(lStatus, lLen, "%L", id, "LOCKED")
		}
	}
}

isSystem(id)
{
	for (new a = 0; a < g_systemNum; ++a)
		if (g_system[a] == id)
			return 1
	return 0
}

displayMenu(id, pos)
{
	if (pos < 0) return

	new filename[32], title[32], status[8], statusCode
	new datanum = get_pluginsnum()
	new menu_body[512], start = pos * 6, k = 0
	
	if (start >= datanum)
		start = pos = g_menuPos[id] = 0
	
	new len = format(menu_body, 511, g_coloredMenus ? "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n", id, "PAUSE_UNPAUSE", pos + 1, ((datanum / 6) + ((datanum % 6) ? 1 : 0)))
	new end = start + 6, keys = MENU_KEY_0|MENU_KEY_8|MENU_KEY_7
	
	if (end > datanum)
		end = datanum
	
	for (new a = start; a < end; ++a)
	{
		get_plugin(a, filename, 31, title, 31, status, 0, status, 0, status, 1)
		getStatus(id, status[0], statusCode, status, 7)
		
		if (isSystem(a) || (statusCode != 'O' && statusCode != 'S'))
		{
			if (g_coloredMenus)
			{
				len += format(menu_body[len], 511-len, "\d%d. %s\R%s^n\w", ++k, title, status)
			} else {
				++k
				len += format(menu_body[len], 511-len, "#. %s %s^n", title, status)
			}
		} else {
			keys |= (1<<k)
			len += format(menu_body[len], 511-len, g_coloredMenus ? "%d. %s\y\R%s^n\w" : "%d. %s %s^n", ++k, title, status)
		}
	}
	
	len += format(menu_body[len], 511-len, "^n7. %L^n", id, "CLEAR_PAUSED")
	len += format(menu_body[len], 511-len, g_coloredMenus ? "8. %L \y\R%s^n\w" : "8. %L %s^n", id, "SAVE_PAUSED", g_Modified ? "*" : "")

	if (end != datanum)
	{
		format(menu_body[len], 511-len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT")
		keys |= MENU_KEY_9
	}
	else
		format(menu_body[len], 511-len, "^n0. %L", id, pos ? "BACK" : "EXIT")
	
	show_menu(id, keys, menu_body, -1, "Pause/Unpause Plugins")
}

public cmdMenu(id, level, cid)
{
	if (cmd_access(id, level, cid, 1))
		displayMenu(id, g_menuPos[id] = 0)
	
	return PLUGIN_HANDLED
}

pausePlugins(id)
{
	new filename[32], title[32], status[2]
	new count = 0, imax = get_pluginsnum()
	
	for (new a = 0; a < imax; ++a)
	{
		get_plugin(a, filename, 31, title, 31, status, 0, status, 0, status, 1)
		
		if (!isSystem(a) && status[0] == 'r' && pause("ac", filename))
		{
			//console_print(id, "Pausing %s (file ^"%s^")", title, filename)
			++count
		}
	}
	
	console_print(id, "%L", id, (count == 1) ? "PAUSED_PLUGIN" : "PAUSED_PLUGINS", count)
}

unpausePlugins(id)
{
	new filename[32], title[32], status[2]
	new count = 0, imax = get_pluginsnum()
	
	for (new a = 0; a < imax; ++a)
	{
		get_plugin(a, filename, 31, title, 31, status, 0, status, 0, status, 1)
		
		if (!isSystem(a) && status[0] == 'p' && unpause("ac", filename))
		{
			//console_print(id, "Unpausing %s (file ^"%s^")", title, filename)
			++count
		}
	}
	
	console_print(id, "%L", id, (count == 1) ? "UNPAUSED_PLUGIN" : "UNPAUSED_PLUGINS", count)
}

findPluginByFile(arg[32], &len)
{
	new name[32], title[32], status[2]
	new inum = get_pluginsnum()
	
	for (new a = 0; a < inum; ++a)
	{
		get_plugin(a, name, 31, title, 31, status, 0, status, 0, status, 1)
		
		if (equali(name, arg, len) && (
			status[0] == 'r' ||	/*running*/
			status[0] == 'p' ||	/*paused*/
			status[0] == 's' ||	/*stopped*/
			status[0] == 'd' ))	/*debug*/
		{
			len = copy(arg, 31, name)
			return a
		}
	}
	
	return -1
}

findPluginByTitle(name[], file[], len)
{
	new title[32], status[2]
	new inum = get_pluginsnum()
	
	for (new a = 0; a < inum; ++a)
	{
		get_plugin(a, file, len, title, 31, status, 0, status, 0, status, 1)
		
		if (equali(title, name))
			return a
	}
	
	return -1
}

public cmdPlugin(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
		return PLUGIN_HANDLED
	
	new cmds[32]
	read_argv(1, cmds, 31)

	if (equal(cmds, "add") && read_argc() > 2)
	{
		read_argv(2, cmds, 31)
		new file[2]

		if ((g_system[g_systemNum] = findPluginByTitle(cmds, file, 0)) != -1)
		{
			if (g_systemNum < MAX_SYSTEM)
				g_systemNum++
			else
				console_print(id, "%L", id, "CANT_MARK_MORE")
		}
	}
	else if (equal(cmds, "off"))
	{
		pausePlugins(id)
	}
	else if (equal(cmds, "on"))
	{
		unpausePlugins(id)
	}
	else if (equal(cmds, "save"))
	{
		if (saveSettings(g_fileToSave))
		{
			g_Modified = 0
			console_print(id, "%L", id, "PAUSE_CONF_SAVED")
		}
		else
			console_print(id, "%L", id, "PAUSE_SAVE_FAILED")
	}
	else if (equal(cmds, "clear"))
	{
		if (file_exists(g_fileToSave))
		{
			delete_file(g_fileToSave)
			console_print(id, "%L", id, "PAUSE_CONF_CLEARED")
		}
		else
			console_print(id, "%L", id, "PAUSE_ALR_CLEARED")
	}
	else if (equal(cmds, "pause"))
	{
		new arg[32], a, len = read_argv(2, arg, 31)
		
		if (len && ((a = findPluginByFile(arg, len)) != -1) && !isSystem(a) && pause("ac", arg))
			console_print(id, "%L %L", id, "PAUSE_PLUGIN_MATCH", arg, id, "PAUSED")
		else
			console_print(id, "%L", id, "PAUSE_COULDNT_FIND", arg)
	}
	else if (equal(cmds, "enable"))
	{
		new arg[32], a, len = read_argv(2, arg, 31)
		
		if (len && (a = findPluginByFile(arg, len)) != -1 && !isSystem(a))
		{
			if (unpause("ac", arg))
			{
				console_print(id, "%L %L", id, "PAUSE_PLUGIN_MATCH", arg, id, "UNPAUSED")
			}
			else
			{
				console_print(id, "%L", id, "CANT_UNPAUSE_PLUGIN", arg)
			}
		}
		else
		{
			console_print(id, "%L", id, "PAUSE_COULDNT_FIND", arg)
		}
	}
	else if (equal(cmds, "stop"))
	{
		new arg[32], a, len = read_argv(2, arg, 31)
		
		if (len && (a = findPluginByFile(arg, len)) != -1 && !isSystem(a) && pause("dc", arg))
		{
			g_Modified = 1
			console_print(id, "%L %L", id, "PAUSE_PLUGIN_MATCH", arg, id, "STOPPED")
		}
		else
			console_print(id, "%L", id, "PAUSE_COULDNT_FIND", arg)
	}
	else if (equal(cmds, "list"))
	{
		new lName[32], lVersion[32], lAuthor[32], lFile[32], lStatus[32]
		
		format(lName, 31, "%L", id, "NAME")
		format(lVersion, 31, "%L", id, "VERSION")
		format(lAuthor, 31, "%L", id, "AUTHOR")
		format(lFile, 31, "%L", id, "FILE")
		format(lStatus, 31, "%L", id, "STATUS")
		
		new arg1[8], running = 0
		new start = read_argv(2, arg1, 7) ? str_to_num(arg1) : 1
		
		if (--start < 0)
			start = 0
		
		new plgnum = get_pluginsnum()
		
		if (start >= plgnum)
			start = plgnum - 1
		
		console_print(id, "^n----- %L -----", id, "PAUSE_LOADED")
		console_print(id, "       %-18.17s %-8.7s %-17.16s %-16.15s %-9.8s", lName, lVersion, lAuthor, lFile, lStatus)
		
		new	plugin[32], title[32], version[16], author[32], status[16]
		new end = start + 10
		
		if (end > plgnum) end = plgnum
		
		for (new a = start; a < end; ++a)
		{
			get_plugin(a, plugin, 31, title, 31, version, 15, author, 31, status, 15)
			if (status[0] == 'r') ++running
			console_print(id, " [%3d] %-18.17s %-8.7s %-17.16s %-16.15s %-9.8s", a + 1, title, version, author, plugin, status)
		}
		
		console_print(id, "----- %L -----", id, "PAUSE_ENTRIES", start + 1, end, plgnum, running)
		
		if (end < plgnum)
			console_print(id, "----- %L -----", id, "PAUSE_USE_MORE", end + 1)
		else
			console_print(id, "----- %L -----", id, "PAUSE_USE_BEGIN")
	} else {
		console_print(id, "%L", id, "PAUSE_USAGE")
		console_print(id, "%L:", id, "PAUSE_COMMANDS")
		console_print(id, "%L", id, "COM_PAUSE_OFF")
		console_print(id, "%L", id, "COM_PAUSE_ON")
		console_print(id, "%L", id, "COM_PAUSE_STOP")
		console_print(id, "%L", id, "COM_PAUSE_PAUSE")
		console_print(id, "%L", id, "COM_PAUSE_ENABLE")
		console_print(id, "%L", id, "COM_PAUSE_SAVE_PAUSED")
		console_print(id, "%L", id, "COM_PAUSE_CLEAR_PAUSED")
		console_print(id, "%L", id, "COM_PAUSE_LIST")
		console_print(id, "%L", id, "COM_PAUSE_ADD")
	}
	
	return PLUGIN_HANDLED
}

saveSettings(filename[])
{
	if (file_exists(filename))
		delete_file(filename)

	new text[256], file[32], title[32], status[2]
	new inum = get_pluginsnum()

	if (!write_file(filename, ";Generated by Pause Plugins Plugin. Do not modify!^n;Title Filename"))
		return 0
	
	for (new a = 0; a < inum; ++a)
	{
		get_plugin(a, file, 31, title, 31, status, 0, status, 0, status, 1)
		
		// "paused"
		if (status[0] == 'p')
		{
			format(text, 255, "^"%s^" ;%s", title, file)
			write_file(filename, text)
		}
	}
	
	return 1
}

loadSettings(filename[])
{
	if (!file_exists(filename))
		return 0
	
	new name[256], file[32], i, pos = 0
	
	while (read_file(filename, pos++, name, 255, i))
	{
		if (name[0] != ';' && parse(name, name, 31) && (i = findPluginByTitle(name, file, 31) != -1))
			pause("ac", file)
	}
	
	return 1
}
