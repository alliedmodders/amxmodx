/* AMX Mod X
*   Commands Menu Plugin
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

// Precache sounds from speech.ini - comment this line to disable
#define PRECACHE_SPEECHINI

/* Commands Menus */

#define MAX_CMDS_LAYERS 3

new g_cmdMenuName[MAX_CMDS_LAYERS][] =
{
	"CMD_MENU",
	"CONF_MENU",
	"SPE_MENU"
}

new g_cmdMenuCmd[MAX_CMDS_LAYERS][] =
{
	"amx_cmdmenu",
	"amx_cfgmenu",
	"amx_speechmenu"
}

new g_cmdMenuCfg[MAX_CMDS_LAYERS][] =
{
	"cmds.ini", 
	"configs.ini",
	"speech.ini"
}

new g_cmdMenuHelp[MAX_CMDS_LAYERS][] =
{
	"- displays commands menu",
	"- displays configs menu",
	"- displays speech menu"
}

/* End of Commands Menu */

#define MAX_CMDS    64
#define MAX_CVARS   48

new g_cmdName[MAX_CMDS*MAX_CMDS_LAYERS][32]
new g_cmdCmd[MAX_CMDS*MAX_CMDS_LAYERS][64]
new g_cmdMisc[MAX_CMDS*MAX_CMDS_LAYERS][2]
new g_cmdNum[MAX_CMDS_LAYERS]

new g_cvarNames[MAX_CVARS][32]
new g_cvarMisc[MAX_CVARS][3]
new g_cvarCmd[MAX_CVARS*5][32]
new g_cvarCmdNum
new g_cvarNum

new g_menuPosition[33]
new g_menuSelect[33][MAX_CMDS]
new g_menuSelectNum[33]
new g_menuLayer[33]

new g_coloredMenus


public plugin_init()
{
	register_plugin("Commands Menu", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("cmdmenu.txt")
	register_dictionary("common.txt")

	new configsDir[64], config[64]
	get_configsdir(configsDir, 63)
	
	for (new a = 0; a < MAX_CMDS_LAYERS; ++a)
	{
		new MenuName[64]
		
		format(MenuName, 63, "%L", "en", g_cmdMenuName[a])
		register_menucmd(register_menuid(MenuName), 1023, "actionCmdMenu")
		register_clcmd(g_cmdMenuCmd[a], "cmdCmdMenu", ADMIN_MENU, g_cmdMenuHelp[a])
		format(config, 63, "%s/%s", configsDir, g_cmdMenuCfg[a])
		loadCmdSettings(config, a)
	}

	register_menucmd(register_menuid("Cvars Menu"), 1023, "actionCvarMenu")
	register_clcmd("amx_cvarmenu", "cmdCvarMenu", ADMIN_CVAR, "- displays cvars menu")

	new cvars_ini_file[64];
	format(cvars_ini_file, 63, "%s/%s", configsDir, "cvars.ini");
	loadCvarSettings(cvars_ini_file)

	g_coloredMenus = colored_menus()
}

#if defined PRECACHE_SPEECHINI
public plugin_precache( )
{
	new configsDir[64], config[64];
	get_configsdir( configsDir, 63 );
	formatex( config, 63, "%s/%s", configsDir, "speech.ini" );

	new fp = fopen( config, "rt" );			// Read file as text

	if ( ! fp )					// File doesn't exists
		return 0;

	new szText[256];
	new line = 0;
	new szName[32], szSound[128], sndExt[5];
	new field1[32], field2[64], field3[64];
	new fieldNums = 0;

	while ( line < MAX_CMDS && ! feof( fp ) )	// Loop till MAX_CMDS or EOF
	{
		fgets( fp, szText, 255 );               // Store line content

		/* Strips newline */
		new len = strlen( szText );
		if ( len != 0 && szText[len-1] == '^n' )		// len != 0 because if the last line of the file is empty, there's no newline
			szText[--len] = 0;

		if ( len == 0 || szText[0] == ';' || szText[0] == '/' )   // Line is empty or a comment
			continue;

		parse( szText, szName, 31, szSound, 127 );
		fieldNums = parse( szSound, field1, 31, field2, 63, field3, 63 );
		if ( fieldNums == 2 && field1[0] == 's' )							// .wav (spk)
		{
			copy( szSound, 127, field2 );
			copy( sndExt, 4, ".wav" );
		}
		else if ( fieldNums == 3 && field1[0] == 'm' && ( field2[0] == 'p' || field2[0] == 'l' ) )	// .mp3 (mp3 play | mp3 loop)
		{
			copy( szSound, 127, field3 );
			copy( sndExt, 4, ".mp3" );
		}
		else												// WTH is this sound, drop it.
			continue;

		replace_all( szSound, 127, "\'", "" );								// Strips all ugly (and sometimes useless) \'

		if ( szSound[0] == '/' )
				replace( szSound, 127, "/", "" );						// Strip leading slash

		if ( sndExt[1] == 'm' || ( ! equali( szSound, "vox", 3 ) && ! equali( szSound, "fvox", 4 ) && ! equali( szSound, "barney", 6 ) && ! equali( szSound, "hgrunt", 6 ) ) )
		{
			// SzSound is a mp3, or a custom wav (not a vox, fvox, or default sound from HL pak)
			if ( !equali( szSound[strlen(szSound)-4], sndExt ) )
				add( szSound, 127, sndExt );			// Add filetype extension if it isn't already specified
			if ( sndExt[1] == 'w' )
				format( szSound, 127, "sound/%s", szSound );	// spk basedir is $moddir/sound, but mp3 play is $moddir, fix this for the file_exists check
			if ( file_exists( szSound ) )
			{
				if ( sndExt[1] == 'm')
				{
					precache_generic( szSound );		// mp3
				}
				else
				{
					replace( szSound, 127, "sound/", "" );	// wav, strip the leading sound/ we added for our file_exists check
					precache_sound( szSound );
				}
			}
		}
		line++
	}
	fclose( fp );								// Close file
	return line;
}
#endif

/* Commands menu */

public actionCmdMenu(id, key)
{
	switch (key)
	{
		case 8: displayCmdMenu(id, ++g_menuPosition[id])
		case 9: displayCmdMenu(id, --g_menuPosition[id])
		default:
		{
			new option = g_menuSelect[id][g_menuPosition[id] * 8 + key]
			new flags = g_cmdMisc[option][1]
			
			if (flags & 1)
				server_cmd("%s", g_cmdCmd[option])
			else if (flags & 2)
				client_cmd(id, "%s", g_cmdCmd[option])
			else if (flags & 4)
				client_cmd(0, "%s", g_cmdCmd[option])
			
			if (flags & 8)
				displayCmdMenu(id, g_menuPosition[id])
		}
	}
	
	return PLUGIN_HANDLED
}

displayCmdMenu(id, pos)
{
	if (pos < 0)
		return

	new menuBody[512]
	new b = 0
	new start = pos * 8

	if (start >= g_menuSelectNum[id])
		start = pos = g_menuPosition[id] = 0
	
	new limit = (g_menuSelectNum[id] / 8 + ((g_menuSelectNum[id] % 8)))
	new len = format(menuBody, 511, g_coloredMenus ? "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n", id, g_cmdMenuName[g_menuLayer[id]], pos + 1, (limit == 0) ? 1 : limit)
	new end = start + 8
	new keys = MENU_KEY_0

	if (end > g_menuSelectNum[id])
		end = g_menuSelectNum[id]

	for (new a = start; a < end; ++a)
	{
		if (g_cmdCmd[g_menuSelect[id][a]][0] == '-')
		{
			if (g_coloredMenus)
				len += format(menuBody[len], 511-len, "\d%s^n\w", g_cmdName[g_menuSelect[id][a]])
			else
				len += format(menuBody[len], 511-len, "%s^n", g_cmdName[g_menuSelect[id][a]])
			++b
		} else {
			keys |= (1<<b)
			len += format(menuBody[len], 511-len, "%d. %s^n", ++b, g_cmdName[g_menuSelect[id][a]])
		}
	}

	if (end != g_menuSelectNum[id])
	{
		format(menuBody[len], 511-len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT")
		keys |= MENU_KEY_9
	}
	else
		format(menuBody[len], 511-len, "^n0. %L", id, pos ? "BACK" : "EXIT")
	
	new MenuName[64]
	
	format(MenuName, 63, "%L", "en", g_cmdMenuName[g_menuLayer[id]])
	show_menu(id, keys, menuBody, -1, MenuName)
}

public cmdCmdMenu(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
		return PLUGIN_HANDLED

	new szCmd[32]
	read_argv(0, szCmd, 31)
	new lvl = 0

	while (lvl < MAX_CMDS_LAYERS)
	{
		if (equal(g_cmdMenuCmd[lvl], szCmd))
			break
		++lvl
	}

	g_menuLayer[id] = lvl
	g_menuSelectNum[id] = 0

	new a = lvl * MAX_CMDS
	new d, c = 0

	while (c < g_cmdNum[lvl])
	{
		d = a + c
		
		if (access(id, g_cmdMisc[d][0]))
		{
			g_menuSelect[id][g_menuSelectNum[id]++] = d
		}
		
		++c
	}

	displayCmdMenu(id, g_menuPosition[id] = 0)
	
	return PLUGIN_HANDLED
}

loadCmdSettings(szFilename[], level)
{
	if (!file_exists(szFilename)) 
		return 0

	new text[256], szFlags[32], szAccess[32]
	new a, pos = 0, c, d = level * MAX_CMDS

	while (g_cmdNum[level] < MAX_CMDS && read_file(szFilename, pos++, text, 255, a))
	{
		if (text[0] == ';') continue
		c = d + g_cmdNum[level]
		
		if (parse(text, g_cmdName[c], 31, g_cmdCmd[c], 63, szFlags, 31, szAccess, 31) > 3)
		{
			while (replace(g_cmdCmd[c], 63, "\'", "^""))
			{
				// do nothing
			}
			
			g_cmdMisc[c][1] = read_flags(szFlags)
			g_cmdMisc[c][0] = read_flags(szAccess)
			g_cmdNum[level]++ 
		}
	}

	return 1
}

/* Cvars menu */

public actionCvarMenu(id, key)
{
	switch (key)
	{
		case 8: displayCvarMenu(id, ++g_menuPosition[id])
		case 9: displayCvarMenu(id, --g_menuPosition[id])
		default:
		{
			new option = g_menuSelect[id][g_menuPosition[id] * 8 + key]
			new szValue[32]
			
			get_cvar_string(g_cvarNames[option], szValue, 31)
			
			new end = g_cvarMisc[option][2]
			new start = g_cvarMisc[option][1]

			for (new i = start; ; ++i)
			{
				if (i < end)
				{
					if (equal(szValue, g_cvarCmd[i]))
					{
						if (++i >= end)
						{
							i = start
						}
						
						set_cvar_string(g_cvarNames[option], g_cvarCmd[i])
						break
					}
				} else {
					set_cvar_string(g_cvarNames[option], g_cvarCmd[start])
					break
				}
			}
			displayCvarMenu(id, g_menuPosition[id])
		}
	}
	
	return PLUGIN_HANDLED
}

displayCvarMenu(id, pos)
{
	if (pos < 0)
		return

	new menuBody[512]
	new b = 0
	new start = pos * 8

	if (start >= g_menuSelectNum[id])
		start = pos = g_menuPosition[id] = 0

	new len = format(menuBody, 511, g_coloredMenus ? "\yCvars Menu\R%d/%d^n\w^n" : "Cvars Menu %d/%d^n^n", pos + 1, (g_menuSelectNum[id] / 8 + ((g_menuSelectNum[id] % 8) ? 1 : 0)))

	new end = start + 8
	new keys = MENU_KEY_0
	new szValue[64]

	if (end > g_menuSelectNum[id])
		end = g_menuSelectNum[id]

	for (new a = start; a < end; ++a)
	{
		get_cvar_string(g_cvarNames[g_menuSelect[id][a]], szValue, 31)
		keys |= (1<<b)
		++b
		
		if (g_coloredMenus)
			len += format(menuBody[len], 511-len, "%d. %s\R%s^n\w", b, g_cvarNames[g_menuSelect[id][a]], szValue)
		else
			len += format(menuBody[len], 511-len, "%d. %s    %s^n", b, g_cvarNames[g_menuSelect[id][a]], szValue)
	}

	if (end != g_menuSelectNum[id])
	{
		format(menuBody[len], 511-len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT")
		keys |= MENU_KEY_9
	}
	else
		format(menuBody[len], 511-len, "^n0. %L", id, pos ? "BACK" : "EXIT")
	
	show_menu(id, keys, menuBody)
}

public cmdCvarMenu(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
		return PLUGIN_HANDLED

	g_menuSelectNum[id] = 0

	for (new a = 0; a < g_cvarNum; ++a)
		if (access(id, g_cvarMisc[a][0]))
			g_menuSelect[id][g_menuSelectNum[id]++] = a

	displayCvarMenu(id, g_menuPosition[id] = 0)

	return PLUGIN_HANDLED
}

loadCvarSettings(szFilename[])
{
	if (!file_exists(szFilename)) 
		return 0

	new text[256], szValues[12][32]
	new inum, a, pos = 0
	new cvar_values = MAX_CVARS * 5
	
	// a b c d
	while (g_cvarNum < MAX_CVARS && read_file(szFilename, pos++, text, 255, a))
	{
		if (text[0] == ';') continue
		
		inum = parse(text, g_cvarNames[g_cvarNum], 31, 
		szValues[0], 31, szValues[1], 31, szValues[2], 31, 
		szValues[3], 31, szValues[4], 31, szValues[5], 31, 
		szValues[6], 31, szValues[7], 31, szValues[8], 31, 
		szValues[9], 31, szValues[10], 31, szValues[11], 31)

		inum -= 2
		if (inum < 2) continue
		g_cvarMisc[g_cvarNum][1] = g_cvarCmdNum

		for (a = 0; a < inum && g_cvarCmdNum < cvar_values; ++a)
		{
			while (replace(szValues[a], 31, "\'", "^""))
			{
				// do nothing
			}
			
			copy(g_cvarCmd[g_cvarCmdNum], 31, szValues[a])
			g_cvarCmdNum++
		}

		g_cvarMisc[g_cvarNum][2] = g_cvarCmdNum
		g_cvarMisc[g_cvarNum][0] = read_flags(szValues[inum])
		g_cvarNum++
	}

	return 1
}
