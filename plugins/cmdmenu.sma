// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Commands Menu Plugin
//

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
};

new g_cmdMenuCmd[MAX_CMDS_LAYERS][] =
{
	"amx_cmdmenu",
	"amx_cfgmenu",
	"amx_speechmenu"
};

new g_cmdMenuCfg[MAX_CMDS_LAYERS][] =
{
	"cmds.ini", 
	"configs.ini",
	"speech.ini"
};

new g_cmdMenuHelp[MAX_CMDS_LAYERS][] =
{
	"- displays commands menu",
	"- displays configs menu",
	"- displays speech menu"
};

/* End of Commands Menu */

#define MAX_CMDS    64
#define MAX_CVARS   48

new g_cmdName[MAX_CMDS*MAX_CMDS_LAYERS][32];
new g_cmdCmd[MAX_CMDS*MAX_CMDS_LAYERS][64];
new g_cmdMisc[MAX_CMDS*MAX_CMDS_LAYERS][2];
new g_cmdNum[MAX_CMDS_LAYERS];

new g_cvarNames[MAX_CVARS][32];
new g_cvarMisc[MAX_CVARS][3];
new g_cvarCmd[MAX_CVARS*5][32];
new g_cvarCmdNum;
new g_cvarNum;

new g_menuPosition[MAX_PLAYERS + 1];
new g_menuSelect[MAX_PLAYERS + 1][MAX_CMDS];
new g_menuSelectNum[MAX_PLAYERS + 1];
new g_menuLayer[MAX_PLAYERS + 1];

new g_coloredMenus;


public plugin_init()
{
	register_plugin("Commands Menu", AMXX_VERSION_STR, "AMXX Dev Team");
	register_dictionary("cmdmenu.txt");
	register_dictionary("common.txt");

	new configsDir[64], config[64];
	get_configsdir(configsDir, charsmax(configsDir));
	
	for (new a = 0; a < MAX_CMDS_LAYERS; ++a)
	{
		new MenuName[64];
		
		formatex(MenuName, charsmax(MenuName), "%L", "en", g_cmdMenuName[a]);
		register_menucmd(register_menuid(MenuName), 1023, "actionCmdMenu");
		register_clcmd(g_cmdMenuCmd[a], "cmdCmdMenu", ADMIN_MENU, g_cmdMenuHelp[a]);
		formatex(config, charsmax(config), "%s/%s", configsDir, g_cmdMenuCfg[a]);
		loadCmdSettings(config, a);
	}

	register_menucmd(register_menuid("Cvars Menu"), 1023, "actionCvarMenu");
	register_clcmd("amx_cvarmenu", "cmdCvarMenu", ADMIN_CVAR, "- displays cvars menu");

	new cvars_ini_file[64];
	formatex(cvars_ini_file, charsmax(cvars_ini_file), "%s/%s", configsDir, "cvars.ini");
	loadCvarSettings(cvars_ini_file);

	g_coloredMenus = colored_menus();
}

#if defined PRECACHE_SPEECHINI
public plugin_precache( )
{
	new configsDir[64], config[64];
	get_configsdir(configsDir, charsmax(configsDir));
	formatex( config, charsmax(config), "%s/%s", configsDir, "speech.ini" );

	new fp = fopen( config, "rt" );			// Read file as text

	if ( ! fp )					// File doesn't exists
	{
		return 0;
	}

	new szText[256];
	new line = 0;
	new szName[32], szSound[128], sndExt[5];
	new field1[32], field2[64], field3[64];
	new fieldNums = 0;
	new const voxIdent[] = "vox", fvoxIdent[] = "fvox", barneyIdent[] = "barney", hgruntIdent[] = "hgrunt";

	while ( line < MAX_CMDS && ! feof( fp ) )	// Loop till MAX_CMDS or EOF
	{
		fgets( fp, szText, charsmax(szText) );               // Store line content

		/* Strips newline */
		new len = strlen( szText );
		if ( len != 0 && szText[len-1] == '^n' )		// len != 0 because if the last line of the file is empty, there's no newline
		{
			szText[--len] = 0;
		}

		if ( len == 0 || szText[0] == ';' || szText[0] == '/' )   // Line is empty or a comment
		{
			continue;
		}

		parse( szText, szName, charsmax(szName), szSound, charsmax(szSound) );
		fieldNums = parse( szSound, field1, charsmax(field1), field2, charsmax(field2), field3, charsmax(field3) );
		if ( fieldNums == 2 && field1[0] == 's' )							// .wav (spk)
		{
			copy( szSound, charsmax(szSound), field2 );
			copy( sndExt, charsmax(sndExt), ".wav" );
		}
		else if ( fieldNums == 3 && field1[0] == 'm' && ( field2[0] == 'p' || field2[0] == 'l' ) )	// .mp3 (mp3 play | mp3 loop)
		{
			copy( szSound, charsmax(szSound), field3 );
			copy( sndExt, charsmax(sndExt), ".mp3" );
		}
		else												// WTH is this sound, drop it.
			continue;

		replace_all( szSound, charsmax(szSound), "\'", "" );								// Strips all ugly (and sometimes useless) \'

		if ( szSound[0] == '/' )
		{
				replace( szSound, charsmax(szSound), "/", "" );						// Strip leading slash
		}

		if ( sndExt[1] == 'm' || ( ! equali( szSound, voxIdent, charsmax(voxIdent) ) && ! equali( szSound, fvoxIdent, charsmax(fvoxIdent) ) && ! equali( szSound, barneyIdent, charsmax(barneyIdent) ) && ! equali( szSound, hgruntIdent, charsmax(hgruntIdent) ) ) )
		{
			// SzSound is a mp3, or a custom wav (not a vox, fvox, or default sound from HL pak)
			if ( !equali( szSound[strlen(szSound)-4], sndExt ) )
			{
				add( szSound, charsmax(szSound), sndExt );			// Add filetype extension if it isn't already specified
			}
			if ( sndExt[1] == 'w' )
			{
				format( szSound, charsmax(szSound), "sound/%s", szSound );	// spk basedir is $moddir/sound, but mp3 play is $moddir, fix this for the file_exists check
			}
			if ( file_exists( szSound ) )
			{
				if ( sndExt[1] == 'm')
				{
					precache_generic( szSound );		// mp3
				}
				else
				{
					replace( szSound, charsmax(szSound), "sound/", "" );	// wav, strip the leading sound/ we added for our file_exists check
					precache_sound( szSound );
				}
			}
		}
		line++;
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
		case 8:
		{	
			displayCmdMenu(id, ++g_menuPosition[id]);
		}
		case 9:
		{
			displayCmdMenu(id, --g_menuPosition[id]);
		}
		default:
		{
			new option = g_menuSelect[id][g_menuPosition[id] * 8 + key];
			new flags = g_cmdMisc[option][1];
			
			if (flags & 1)
				server_cmd("%s", g_cmdCmd[option])
			else if (flags & 2)
				client_cmd(id, "%s", g_cmdCmd[option])
			else if (flags & 4)
				client_cmd(0, "%s", g_cmdCmd[option])
			
			if (flags & 8)
			{
				displayCmdMenu(id, g_menuPosition[id]);
			}
		}
	}
	
	return PLUGIN_HANDLED;
}

displayCmdMenu(id, pos)
{
	if (pos < 0)
	{
		return;
	}

	new menuBody[512];
	new b = 0;
	new start = pos * 8;

	if (start >= g_menuSelectNum[id])
	{
		start = pos = g_menuPosition[id] = 0;
	}
	
	new limit = (g_menuSelectNum[id] / 8 + ((g_menuSelectNum[id] % 8)));
	new len = formatex(menuBody, charsmax(menuBody), g_coloredMenus ? "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n", id, g_cmdMenuName[g_menuLayer[id]], pos + 1, (limit == 0) ? 1 : limit);
	new end = start + 8;
	new keys = MENU_KEY_0;

	if (end > g_menuSelectNum[id])
	{
		end = g_menuSelectNum[id];
	}

	for (new a = start; a < end; ++a)
	{
		if (g_cmdCmd[g_menuSelect[id][a]][0] == '-')
		{
			if (g_coloredMenus)
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, "\d%s^n\w", g_cmdName[g_menuSelect[id][a]]);
			}
			else
			{
				len += formatex(menuBody[len], charsmax(menuBody) - len, "%s^n", g_cmdName[g_menuSelect[id][a]]);
			}
			++b;
		} 
		else 
		{
			keys |= (1<<b);
			len += formatex(menuBody[len], charsmax(menuBody) - len, "%d. %s^n", ++b, g_cmdName[g_menuSelect[id][a]]);
		}
	}

	if (end != g_menuSelectNum[id])
	{
		formatex(menuBody[len], charsmax(menuBody) - len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT");
		keys |= MENU_KEY_9;
	}
	else
	{
		formatex(menuBody[len], charsmax(menuBody) - len, "^n0. %L", id, pos ? "BACK" : "EXIT");
	}
	
	new MenuName[64];
	
	formatex(MenuName, charsmax(MenuName), "%L", "en", g_cmdMenuName[g_menuLayer[id]]);
	show_menu(id, keys, menuBody, -1, MenuName);
}

public cmdCmdMenu(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}

	new szCmd[32];
	read_argv(0, szCmd, charsmax(szCmd));
	new lvl = 0;

	while (lvl < MAX_CMDS_LAYERS)
	{
		if (equal(g_cmdMenuCmd[lvl], szCmd))
		{
			break;
		}
		++lvl;
	}

	g_menuLayer[id] = lvl;
	g_menuSelectNum[id] = 0;

	new a = lvl * MAX_CMDS;
	new d, c = 0;

	while (c < g_cmdNum[lvl])
	{
		d = a + c;
		
		if (access(id, g_cmdMisc[d][0]))
		{
			g_menuSelect[id][g_menuSelectNum[id]++] = d;
		}
		
		++c;
	}

	displayCmdMenu(id, g_menuPosition[id] = 0);
	
	return PLUGIN_HANDLED;
}

loadCmdSettings(szFilename[], level)
{
	if (!file_exists(szFilename))
	{
		return 0;
	}

	new text[256], szFlags[32], szAccess[32];
	new a, pos = 0, c, d = level * MAX_CMDS;

	while (g_cmdNum[level] < MAX_CMDS && read_file(szFilename, pos++, text, charsmax(text), a))
	{
		if (text[0] == ';')
		{
			continue;
		}
		c = d + g_cmdNum[level];
		
		if (parse(text, g_cmdName[c], charsmax(g_cmdName[]), g_cmdCmd[c], charsmax(g_cmdCmd[]), szFlags, charsmax(szFlags), szAccess, charsmax(szAccess)) > 3)
		{
			while (replace(g_cmdCmd[c], charsmax(g_cmdCmd[]), "\'", "^""))
			{
				// do nothing
			}
			
			g_cmdMisc[c][1] = read_flags(szFlags);
			g_cmdMisc[c][0] = read_flags(szAccess);
			g_cmdNum[level]++;
		}
	}

	return 1;
}

/* Cvars menu */

public actionCvarMenu(id, key)
{
	switch (key)
	{
		case 8:
		{
			displayCvarMenu(id, ++g_menuPosition[id]);
		}
		case 9:
		{
			displayCvarMenu(id, --g_menuPosition[id]);
		}
		default:
		{
			new option = g_menuSelect[id][g_menuPosition[id] * 8 + key];
			new szValue[32];
			
			get_cvar_string(g_cvarNames[option], szValue, charsmax(szValue));
			
			new end = g_cvarMisc[option][2];
			new start = g_cvarMisc[option][1];

			for (new i = start; ; ++i)
			{
				if (i < end)
				{
					if (equal(szValue, g_cvarCmd[i]))
					{
						if (++i >= end)
						{
							i = start;
						}
						
						set_cvar_string(g_cvarNames[option], g_cvarCmd[i]);
						break;
					}
				} 
				else 
				{
					set_cvar_string(g_cvarNames[option], g_cvarCmd[start]);
					break;
				}
			}
			displayCvarMenu(id, g_menuPosition[id]);
		}
	}
	
	return PLUGIN_HANDLED;
}

displayCvarMenu(id, pos)
{
	if (pos < 0)
	{
		return;
	}

	new menuBody[512];
	new b = 0;
	new start = pos * 8;

	if (start >= g_menuSelectNum[id])
	{
		start = pos = g_menuPosition[id] = 0;
	}

	new len = formatex(menuBody, charsmax(menuBody), g_coloredMenus ? "\yCvars Menu\R%d/%d^n\w^n" : "Cvars Menu %d/%d^n^n", pos + 1, (g_menuSelectNum[id] / 8 + ((g_menuSelectNum[id] % 8) ? 1 : 0)));

	new end = start + 8;
	new keys = MENU_KEY_0;
	new szValue[64];

	if (end > g_menuSelectNum[id])
	{
		end = g_menuSelectNum[id];
	}

	for (new a = start; a < end; ++a)
	{
		get_cvar_string(g_cvarNames[g_menuSelect[id][a]], szValue, charsmax(szValue));
		keys |= (1<<b);
		++b;
		
		if (g_coloredMenus)
		{
			len += formatex(menuBody[len], charsmax(menuBody) - len, "%d. %s\R%s^n\w", b, g_cvarNames[g_menuSelect[id][a]], szValue);
		}
		else
		{
			len += formatex(menuBody[len], charsmax(menuBody) - len, "%d. %s    %s^n", b, g_cvarNames[g_menuSelect[id][a]], szValue);
		}
	}

	if (end != g_menuSelectNum[id])
	{
		formatex(menuBody[len], charsmax(menuBody) - len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT");
		keys |= MENU_KEY_9;
	}
	else
	{
		formatex(menuBody[len], charsmax(menuBody) - len, "^n0. %L", id, pos ? "BACK" : "EXIT");
	}
	
	show_menu(id, keys, menuBody);
}

public cmdCvarMenu(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}

	g_menuSelectNum[id] = 0;

	for (new a = 0; a < g_cvarNum; ++a)
	{
		if (access(id, g_cvarMisc[a][0]))
		{
			g_menuSelect[id][g_menuSelectNum[id]++] = a;
		}
	}

	displayCvarMenu(id, g_menuPosition[id] = 0);

	return PLUGIN_HANDLED;
}

loadCvarSettings(szFilename[])
{
	if (!file_exists(szFilename)) 
	{
		return 0;
	}

	new text[256], szValues[12][32];
	new inum, a, pos = 0;
	new cvar_values = MAX_CVARS * 5;
	
	// a b c d
	while (g_cvarNum < MAX_CVARS && read_file(szFilename, pos++, text, charsmax(text), a))
	{
		if (text[0] == ';')
		{
			continue;
		}
		
		inum = parse(text, g_cvarNames[g_cvarNum], charsmax(g_cvarNames[]), 
		szValues[0], charsmax(szValues[]),
		szValues[1], charsmax(szValues[]),
		szValues[2], charsmax(szValues[]), 
		szValues[3], charsmax(szValues[]),
		szValues[4], charsmax(szValues[]),
		szValues[5], charsmax(szValues[]), 
		szValues[6], charsmax(szValues[]),
		szValues[7], charsmax(szValues[]),
		szValues[8], charsmax(szValues[]), 
		szValues[9], charsmax(szValues[]),
		szValues[10], charsmax(szValues[]),
		szValues[11], charsmax(szValues[]));

		inum -= 2;
		if (inum < 2)
		{
			continue;
		}
		
		g_cvarMisc[g_cvarNum][1] = g_cvarCmdNum;

		for (a = 0; a < inum && g_cvarCmdNum < cvar_values; ++a)
		{
			while (replace(szValues[a], charsmax(szValues[]), "\'", "^""))
			{
				// do nothing
			}
			
			copy(g_cvarCmd[g_cvarCmdNum], charsmax(g_cvarCmd[]), szValues[a]);
			g_cvarCmdNum++;
		}

		g_cvarMisc[g_cvarNum][2] = g_cvarCmdNum;
		g_cvarMisc[g_cvarNum][0] = read_flags(szValues[inum]);
		g_cvarNum++;
	}

	return 1;
}
