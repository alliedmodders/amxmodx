// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// NextMap Plugin
//

#include <amxmodx>

// WARNING: If you comment this line make sure
// that in your mapcycle file maps don't repeat.
// However the same map in a row is still valid.
#define OBEY_MAPCYCLE

new g_nextMap[32]
new g_mapCycle[32]
new g_pos
new g_currentMap[32]

// pcvars
new g_mp_friendlyfire, g_mp_chattime
new g_amx_nextmap

public plugin_init()
{
	register_plugin("NextMap", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("nextmap.txt")
	register_event("30", "changeMap", "a")
	register_clcmd("say nextmap", "sayNextMap", 0, "- displays nextmap")
	register_clcmd("say currentmap", "sayCurrentMap", 0, "- display current map")

	g_amx_nextmap = register_cvar("amx_nextmap", "", FCVAR_SERVER|FCVAR_EXTDLL|FCVAR_SPONLY)
	g_mp_chattime = get_cvar_pointer("mp_chattime")
	g_mp_friendlyfire = get_cvar_pointer("mp_friendlyfire")
	if( g_mp_friendlyfire )
	{
		register_clcmd("say ff", "sayFFStatus", 0, "- display friendly fire status")
	}

	get_mapname(g_currentMap, charsmax(g_currentMap))

	new szString[40], szString2[32], szString3[8]
	
	get_localinfo("lastmapcycle", szString, charsmax(szString))
	parse(szString, szString2, charsmax(szString2), szString3, charsmax(szString3))
	
	get_cvar_string("mapcyclefile", g_mapCycle, charsmax(g_mapCycle))

	if (!equal(g_mapCycle, szString2))
		g_pos = 0	// mapcyclefile has been changed - go from first
	else
		g_pos = str_to_num(szString3)

	readMapCycle(g_mapCycle, g_nextMap, charsmax(g_nextMap))
	set_pcvar_string(g_amx_nextmap, g_nextMap)
	formatex(szString, charsmax(szString), "%s %d", g_mapCycle, g_pos)	// save lastmapcycle settings
	set_localinfo("lastmapcycle", szString)
}

getNextMapName(szArg[], iMax)
{
	new len = get_pcvar_string(g_amx_nextmap, szArg, iMax)
	
	if (ValidMap(szArg)) return len
	len = copy(szArg, iMax, g_nextMap)
	set_pcvar_string(g_amx_nextmap, g_nextMap)
	
	return len
}

public sayNextMap()
{
	new name[32]
	
	getNextMapName(name, charsmax(name))
	client_print(0, print_chat, "%L %s", LANG_PLAYER, "NEXT_MAP", name)
}

public sayCurrentMap()
{
	client_print(0, print_chat, "%L: %s", LANG_PLAYER, "PLAYED_MAP", g_currentMap)
}

public sayFFStatus()
{
	client_print(0, print_chat, "%L: %L", LANG_PLAYER, "FRIEND_FIRE", LANG_PLAYER, get_pcvar_num(g_mp_friendlyfire) ? "ON" : "OFF")
}

public delayedChange(param[])
{
	if (g_mp_chattime) {
		set_pcvar_float(g_mp_chattime, get_pcvar_float(g_mp_chattime) - 2.0)
	}
	engine_changelevel(param)
}

public changeMap()
{
	new string[32]
	new Float:chattime = g_mp_chattime ? get_pcvar_float(g_mp_chattime) : 10.0;	// mp_chattime defaults to 10 in other mods
	
	if (g_mp_chattime) {
		set_pcvar_float(g_mp_chattime, chattime + 2.0)		// make sure mp_chattime is long
	}
	new len = getNextMapName(string, charsmax(string)) + 1
	set_task(chattime, "delayedChange", 0, string, len)	// change with 1.5 sec. delay
}

new g_warning[] = "WARNING: Couldn't find a valid map or the file doesn't exist (file ^"%s^")"

stock bool:ValidMap(mapname[])
{
	if ( is_map_valid(mapname) )
	{
		return true;
	}
	// If the is_map_valid check failed, check the end of the string
	new len = strlen(mapname) - 4;
	
	// The mapname was too short to possibly house the .bsp extension
	if (len < 0)
	{
		return false;
	}
	if ( equali(mapname[len], ".bsp") )
	{
		// If the ending was .bsp, then cut it off.
		// the string is byref'ed, so this copies back to the loaded text.
		mapname[len] = '^0';
		
		// recheck
		if ( is_map_valid(mapname) )
		{
			return true;
		}
	}
	
	return false;
}

#if defined OBEY_MAPCYCLE
readMapCycle(szFileName[], szNext[], iNext)
{
	new b, i = 0, iMaps = 0
	new szBuffer[32], szFirst[32]

	if (file_exists(szFileName))
	{
		while (read_file(szFileName, i++, szBuffer, charsmax(szBuffer), b))
		{
			if (!isalnum(szBuffer[0]) || !ValidMap(szBuffer)) continue
			
			if (!iMaps)
				copy(szFirst, charsmax(szFirst), szBuffer)
			
			if (++iMaps > g_pos)
			{
				copy(szNext, iNext, szBuffer)
				g_pos = iMaps
				return
			}
		}
	}

	if (!iMaps)
	{
		log_amx(g_warning, szFileName)
		copy(szNext, iNext, g_currentMap)
	}
	else
		copy(szNext, iNext, szFirst)
	g_pos = 1
}

#else

readMapCycle(szFileName[], szNext[], iNext)
{
	new b, i = 0, iMaps = 0
	new szBuffer[32], szFirst[32]
	
	new a = g_pos

	if (file_exists(szFileName))
	{
		while (read_file(szFileName, i++, szBuffer, charsmax(szBuffer), b))
		{
			if (!isalnum(szBuffer[0]) || !ValidMap(szBuffer)) continue
			
			if (!iMaps)
			{
				iMaps = 1
				copy(szFirst, charsmax(szFirst), szBuffer)
			}
			
			if (iMaps == 1)
			{
				if (equali(g_currentMap, szBuffer))
				{
					if (a-- == 0)
						iMaps = 2
				}
			} else {
				if (equali(g_currentMap, szBuffer))
					++g_pos
				else
					g_pos = 0
				
				copy(szNext, iNext, szBuffer)
				return
			}
		}
	}
	
	if (!iMaps)
	{
		log_amx(g_warning, szFileName)
		copy(szNext, iNext, g_currentMap)
	}
	else
		copy(szNext, iNext, szFirst)
	
	g_pos = 0
}
#endif
