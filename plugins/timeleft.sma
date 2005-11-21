/* AMX Mod X
*   TimeLeft Plugin
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

new g_TimeSet[32][2]
new g_LastTime
new g_CountDown
new g_Switch

public plugin_init()
{
	register_plugin("TimeLeft", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("timeleft.txt")
	register_cvar("amx_time_voice", "1")
	register_srvcmd("amx_time_display", "setDisplaying")
	register_cvar("amx_timeleft", "00:00", FCVAR_SERVER|FCVAR_EXTDLL|FCVAR_UNLOGGED|FCVAR_SPONLY)
	register_clcmd("say timeleft", "sayTimeLeft", 0, "- displays timeleft")
	register_clcmd("say thetime", "sayTheTime", 0, "- displays current time")
	
	set_task(0.8, "timeRemain", 8648458, "", 0, "b")
}

public sayTheTime(id)
{
	if (get_cvar_num("amx_time_voice"))
	{
		new mhours[6], mmins[6], whours[32], wmins[32], wpm[6]
		
		get_time("%H", mhours, 5)
		get_time("%M", mmins, 5)
		
		new mins = str_to_num(mmins)
		new hrs = str_to_num(mhours)
		
		if (mins)
			num_to_word(mins, wmins, 31)
		else
			wmins[0] = 0
		
		if (hrs < 12)
			wpm = "am "
		else
		{
			if (hrs > 12) hrs -= 12
			wpm = "pm "
		}

		if (hrs) 
			num_to_word(hrs, whours, 31)
		else
			whours = "twelve "
		
		client_cmd(id, "spk ^"fvox/time_is_now %s_period %s%s^"", whours, wmins, wpm)
	}
	
	new ctime[64]
	
	get_time("%m/%d/%Y - %H:%M:%S", ctime, 63)
	client_print(0, print_chat, "%L:   %s", LANG_PLAYER, "THE_TIME", ctime)
	
	return PLUGIN_CONTINUE
}

public sayTimeLeft(id)
{
	if (get_cvar_float("mp_timelimit"))
	{
		new a = get_timeleft()
		
		if (get_cvar_num("amx_time_voice"))
		{
			new svoice[128]
			setTimeVoice(svoice, 127, 0, a)
			client_cmd(id, "%s", svoice)
		}
		client_print(0, print_chat, "%L:  %d:%02d", LANG_PLAYER, "TIME_LEFT", (a / 60), (a % 60))
	}
	else
		client_print(0, print_chat, "%L", LANG_PLAYER, "NO_T_LIMIT")
	
	return PLUGIN_CONTINUE
}

setTimeText(text[], len, tmlf, id)
{
	new secs = tmlf % 60
	new mins = tmlf / 60
	
	if (secs == 0)
		format(text, len, "%d %L", mins, id, (mins > 1) ? "MINUTES" : "MINUTE")
	else if (mins == 0)
		format(text, len, "%d %L", secs, id, (secs > 1) ? "SECONDS" : "SECOND")
	else
		format(text, len, "%d %L %d %L", mins, id, (mins > 1) ? "MINUTES" : "MINUTE", secs, id, (secs > 1) ? "SECONDS" : "SECOND")
}

setTimeVoice(text[], len, flags, tmlf)
{
	new temp[7][32]
	new secs = tmlf % 60
	new mins = tmlf / 60
	
	for (new a = 0;a < 7;++a)
		temp[a][0] = 0

	if (secs > 0)
	{
		num_to_word(secs, temp[4], 31)
		
		if (!(flags & 8)) 
			temp[5] = "seconds "	/* there is no "second" in default hl */
	}
	
	if (mins > 59)
	{
		new hours = mins / 60
		
		num_to_word(hours, temp[0], 31)
		
		if (!(flags & 8))
			temp[1] = "hours "
		
		mins = mins % 60
	}
	
	if (mins > 0)
	{
		num_to_word(mins, temp[2], 31)
		
		if (!(flags & 8))
			temp[3] = "minutes "
	}
	
	if (!(flags & 4))
		temp[6] = "remaining "
	
	return format(text, len, "spk ^"vox/%s%s%s%s%s%s%s^"", temp[0], temp[1], temp[2], temp[3], temp[4], temp[5], temp[6])
}

findDispFormat(time)
{
	for (new i = 0; g_TimeSet[i][0]; ++i)
	{
		if (g_TimeSet[i][1] & 16)
		{
			if (g_TimeSet[i][0] > time)
			{
				if (!g_Switch)
				{
					g_CountDown = g_Switch = time
					remove_task(8648458)
					set_task(1.0, "timeRemain", 34543, "", 0, "b")
				}
				
				return i
			}
		}
		else if (g_TimeSet[i][0] == time)
		{
			return i
		}
	}
	
	return -1
}

public setDisplaying()
{
	new arg[32], flags[32], num[32]
	new argc = read_argc() - 1
	new i = 0

	while (i < argc && i < 32)
	{
		read_argv(i + 1, arg, 31)
		parse(arg, flags, 31, num, 31)
		
		g_TimeSet[i][0] = str_to_num(num)
		g_TimeSet[i][1] = read_flags(flags)
		
		i++
	}
	g_TimeSet[i][0] = 0
	
	return PLUGIN_HANDLED
}

public timeRemain(param[])
{
	new gmtm = get_timeleft()
	new tmlf = g_Switch ? --g_CountDown : gmtm
	new stimel[12]
	
	format(stimel, 11, "%02d:%02d", gmtm / 60, gmtm % 60)
	set_cvar_string("amx_timeleft", stimel)
	
	if (g_Switch && gmtm > g_Switch)
	{
		remove_task(34543)
		g_Switch = 0
		set_task(0.8, "timeRemain", 8648458, "", 0, "b")
		
		return
	}

	if (tmlf > 0 && g_LastTime != tmlf)
	{
		g_LastTime = tmlf
		new tm_set = findDispFormat(tmlf)
		
		if (tm_set != -1)
		{
			new flags = g_TimeSet[tm_set][1]
			new arg[128]
			
			if (flags & 1)
			{
				new players[32], pnum
				
				get_players(players, pnum, "c")
				
				for (new i = 0; i < pnum; i++)
				{
					setTimeText(arg, 127, tmlf, players[i])
					
					if (flags & 16)
						set_hudmessage(255, 255, 255, -1.0, 0.85, 0, 0.0, 1.1, 0.1, 0.5, -1)
					else
						set_hudmessage(255, 255, 255, -1.0, 0.85, 0, 0.0, 3.0, 0.0, 0.5, -1)
					
					show_hudmessage(players[i], "%s", arg)
				}
			}

			if (flags & 2)
			{
				setTimeVoice(arg, 127, flags, tmlf)
				client_cmd(0, "%s", arg)
			}
		}
	}
}