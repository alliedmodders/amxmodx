// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Admin Help Plugin
//

#include <amxmodx>

new g_cvarDisplayClientMessage;
new g_cvarHelpAmount;

public plugin_init()
{
	register_plugin("Admin Help", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("adminhelp.txt")
	register_concmd("amx_help", "cmdHelp", 0, "<page> [nr of cmds (only for server)] - displays this help")

	g_cvarDisplayClientMessage = register_cvar("amx_help_display_msg", "1")
	g_cvarHelpAmount = register_cvar("amx_help_amount_per_page", "10")
}

public client_putinserver(id)
{
	if (get_pcvar_num(g_cvarDisplayClientMessage) && !is_user_bot(id))
	{
		set_task(15.0, "dispInfo", id)
	}
}

public client_disconnected(id)
{
	remove_task(id)
}

public cmdHelp(id, level, cid)
{
	new arg1[8], flags = get_user_flags(id)
	new start = read_argv(1, arg1, charsmax(arg1)) ? str_to_num(arg1) : 1
	new lHelpAmount = get_pcvar_num(g_cvarHelpAmount)

	// HACK: ADMIN_ADMIN is never set as a user's actual flags, so those types of commands never show
	if (flags > 0 && !(flags & ADMIN_USER))
	{
		flags |= ADMIN_ADMIN;
	}

	if (id == 0 && read_argc() == 3)
	{
		if (read_argv(2, arg1, charsmax(arg1)))
		{
			lHelpAmount = str_to_num(arg1);
		}
	}

	if (lHelpAmount <= 0)
	{
		lHelpAmount = 10
	}

	if (--start < 0)
	{
		start = 0
	}

	new clcmdsnum = get_concmdsnum(flags, id)

	if (start >= clcmdsnum)
	{
		start = clcmdsnum - 1
	}

	console_print(id, "^n----- %L -----", id, "HELP_COMS")
	
	new info[128], cmd[32], eflags
	new end = start + lHelpAmount

	if (end > clcmdsnum)
	{
		end = clcmdsnum
	}

	for (new i = start; i < end; i++)
	{
		get_concmd(i, cmd, charsmax(cmd), eflags, info, charsmax(info), flags, id)
		console_print(id, "%3d: %s %s", i + 1, cmd, info)
	}
	
	console_print(id, "----- %L -----", id, "HELP_ENTRIES", start + 1, end, clcmdsnum)

	if (end < clcmdsnum)
	{
		console_print(id, "----- %L -----", id, "HELP_USE_MORE", end + 1)
	}
	else
	{
		console_print(id, "----- %L -----", id, "HELP_USE_BEGIN")
	}

	return PLUGIN_HANDLED
}

public dispInfo(id)
{
	client_print(id, print_chat, "%L", id, "TYPE_HELP")

	static amx_nextmap, mp_timelimit = 0
	if( !mp_timelimit )
	{
		amx_nextmap = get_cvar_pointer("amx_nextmap")
		mp_timelimit = get_cvar_pointer("mp_timelimit")
	}

	new nextmap[32]
	if( amx_nextmap )
	{
		get_pcvar_string(amx_nextmap, nextmap, charsmax(nextmap))
	}
	
	if (get_pcvar_float(mp_timelimit))
	{
		new timeleft = get_timeleft()
		
		if (timeleft > 0)
		{
			client_print(id, print_chat, "%L", id, "TIME_INFO_1", timeleft / 60, timeleft % 60, nextmap)
		}
		else if (amx_nextmap)
		{
			client_print(id, print_chat, "%L", id, "TIME_INFO_2", nextmap)
		}
	}
}
