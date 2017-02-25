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

const MaxMapLength = 32;
const MaxDefaultEntries = 10;

new CvarDisplayClientMessage;
new CvarHelpAmount;
new CvarNextmap[MaxMapLength];
new Float:CvarTimeLimit;

new bool:DisplayClientMessage[MAX_PLAYERS + 1 char];

public plugin_init()
{
	register_plugin("Admin Help", AMXX_VERSION_STR, "AMXX Dev Team");
	register_dictionary("adminhelp.txt");
	register_concmd("amx_help", "@ConsoleCommand_Help", ADMIN_ALL, "HELP_CMD_INFO", .info_ml = true);

	bind_pcvar_num(register_cvar("amx_help_display_msg", "1"), CvarDisplayClientMessage);
	bind_pcvar_num(register_cvar("amx_help_amount_per_page", "10"), CvarHelpAmount);
}

public OnConfigsExecuted()
{
	new const pointer = get_cvar_pointer("amx_nextmap");

	if (pointer)
	{
		bind_pcvar_string(pointer, CvarNextmap, charsmax(CvarNextmap));
	}

	bind_pcvar_float(get_cvar_pointer("mp_timelimit"), CvarTimeLimit);
}

public client_putinserver(id)
{
	if (CvarDisplayClientMessage > 0 && !is_user_bot(id))
	{
		DisplayClientMessage{id} = true;
		set_task(15.0, "dispInfo", id);
	}
}

public client_disconnected(id)
{
	if (DisplayClientMessage{id})
	{
		DisplayClientMessage{id} = false; 
		remove_task(id);
	}
}

@ConsoleCommand_Help(id, level, cid)
{
	new user_flags = get_user_flags(id);

	// HACK: ADMIN_ADMIN is never set as a user's actual flags, so those types of commands never show
	if (user_flags > 0 && !(user_flags & ADMIN_USER))
	{
		user_flags |= ADMIN_ADMIN;
	}

	new clcmdsnum = get_concmdsnum(user_flags, id);

	new start  = clamp(read_argv_int(1), .min = 1, .max = clcmdsnum) - 1; // Zero-based list
	new amount = !id ? read_argv_int(2) : CvarHelpAmount;
	new end    = min(start + (amount > 0 ? amount : MaxDefaultEntries), clcmdsnum);

	console_print(id, "^n----- %l -----", "HELP_COMS");
	
	new info[128], command[32], command_flags, bool:is_info_ml;

	for (new index = start; index < end; index++)
	{
		get_concmd(index, command, charsmax(command), command_flags, info, charsmax(info), user_flags, id, is_info_ml);

		if (is_info_ml)
		{
			LookupLangKey(info, charsmax(info), info, id);
		}

		console_print(id, "%3d: %s %s", index + 1, command, info);
	}
	
	console_print(id, "----- %l -----", "HELP_ENTRIES", start + 1, end, clcmdsnum);

	if (end < clcmdsnum)
	{
		console_print(id, "----- %l -----", "HELP_USE_MORE", end + 1);
	}
	else
	{
		console_print(id, "----- %l -----", "HELP_USE_BEGIN");
	}

	return PLUGIN_HANDLED;
}

public dispInfo(id)
{
	client_print(id, print_chat, "%l", "TYPE_HELP");

	if (CvarTimeLimit > 0.0)
	{
		new timeleft = get_timeleft();
		
		if (timeleft > 0)
		{
			client_print(id, print_chat, "%l", "TIME_INFO_1", timeleft / 60, timeleft % 60, CvarNextmap);
		}
		else if (CvarNextmap[0] != EOS)
		{
			client_print(id, print_chat, "%l", "TIME_INFO_2", CvarNextmap);
		}
	}
}
