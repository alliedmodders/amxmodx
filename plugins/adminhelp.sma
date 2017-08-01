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

const MaxMapLength         = 32;
const MaxDefaultEntries    = 10;
const MaxCommandLength     = 32;
const MaxCommandInfoLength = 128;
const DefaultMsgTime       = 15;

new const HelpCommand[]   = "amx_help";
new const SearchCommand[] = "amx_searchcmd";

new CvarDisplayClientMessage;
new CvarDisplayMessageTime;
new CvarHelpAmount;

new CvarNextmap[MaxMapLength];
new Float:CvarTimeLimit;

new bool:DisplayClientMessage[MAX_PLAYERS + 1 char];

public plugin_init()
{
	register_plugin("Admin Help", AMXX_VERSION_STR, "AMXX Dev Team");
	register_dictionary("adminhelp.txt");

	register_concmd(HelpCommand  , "@ConsoleCommand_Help"  , ADMIN_ALL, "HELP_CMD_INFO"  , .info_ml = true);
	register_concmd(SearchCommand, "@ConsoleCommand_Search", ADMIN_ALL, "SEARCH_CMD_INFO", .info_ml = true);

	bind_pcvar_num(create_cvar("amx_help_display_msg"     , "1" , .has_min = true, .min_val = 0.0, .has_max = true, .max_val = 1.0), CvarDisplayClientMessage);
	bind_pcvar_num(create_cvar("amx_help_display_msg_time", "15", .has_min = true, .min_val = 0.0), CvarDisplayMessageTime);
	bind_pcvar_num(create_cvar("amx_help_amount_per_page" , "10", .has_min = true, .min_val = 0.0), CvarHelpAmount);
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

		new Float:messageTime = float(CvarDisplayMessageTime <= 0 ? DefaultMsgTime : CvarDisplayMessageTime);
		set_task(messageTime, "@Task_DisplayMessage", id);
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

@ConsoleCommand_Search(id, level, cid)
{
	new entry[MaxCommandLength];
	read_argv(1, entry, charsmax(entry));

	return ProcessHelp(id, .start_argindex = 2, .do_search = true, .main_command = SearchCommand, .search = entry);
}

@ConsoleCommand_Help(id, level, cid)
{
	return ProcessHelp(id, .start_argindex = 1, .do_search = false, .main_command = HelpCommand);
}

ProcessHelp(id, start_argindex, bool:do_search, const main_command[], const search[] = "")
{
	new user_flags = get_user_flags(id);

	// HACK: ADMIN_ADMIN is never set as a user's actual flags, so those types of commands never show
	if (user_flags > 0 && !(user_flags & ADMIN_USER))
	{
		user_flags |= ADMIN_ADMIN;
	}

	new clcmdsnum = get_concmdsnum(user_flags, id);

	if (CvarHelpAmount <= 0)
	{
		CvarHelpAmount = MaxDefaultEntries;
	}

	new start  = clamp(read_argv_int(start_argindex), .min = 1, .max = clcmdsnum) - 1; // Zero-based list;
	new amount = !id ? read_argv_int(start_argindex + 1) : CvarHelpAmount;
	new end    = min(start + (amount > 0 ? amount : CvarHelpAmount), clcmdsnum);

	console_print(id, "^n----- %l -----", "HELP_COMS");

	new info[MaxCommandInfoLength];
	new command[MaxCommandLength];
	new command_flags;
	new bool:is_info_ml;
	new entries_found;
	new total_entries;
	new index;

	if (do_search)
	{
		for (index = 0; index < clcmdsnum; ++index)
		{
			get_concmd(index, command, charsmax(command), command_flags, info, charsmax(info), user_flags, id, is_info_ml);

			if (containi(command, search) != -1 && ++entries_found > start && (total_entries = entries_found) <= end)
			{
				if (is_info_ml)
				{
					LookupLangKey(info, charsmax(info), info, id);
				}

				console_print(id, "%3d: %s %s", entries_found, command, info);
			}
		}

		if (!entries_found || entries_found > total_entries)
		{
			console_print(id, "%l", "NO_MATCHING_RESULTS");
			return PLUGIN_HANDLED;
		}

		index = entries_found;
		clcmdsnum = total_entries;
		end = min(end, clcmdsnum);
	}
	else
	{
		for (index = start; index < end; ++index)
		{
			get_concmd(index, command, charsmax(command), command_flags, info, charsmax(info), user_flags, id, is_info_ml);

			if (is_info_ml)
			{
				LookupLangKey(info, charsmax(info), info, id);
			}

			console_print(id, "%3d: %s %s", index + 1, command, info);
		}
	}

	console_print(id, "----- %l -----", "HELP_ENTRIES", start + 1, end, clcmdsnum);

	formatex(command, charsmax(command), "%s%c%s", main_command, do_search ? " " : "", search);

	if (end < clcmdsnum)
	{
		console_print(id, "----- %l -----", "HELP_USE_MORE", command, end + 1);
	}
	else if (start || index != clcmdsnum)
	{
		console_print(id, "----- %l -----", "HELP_USE_BEGIN", command);
	}

	return PLUGIN_HANDLED;
}

@Task_DisplayMessage(id)
{
	client_print(id, print_chat, "%l", "TYPE_HELP", HelpCommand, SearchCommand);

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
