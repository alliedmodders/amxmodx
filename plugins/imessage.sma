// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Info. Messages Plugin
//

#include <amxmodx>
#include <amxmisc>

const MAX_MSG_LEN = 384;
const TASK_MSG    = 12345;

enum _:MessageInfo
{
	Message[MAX_MSG_LEN],
	R,
	G,
	B
}

new Array:g_messages;
new g_messagesNum;
new g_hudObject;
new g_current;

new g_hostname[64];
new Float:g_amx_freq_imessage;
new Float:g_amx_imessage_x_pos;
new Float:g_amx_imessage_y_pos;
new Float:g_amx_imessage_holdtime;
new g_amx_imessage_only_dead;

public plugin_init()
{
	register_plugin("Info. Messages", AMXX_VERSION_STR, "AMXX Dev Team");
	register_srvcmd("amx_imessage", "setMessage", _, "<message> <color in RRRGGGBBB format>");

	register_dictionary("imessage.txt");
	register_dictionary("common.txt");

	g_messages = ArrayCreate(MessageInfo);
	g_hudObject = CreateHudSyncObj();

	bind_pcvar_num(create_cvar("amx_imessage_only_dead", "0", _, "Set to 1 to show info messages only to dead clients", true, 0.0, true, 1.0), g_amx_imessage_only_dead);
	bind_pcvar_float(create_cvar("amx_freq_imessage", "180", _, "Frequency in seconds of info messages", true, 0.0), g_amx_freq_imessage);
	bind_pcvar_float(create_cvar("amx_imessage_x_pos", "-1.0", _, "X position for info messages", true, -1.0, true, 1.0), g_amx_imessage_x_pos);
	bind_pcvar_float(create_cvar("amx_imessage_y_pos", "0.2", _, "Y position for info messages", true, -1.0, true, 1.0), g_amx_imessage_y_pos);
	bind_pcvar_float(create_cvar("amx_imessage_holdtime", "12.0", _, "Hold time for info messages", true, 0.0), g_amx_imessage_holdtime);
	bind_pcvar_string(get_cvar_pointer("hostname"), g_hostname, charsmax(g_hostname));
	
	new lastinfo[8];
	get_localinfo("lastinfomsg", lastinfo, charsmax(lastinfo));
	g_current = str_to_num(lastinfo);
	set_localinfo("lastinfomsg", "");
}

public infoMessage()
{
	// No messages, just get out of here
	if (!g_messagesNum)
	{
		return;
	}

	// If the last message is reached, go back to the first one
	if (g_current >= g_messagesNum)
	{
		g_current = 0;
	}
	
	static message[MessageInfo];
	ArrayGetArray(g_messages, g_current, message);
	replace_stringex(message[Message], charsmax(message[Message]), "%hostname%", g_hostname);
	
	set_hudmessage(message[R], message[G], message[B], g_amx_imessage_x_pos, g_amx_imessage_y_pos, 0, 0.5, g_amx_imessage_holdtime, 2.0, 2.0, -1);

	if(g_amx_imessage_only_dead)
	{
		new players[MAX_PLAYERS], pnum;
		get_players_ex(players, pnum, GetPlayers_ExcludeAlive);

		for(new player, i; i < pnum; i++)
		{
			player = players[i];

			ShowSyncHudMsg(player, g_hudObject, message[Message]);
			console_print(player, message[Message]);
		}
	}
	else
	{
		ShowSyncHudMsg(0, g_hudObject, message[Message]);
		console_print(0, message[Message]);
	}
	
	g_current++;
	
	if (g_amx_freq_imessage > 0.0)
	{
		set_task(g_amx_freq_imessage, "infoMessage", TASK_MSG);
	}
}

public setMessage()
{
	remove_task(TASK_MSG);

	static message[MessageInfo];
	read_argv(1, message[Message], charsmax(message[Message]));
	replace_string(message[Message], charsmax(message[Message]), "\n", "^n");

	new fullcolor[10];
	read_argv(2, fullcolor, charsmax(fullcolor));

	message[B] = str_to_num(fullcolor[6]);
	fullcolor[6] = 0;

	message[G] = str_to_num(fullcolor[3]);
	fullcolor[3] = 0;

	message[R] = str_to_num(fullcolor[0]);
	fullcolor[0] = 0;

	g_messagesNum++;
	ArrayPushArray(g_messages, message);
	
	if (g_amx_freq_imessage > 0.0)
	{
		set_task(g_amx_freq_imessage, "infoMessage", TASK_MSG);
	}
	
	return PLUGIN_HANDLED;
}

public plugin_end()
{
	ArrayDestroy(g_messages);
	set_localinfo("lastinfomsg", fmt("%i", g_current));
}
