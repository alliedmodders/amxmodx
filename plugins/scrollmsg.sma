// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Scrolling Message Plugin
//

#include <amxmodx>
#include <amxmisc>

const SCROLLMSG_SIZE = 512;
const SCROLLMSG_TASK = 123;

new g_hostname[64];
new g_amx_scrollmsg_color_red;
new g_amx_scrollmsg_color_green;
new g_amx_scrollmsg_color_blue;
new g_amx_scrollmsg_only_dead;
new Float:g_amx_scrollmsg_speed;
new Float:g_amx_scrollmsg_x_move_amount;
new Float:g_amx_scrollmsg_x_start_pos;
new Float:g_amx_scrollmsg_x_end_pos;
new Float:g_amx_scrollmsg_y_pos;

new g_start_pos;
new g_end_pos;
new g_scroll_msg[SCROLLMSG_SIZE];
new g_display_msg[SCROLLMSG_SIZE];
new g_length;
new g_frequency;
new g_hud_object;
new Float:g_x_pos;

public plugin_init()
{
	register_plugin("Scrolling Message", AMXX_VERSION_STR, "AMXX Dev Team");

	register_dictionary("scrollmsg.txt");
	register_dictionary("common.txt");

	register_srvcmd("amx_scrollmsg", "setMessage", _, "<message> <frequency>");
	bind_pcvar_string(get_cvar_pointer("hostname"), g_hostname, charsmax(g_hostname));

	bind_pcvar_num(create_cvar(   "amx_scrollmsg_color_red",       "200",      _, "Red color amount",                             true, 0.0, true, 255.0), g_amx_scrollmsg_color_red);
	bind_pcvar_num(create_cvar(   "amx_scrollmsg_color_green",     "100",      _, "Green color amount",                           true, 0.0, true, 255.0), g_amx_scrollmsg_color_green);
	bind_pcvar_num(create_cvar(   "amx_scrollmsg_color_blue",      "0",        _, "Blue color amount",                            true, 0.0, true, 255.0), g_amx_scrollmsg_color_blue);
	bind_pcvar_num(create_cvar(   "amx_scrollmsg_only_dead",       "0",        _, "Display the message only to dead clients?",    true, 0.0, true, 1.0),   g_amx_scrollmsg_only_dead);
	bind_pcvar_float(create_cvar( "amx_scrollmsg_speed",           "0.3",      _, "The rate at which the message will move",      true, 0.0),              g_amx_scrollmsg_speed);
	bind_pcvar_float(create_cvar( "amx_scrollmsg_x_move_amount",   "0.0063",   _, "The amount of units to move on the X axis"),                            g_amx_scrollmsg_x_move_amount);
	bind_pcvar_float(create_cvar( "amx_scrollmsg_x_start_pos",     "0.35",     _, "Starting position on the X axis",              true, -1.0, true, 1.0),  g_amx_scrollmsg_x_start_pos);
	bind_pcvar_float(create_cvar( "amx_scrollmsg_x_end_pos",       "0.65",     _, "Ending position on the X axis",                true, -1.0, true, 1.0),  g_amx_scrollmsg_x_end_pos);
	bind_pcvar_float(create_cvar( "amx_scrollmsg_y_pos",           "0.9",      _, "The Y position of the message",                true, -1.0, true, 1.0),  g_amx_scrollmsg_y_pos);

	g_hud_object = CreateHudSyncObj();
	AutoExecConfig();
}

public showMsg()
{
	new a = g_start_pos, i = 0;

	while (a < g_end_pos)
	{
		g_display_msg[i++] = g_scroll_msg[a++];
	}

	g_display_msg[i] = 0;

	if (g_end_pos < g_length)
	{
		g_end_pos++;
	}

	if (g_x_pos > g_amx_scrollmsg_x_start_pos)
	{
		g_x_pos -= g_amx_scrollmsg_x_move_amount;
	}
	else
	{
		g_start_pos++;
		g_x_pos = g_amx_scrollmsg_x_start_pos;
	}

	set_hudmessage(g_amx_scrollmsg_color_red, g_amx_scrollmsg_color_green, g_amx_scrollmsg_color_blue, g_x_pos, g_amx_scrollmsg_y_pos, 0, g_amx_scrollmsg_speed, g_amx_scrollmsg_speed, 0.05, 0.05, 2);

	if (g_amx_scrollmsg_only_dead)
	{
		new players[MAX_PLAYERS], pnum;
		get_players_ex(players, pnum, GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV|GetPlayers_ExcludeAlive);

		for (new i; i < pnum; i++)
		{
			ShowSyncHudMsg(players[i], g_hud_object, g_display_msg);
		}
	}
	else
	{
		ShowSyncHudMsg(0, g_hud_object, g_display_msg);
	}
}

public msgInit()
{
	g_end_pos = 1;
	g_start_pos = 0;
	g_x_pos = g_amx_scrollmsg_x_end_pos;
	
	replace_stringex(g_scroll_msg, charsmax(g_scroll_msg), "%hostname%", g_hostname);
	
	g_length = strlen(g_scroll_msg);
	
	set_task_ex(g_amx_scrollmsg_speed, "showMsg", SCROLLMSG_TASK, "", 0, SetTask_RepeatTimes, g_length + 48);
	console_print(0, g_scroll_msg);
}

public setMessage()
{
	remove_task(SCROLLMSG_TASK);		/* remove current messaging */
	read_argv(1, g_scroll_msg, charsmax(g_scroll_msg));
	
	g_length = strlen(g_scroll_msg);
	g_frequency = read_argv_int(2);
	
	if (g_frequency > 0)
	{
		new minimal = floatround((g_length + 48) * (g_amx_scrollmsg_speed + 0.1));
		
		if (g_frequency < minimal)
		{
			server_print("%L", LANG_SERVER, "MIN_FREQ", minimal);
			g_frequency = minimal;
		}

		server_print("%L", LANG_SERVER, "MSG_FREQ", g_frequency / 60, g_frequency % 60);
		set_task_ex(float(g_frequency), "msgInit", SCROLLMSG_TASK, "", 0, SetTask_Repeat);
	}
	else
	{
		server_print("%L", LANG_SERVER, "MSG_DISABLED");
	}
	
	return PLUGIN_HANDLED;
}
