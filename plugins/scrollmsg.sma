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

new g_startPos;
new g_endPos;
new g_scrollMsg[SCROLLMSG_SIZE];
new g_displayMsg[SCROLLMSG_SIZE];
new Float:g_xPos;
new g_Length;
new g_Frequency;

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
}

public showMsg()
{
	new a = g_startPos, i = 0;

	while (a < g_endPos)
	{
		g_displayMsg[i++] = g_scrollMsg[a++];
	}

	g_displayMsg[i] = 0;

	if (g_endPos < g_Length)
	{
		g_endPos++;
	}

	if (g_xPos > g_amx_scrollmsg_x_start_pos)
	{
		g_xPos -= g_amx_scrollmsg_x_move_amount;
	}
	else
	{
		g_startPos++;
		g_xPos = g_amx_scrollmsg_x_start_pos;
	}

	set_hudmessage(g_amx_scrollmsg_color_red, g_amx_scrollmsg_color_green, g_amx_scrollmsg_color_blue, g_xPos, g_amx_scrollmsg_y_pos, 0, g_amx_scrollmsg_speed, g_amx_scrollmsg_speed, 0.05, 0.05, 2);

	if(g_amx_scrollmsg_only_dead)
	{
		new players[MAX_PLAYERS], pnum;
		get_players_ex(players, pnum, GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV|GetPlayers_ExcludeAlive);

		for(new i; i < pnum; i++)
		{
			show_hudmessage(players[i], g_displayMsg);
		}
	}
	else
	{
		show_hudmessage(0, "%s", g_displayMsg);
	}
}

public msgInit()
{
	g_endPos = 1;
	g_startPos = 0;
	g_xPos = g_amx_scrollmsg_x_end_pos;
	
	replace(g_scrollMsg, charsmax(g_scrollMsg), "%hostname%", g_hostname);
	
	g_Length = strlen(g_scrollMsg);
	
	set_task(g_amx_scrollmsg_speed, "showMsg", SCROLLMSG_TASK, "", 0, "a", g_Length + 48);
	client_print(0, print_console, "%s", g_scrollMsg);
}

public setMessage()
{
	remove_task(SCROLLMSG_TASK);		/* remove current messaging */
	read_argv(1, g_scrollMsg, charsmax(g_scrollMsg));
	
	g_Length = strlen(g_scrollMsg);
	
	new mytime[32];
	
	read_argv(2, mytime, charsmax(mytime));
	
	g_Frequency = str_to_num(mytime);
	
	if (g_Frequency > 0)
	{
		new minimal = floatround((g_Length + 48) * (g_amx_scrollmsg_speed + 0.1));
		
		if (g_Frequency < minimal)
		{
			server_print("%L", LANG_SERVER, "MIN_FREQ", minimal);
			g_Frequency = minimal;
		}

		server_print("%L", LANG_SERVER, "MSG_FREQ", g_Frequency / 60, g_Frequency % 60);
		set_task(float(g_Frequency), "msgInit", SCROLLMSG_TASK, "", 0, "b");
	}
	else
	{
		server_print("%L", LANG_SERVER, "MSG_DISABLED");
	}
	
	return PLUGIN_HANDLED;
}
