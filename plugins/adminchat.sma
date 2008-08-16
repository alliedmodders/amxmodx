/* AMX Mod X
*   Admin Chat Plugin
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

new g_msgChannel

#define MAX_CLR 10

new g_Colors[MAX_CLR][] = {"COL_WHITE", "COL_RED", "COL_GREEN", "COL_BLUE", "COL_YELLOW", "COL_MAGENTA", "COL_CYAN", "COL_ORANGE", "COL_OCEAN", "COL_MAROON"}
new g_Values[MAX_CLR][] = {{255, 255, 255}, {255, 0, 0}, {0, 255, 0}, {0, 0, 255}, {255, 255, 0}, {255, 0, 255}, {0, 255, 255}, {227, 96, 8}, {45, 89, 116}, {103, 44, 38}}
new Float:g_Pos[4][] = {{0.0, 0.0}, {0.05, 0.55}, {-1.0, 0.2}, {-1.0, 0.7}}

new amx_show_activity;
new g_AdminChatFlag = ADMIN_CHAT;

public plugin_init()
{
	new admin_chat_id

	register_plugin("Admin Chat", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("adminchat.txt")
	register_dictionary("common.txt")
	register_clcmd("say", "cmdSayChat", ADMIN_CHAT, "@[@|@|@][w|r|g|b|y|m|c]<text> - displays hud message")
	register_clcmd("say_team", "cmdSayAdmin", 0, "@<text> - displays message to admins")
	register_concmd("amx_say", "cmdSay", ADMIN_CHAT, "<message> - sends message to all players")
	admin_chat_id = register_concmd("amx_chat", "cmdChat", ADMIN_CHAT, "<message> - sends message to admins")
	register_concmd("amx_psay", "cmdPsay", ADMIN_CHAT, "<name or #userid> <message> - sends private message")
	register_concmd("amx_tsay", "cmdTsay", ADMIN_CHAT, "<color> <message> - sends left side hud message to all players")
	register_concmd("amx_csay", "cmdTsay", ADMIN_CHAT, "<color> <message> - sends center hud message to all players")
	
	amx_show_activity = get_cvar_pointer("amx_show_activity");
	
	if (amx_show_activity == 0)
	{
		amx_show_activity = register_cvar("amx_show_activity", "2");
	}

	new str[1]
	get_concmd(admin_chat_id, str, 0, g_AdminChatFlag, str, 0, -1)
}

public cmdSayChat(id)
{
	if (!access(id, g_AdminChatFlag))
	{
		return PLUGIN_CONTINUE
	}
	
	new said[6], i = 0
	read_argv(1, said, 5)
	
	while (said[i] == '@')
	{
		i++
	}
	
	if (!i || i > 3)
	{
		return PLUGIN_CONTINUE
	}
	
	new message[192], a = 0
	read_args(message, 191)
	remove_quotes(message)
	
	switch (said[i])
	{
		case 'r': a = 1
		case 'g': a = 2
		case 'b': a = 3
		case 'y': a = 4
		case 'm': a = 5
		case 'c': a = 6
		case 'o': a = 7
	}
	
	new n, s = i
	if (a)
	{
		n++
		s++
	}
	while (said[s] && isspace(said[s]))
	{
		n++
		s++
	}
	

	new name[32], authid[32], userid
	
	get_user_authid(id, authid, 31)
	get_user_name(id, name, 31)
	userid = get_user_userid(id)
	
	log_amx("Chat: ^"%s<%d><%s><>^" tsay ^"%s^"", name, userid, authid, message[i + n])
	log_message("^"%s<%d><%s><>^" triggered ^"amx_tsay^" (text ^"%s^") (color ^"%L^")", name, userid, authid, message[i + n], "en", g_Colors[a])
	
	if (++g_msgChannel > 6 || g_msgChannel < 3)
	{
		g_msgChannel = 3
	}
	
	new Float:verpos = g_Pos[i][1] + float(g_msgChannel) / 35.0
	
	set_hudmessage(g_Values[a][0], g_Values[a][1], g_Values[a][2], g_Pos[i][0], verpos, 0, 6.0, 6.0, 0.5, 0.15, -1)

	switch ( get_pcvar_num(amx_show_activity) )
	{
		case 3, 4:
		{
			new maxpl = get_maxplayers();
			for (new pl = 1; pl <= maxpl; pl++)
			{
				if (is_user_connected(pl) && !is_user_bot(pl))
				{
					if (is_user_admin(pl))
					{
						show_hudmessage(pl, "%s :   %s", name, message[i + n])
						client_print(pl, print_notify, "%s :   %s", name, message[i + n])
					}
					else
					{
						show_hudmessage(pl, "%s", message[i + n])
						client_print(pl, print_notify, "%s", message[i + n])
					}
				}
			}
		}
		case 2:
		{
			show_hudmessage(0, "%s :   %s", name, message[i + n])
			client_print(0, print_notify, "%s :   %s", name, message[i + n])
		}
		default:
		{
			show_hudmessage(0, "%s", message[i + n])
			client_print(0, print_notify, "%s", message[i + n])
		}
	}

	return PLUGIN_HANDLED
}

public cmdSayAdmin(id)
{
	new said[2]
	read_argv(1, said, 1)
	
	if (said[0] != '@')
		return PLUGIN_CONTINUE
	
	new message[192], name[32], authid[32], userid
	new players[32], inum
	
	read_args(message, 191)
	remove_quotes(message)
	get_user_authid(id, authid, 31)
	get_user_name(id, name, 31)
	userid = get_user_userid(id)
	
	log_amx("Chat: ^"%s<%d><%s><>^" chat ^"%s^"", name, userid, authid, message[1])
	log_message("^"%s<%d><%s><>^" triggered ^"amx_chat^" (text ^"%s^")", name, userid, authid, message[1])
	
	if (is_user_admin(id))
		format(message, 191, "(%L) %s :  %s", id, "ADMIN", name, message[1])
	else
		format(message, 191, "(%L) %s :  %s", id, "PLAYER", name, message[1])

	get_players(players, inum)
	
	for (new i = 0; i < inum; ++i)
	{
		// dont print the message to the client that used the cmd if he has ADMIN_CHAT to avoid double printing
		if (players[i] != id && get_user_flags(players[i]) & g_AdminChatFlag)
			client_print(players[i], print_chat, "%s", message)
	}
	
	client_print(id, print_chat, "%s", message)
	
	return PLUGIN_HANDLED
}

public cmdChat(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
		return PLUGIN_HANDLED

	new message[192], name[32], players[32], inum, authid[32], userid
	
	read_args(message, 191)
	remove_quotes(message)
	get_user_authid(id, authid, 31)
	get_user_name(id, name, 31)
	userid = get_user_userid(id)
	get_players(players, inum)
	
	log_amx("Chat: ^"%s<%d><%s><>^" chat ^"%s^"", name, userid, authid, message)
	log_message("^"%s<%d><%s><>^" triggered ^"amx_chat^" (text ^"%s^")", name, userid, authid, message)
	
	format(message, 191, "(ADMINS) %s :   %s", name, message)
	console_print(id, "%s", message)
	
	for (new i = 0; i < inum; ++i)
	{
		if (access(players[i], g_AdminChatFlag))
			client_print(players[i], print_chat, "%s", message)
	}
	
	return PLUGIN_HANDLED
}

public cmdSay(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
		return PLUGIN_HANDLED

	new message[192], name[32], authid[32], userid
	
	read_args(message, 191)
	remove_quotes(message)
	get_user_authid(id, authid, 31)
	get_user_name(id, name, 31)
	userid = get_user_userid(id)
	client_print(0, print_chat, "%L", LANG_PLAYER, "PRINT_ALL", name, message)
	console_print(id, "%L", LANG_PLAYER, "PRINT_ALL", name, message)
	
	log_amx("Chat: ^"%s<%d><%s><>^" say ^"%s^"", name, userid, authid, message)
	log_message("^"%s<%d><%s><>^" triggered ^"amx_say^" (text ^"%s^")", name, userid, authid, message)
	
	return PLUGIN_HANDLED
}

public cmdPsay(id, level, cid)
{
	if (!cmd_access(id, level, cid, 3))
		return PLUGIN_HANDLED
	
	new name[32]
	read_argv(1, name, 31)
	new priv = cmd_target(id, name, 0)

	if (!priv)
		return PLUGIN_HANDLED
	
	new length = strlen(name) + 1

	get_user_name(priv, name, 31); 
	
	new message[192], name2[32], authid[32], authid2[32], userid, userid2
	
	get_user_authid(id, authid, 31)
	get_user_name(id, name2, 31)
	userid = get_user_userid(id)
	read_args(message, 191)
	
	if (message[0] == '"' && message[length] == '"') // HLSW fix
	{
		message[0] = ' '
		message[length] = ' '
		length += 2
	}
	
	remove_quotes(message[length])
	get_user_name(priv, name, 31)
	
	if (id && id != priv)
		client_print(id, print_chat, "(%s) %s :   %s", name, name2, message[length])
	
	client_print(priv, print_chat, "(%s) %s :   %s", name, name2, message[length])
	console_print(id, "(%s) %s :   %s", name, name2, message[length])
	get_user_authid(priv, authid2, 31)
	userid2 = get_user_userid(priv)
	
	log_amx("Chat: ^"%s<%d><%s><>^" psay ^"%s<%d><%s><>^" ^"%s^"", name2, userid, authid, name, userid2, authid2, message[length])
	log_message("^"%s<%d><%s><>^" triggered ^"amx_psay^" against ^"%s<%d><%s><>^" (text ^"%s^")", name2, userid, authid, name, userid2, authid2, message[length])
	
	return PLUGIN_HANDLED
}

public cmdTsay(id, level, cid)
{
	if (!cmd_access(id, level, cid, 3))
		return PLUGIN_HANDLED
	
	new cmd[16], color[16], color2[16], message[192], name[32], authid[32], userid = 0
	
	read_argv(0, cmd, 15)
	new bool:tsay = (tolower(cmd[4]) == 't')
	
	read_args(message, 191)
	remove_quotes(message)
	parse(message, color, 15)
	
	new found = 0, a = 0
	new lang[3], langnum = get_langsnum()

	for (new i = 0; i < MAX_CLR; ++i)
	{
		for (new j = 0; j < langnum; j++)
		{
			get_lang(j, lang)
			format(color2, 15, "%L", lang, g_Colors[i])
			
			if (equali(color, color2))
			{
				a = i
				found = 1
				break
			}
		}
		if (found == 1)
			break
	}
	
	new length = found ? (strlen(color) + 1) : 0
	
	if (++g_msgChannel > 6 || g_msgChannel < 3)
		g_msgChannel = 3

	new Float:verpos = (tsay ? 0.55 : 0.1) + float(g_msgChannel) / 35.0
	
	get_user_authid(id, authid, 31)
	get_user_name(id, name, 31)
	userid = get_user_userid(id)
	set_hudmessage(g_Values[a][0], g_Values[a][1], g_Values[a][2], tsay ? 0.05 : -1.0, verpos, 0, 6.0, 6.0, 0.5, 0.15, -1)

	switch ( get_pcvar_num(amx_show_activity) )
	{
		case 3, 4:
		{
			new maxpl = get_maxplayers();
			for (new pl = 1; pl <= maxpl; pl++)
			{
				if (is_user_connected(pl) && !is_user_bot(pl))
				{
					if (is_user_admin(pl))
					{
						show_hudmessage(pl, "%s :   %s", name, message[length])
						client_print(pl, print_notify, "%s :   %s", name, message[length])
					}
					else
					{
						show_hudmessage(pl, "%s", message[length])
						client_print(pl, print_notify, "%s", message[length])
					}
				}
			}
			console_print(id, "%s :  %s", name, message[length])
		}
		case 2:
		{
			show_hudmessage(0, "%s :   %s", name, message[length])
			client_print(0, print_notify, "%s :   %s", name, message[length])
			console_print(id, "%s :  %s", name, message[length])
		}
		default:
		{
			show_hudmessage(0, "%s", message[length])
			client_print(0, print_notify, "%s", message[length])
			console_print(id, "%s", message[length])
		}
	}

	log_amx("Chat: ^"%s<%d><%s><>^" %s ^"%s^"", name, userid, authid, cmd[4], message[length])
	log_message("^"%s<%d><%s><>^" triggered ^"%s^" (text ^"%s^") (color ^"%s^")", name, userid, authid, cmd, message[length], color2)

	return PLUGIN_HANDLED
}
