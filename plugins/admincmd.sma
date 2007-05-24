/* AMX Mod X
*   Admin Commands Plugin
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


new g_pauseCon
new Float:g_pausAble
new bool:g_Paused
new bool:g_PauseAllowed = false
new g_addCvar[] = "amx_cvar add %s"

new pausable;
new rcon_password;


public plugin_init()
{
	register_plugin("Admin Commands", AMXX_VERSION_STR, "AMXX Dev Team")

	register_dictionary("admincmd.txt")
	register_dictionary("common.txt")
	register_dictionary("adminhelp.txt")
	
	
	register_concmd("amx_kick", "cmdKick", ADMIN_KICK, "<name or #userid> [reason]")
	register_concmd("amx_ban", "cmdBan", ADMIN_BAN, "<name or #userid> <minutes> [reason]")
	register_concmd("amx_banip", "cmdBanIP", ADMIN_BAN, "<name or #userid> <minutes> [reason]")
	register_concmd("amx_addban", "cmdAddBan", ADMIN_RCON, "<authid or ip> <minutes> [reason]")
	register_concmd("amx_unban", "cmdUnban", ADMIN_BAN, "<authid or ip>")
	register_concmd("amx_slay", "cmdSlay", ADMIN_SLAY, "<name or #userid>")
	register_concmd("amx_slap", "cmdSlap", ADMIN_SLAY, "<name or #userid> [power]")
	register_concmd("amx_leave", "cmdLeave", ADMIN_KICK, "<tag> [tag] [tag] [tag]")
	register_concmd("amx_pause", "cmdPause", ADMIN_CVAR, "- pause or unpause the game")
	register_concmd("amx_who", "cmdWho", ADMIN_ADMIN, "- displays who is on server")
	register_concmd("amx_cvar", "cmdCvar", ADMIN_CVAR, "<cvar> [value]")
	register_concmd("amx_plugins", "cmdPlugins", ADMIN_ADMIN)
	register_concmd("amx_modules", "cmdModules", ADMIN_ADMIN)
	register_concmd("amx_map", "cmdMap", ADMIN_MAP, "<mapname>")
	register_concmd("amx_cfg", "cmdCfg", ADMIN_CFG, "<filename>")
	register_concmd("amx_nick", "cmdNick", ADMIN_SLAY, "<name or #userid> <new nick>")
	register_clcmd("amx_rcon", "cmdRcon", ADMIN_RCON, "<command line>")
	register_clcmd("amx_showrcon", "cmdShowRcon", ADMIN_RCON, "<command line>")
	register_clcmd("pauseAck", "cmdLBack")


	rcon_password=get_cvar_pointer("rcon_password");
	pausable=get_cvar_pointer("pausable");
	
	
}

public plugin_cfg()
{
	// Cvars which can be changed only with rcon access
	server_cmd(g_addCvar, "rcon_password")
	server_cmd(g_addCvar, "amx_show_activity")
	server_cmd(g_addCvar, "amx_mode")
	server_cmd(g_addCvar, "amx_password_field")
	server_cmd(g_addCvar, "amx_default_access")
	server_cmd(g_addCvar, "amx_reserved_slots")
	server_cmd(g_addCvar, "amx_reservation")
	server_cmd(g_addCvar, "amx_conmotd_file")
	server_cmd(g_addCvar, "amx_sql_table");
	server_cmd(g_addCvar, "amx_sql_host");
	server_cmd(g_addCvar, "amx_sql_user");
	server_cmd(g_addCvar, "amx_sql_pass");
	server_cmd(g_addCvar, "amx_sql_db");
	server_cmd(g_addCvar, "amx_sql_type");

}

public cmdKick(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
		return PLUGIN_HANDLED

	new arg[32]
	read_argv(1, arg, 31)
	new player = cmd_target(id, arg, 1)
	
	if (!player)
		return PLUGIN_HANDLED
	
	new authid[32], authid2[32], name2[32], name[32], userid2, reason[32]
	
	get_user_authid(id, authid, 31)
	get_user_authid(player, authid2, 31)
	get_user_name(player, name2, 31)
	get_user_name(id, name, 31)
	userid2 = get_user_userid(player)
	read_argv(2, reason, 31)
	remove_quotes(reason)
	
	log_amx("Kick: ^"%s<%d><%s><>^" kick ^"%s<%d><%s><>^" (reason ^"%s^")", name, get_user_userid(id), authid, name2, userid2, authid2, reason)

	show_activity_key("ADMIN_KICK_1", "ADMIN_KICK_2", name, name2);

	if (is_user_bot(player))
		server_cmd("kick #%d", userid2)
	else
	{
		if (reason[0])
			server_cmd("kick #%d ^"%s^"", userid2, reason)
		else
			server_cmd("kick #%d", userid2)
	}
	
	console_print(id, "[AMXX] Client ^"%s^" kicked", name2)
	
	return PLUGIN_HANDLED
}

public cmdUnban(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
		return PLUGIN_HANDLED
	
	new arg[32], authid[32], name[32]
	
	read_argv(1, arg, 31)
	
	if (contain(arg, ".") != -1)
	{
		server_cmd("removeip ^"%s^";writeip", arg)
		console_print(id, "[AMXX] %L", id, "IP_REMOVED", arg)
	} else {
		server_cmd("removeid %s;writeid", arg)
		console_print(id, "[AMXX] %L", id, "AUTHID_REMOVED", arg)
	}

	get_user_name(id, name, 31)

	show_activity_key("ADMIN_UNBAN_1", "ADMIN_UNBAN_2", name, arg);

	get_user_authid(id, authid, 31)
	log_amx("Cmd: ^"%s<%d><%s><>^" unban ^"%s^"", name, get_user_userid(id), authid, arg)
	
	return PLUGIN_HANDLED
}

public cmdAddBan(id, level, cid)
{
	if (!cmd_access(id, level, cid, 3))
		return PLUGIN_HANDLED

	new arg[32], authid[32], name[32], minutes[32], reason[32]
	
	read_argv(1, arg, 31)
	read_argv(2, minutes, 31)
	read_argv(3, reason, 31)
	
	if (contain(arg, ".") != -1)
	{
		server_cmd("addip ^"%s^" ^"%s^";wait;writeip", minutes, arg)
		console_print(id, "[AMXX] Ip ^"%s^" added to ban list", arg)
	} else {
		server_cmd("banid ^"%s^" ^"%s^";wait;writeid", minutes, arg)
		console_print(id, "[AMXX] Authid ^"%s^" added to ban list", arg)
	}

	get_user_name(id, name, 31)

	show_activity_key("ADMIN_ADDBAN_1", "ADMIN_ADDBAN_2", name, arg);

	get_user_authid(id, authid, 31)
	log_amx("Cmd: ^"%s<%d><%s><>^" ban ^"%s^" (minutes ^"%s^") (reason ^"%s^")", name, get_user_userid(id), authid, arg, minutes, reason)

	return PLUGIN_HANDLED
}

public cmdBan(id, level, cid)
{
	if (!cmd_access(id, level, cid, 3))
		return PLUGIN_HANDLED

	new target[32], minutes[8], reason[64]
	
	read_argv(1, target, 31)
	read_argv(2, minutes, 7)
	read_argv(3, reason, 63)
	
	new player = cmd_target(id, target, 9)
	
	if (!player)
		return PLUGIN_HANDLED

	new authid[32], name2[32], authid2[32], name[32]
	new userid2 = get_user_userid(player)

	get_user_authid(player, authid2, 31)
	get_user_authid(id, authid, 31)
	get_user_name(player, name2, 31)
	get_user_name(id, name, 31)
	
	log_amx("Ban: ^"%s<%d><%s><>^" ban and kick ^"%s<%d><%s><>^" (minutes ^"%s^") (reason ^"%s^")", name, get_user_userid(id), authid, name2, userid2, authid2, minutes, reason)
	
	new temp[64], banned[16], nNum = str_to_num(minutes)
	if (nNum)
		format(temp, 63, "%L", player, "FOR_MIN", minutes)
	else
		format(temp, 63, "%L", player, "PERM")

	format(banned, 15, "%L", player, "BANNED")

	if (reason[0])
		server_cmd("kick #%d ^"%s (%s %s)^";wait;banid ^"%s^" ^"%s^";wait;writeid", userid2, reason, banned, temp, minutes, authid2)
	else
		server_cmd("kick #%d ^"%s %s^";wait;banid ^"%s^" ^"%s^";wait;writeid", userid2, banned, temp, minutes, authid2)

	
	// Display the message to all clients

	new msg[256];
	new len;
	new maxpl = get_maxplayers();
	for (new i = 1; i <= maxpl; i++)
	{
		if (is_user_connected(i) && !is_user_bot(i))
		{
			len = formatex(msg, charsmax(msg), "%L", i, "BAN");
			len += formatex(msg[len], charsmax(msg) - len, " %s ", name2);
			if (nNum)
			{
				formatex(msg[len], charsmax(msg) - len, "%L", i, "FOR_MIN", minutes);
			}
			else
			{
				formatex(msg[len], charsmax(msg) - len, "%L", i, "PERM");
			}
			show_activity_id(i, id, name, msg);
		}
	}
	
	console_print(id, "[AMXX] %L", id, "CLIENT_BANNED", name2)
	
	return PLUGIN_HANDLED
}

public cmdBanIP(id, level, cid)
{
	if (!cmd_access(id, level, cid, 3))
		return PLUGIN_HANDLED
	
	new target[32], minutes[8], reason[64]
	
	read_argv(1, target, 31)
	read_argv(2, minutes, 7)
	read_argv(3, reason, 63)
	
	new player = cmd_target(id, target, 9)
	
	if (!player)
	{
		player = cmd_target(id, target, 9);
		return PLUGIN_HANDLED
	}
	
	new authid[32], name2[32], authid2[32], name[32]
	new userid2 = get_user_userid(player)
	
	get_user_authid(player, authid2, 31)
	get_user_authid(id, authid, 31)
	get_user_name(player, name2, 31)
	get_user_name(id, name, 31)
	
	log_amx("Ban: ^"%s<%d><%s><>^" ban and kick ^"%s<%d><%s><>^" (minutes ^"%s^") (reason ^"%s^")", name, get_user_userid(id), authid, name2, userid2, authid2, minutes, reason)

	new temp[64], banned[16], nNum = str_to_num(minutes)
	if (nNum)
		format(temp, 63, "%L", player, "FOR_MIN", minutes)
	else
		format(temp, 63, "%L", player, "PERM")
	format(banned, 15, "%L", player, "BANNED")

	new address[32]
	get_user_ip(player, address, 31, 1)

	if (reason[0])
		server_cmd("kick #%d ^"%s (%s %s)^";wait;addip ^"%s^" ^"%s^";wait;writeip", userid2, reason, banned, temp, minutes, address)
	else
		server_cmd("kick #%d ^"%s %s^";wait;addip ^"%s^" ^"%s^";wait;writeip", userid2, banned, temp, minutes, address)

	// Display the message to all clients

	new msg[256];
	new len;
	new maxpl = get_maxplayers();
	for (new i = 1; i <= maxpl; i++)
	{
		if (is_user_connected(i) && !is_user_bot(i))
		{
			len = formatex(msg, charsmax(msg), "%L", i, "BAN");
			len += formatex(msg[len], charsmax(msg) - len, " %s ", name2);
			if (nNum)
			{
				formatex(msg[len], charsmax(msg) - len, "%L", i, "FOR_MIN", minutes);
			}
			else
			{
				formatex(msg[len], charsmax(msg) - len, "%L", i, "PERM");
			}
			show_activity_id(i, id, name, msg);
		}
	}

	console_print(id, "[AMXX] %L", id, "CLIENT_BANNED", name2)
	
	return PLUGIN_HANDLED
}

public cmdSlay(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
		return PLUGIN_HANDLED
	
	new arg[32]
	
	read_argv(1, arg, 31)
	
	new player = cmd_target(id, arg, 5)
	
	if (!player)
		return PLUGIN_HANDLED
	
	user_kill(player)
	
	new authid[32], name2[32], authid2[32], name[32]
	
	get_user_authid(id, authid, 31)
	get_user_name(id, name, 31)
	get_user_authid(player, authid2, 31)
	get_user_name(player, name2, 31)
	
	log_amx("Cmd: ^"%s<%d><%s><>^" slay ^"%s<%d><%s><>^"", name, get_user_userid(id), authid, name2, get_user_userid(player), authid2)

	show_activity_key("ADMIN_SLAY_1", "ADMIN_SLAY_2", name, name2);

	console_print(id, "[AMXX] %L", id, "CLIENT_SLAYED", name2)
	
	return PLUGIN_HANDLED
}

public cmdSlap(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
		return PLUGIN_HANDLED

	new arg[32]
	
	read_argv(1, arg, 31)
	new player = cmd_target(id, arg, 5)
	
	if (!player)
		return PLUGIN_HANDLED

	new spower[32], authid[32], name2[32], authid2[32], name[32]
	
	read_argv(2, spower, 31)
	
	new damage = str_to_num(spower)
	
	user_slap(player, damage)
	
	get_user_authid(id, authid, 31)
	get_user_name(id, name, 31)
	get_user_authid(player, authid2, 31)
	get_user_name(player, name2, 31)
	
	log_amx("Cmd: ^"%s<%d><%s><>^" slap with %d damage ^"%s<%d><%s><>^"", name, get_user_userid(id), authid, damage, name2, get_user_userid(player), authid2)

	show_activity_key("ADMIN_SLAP_1", "ADMIN_SLAP_2", name, name2, damage);

	console_print(id, "[AMXX] %L", id, "CLIENT_SLAPED", name2, damage)
	
	return PLUGIN_HANDLED
}

public chMap(map[])
{
	server_cmd("changelevel %s", map)
}

public cmdMap(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
		return PLUGIN_HANDLED

	new arg[32]
	new arglen = read_argv(1, arg, 31)
	
	if (!is_map_valid(arg))
	{
		console_print(id, "[AMXX] %L", id, "MAP_NOT_FOUND")
		return PLUGIN_HANDLED
	}

	new authid[32], name[32]
	
	get_user_authid(id, authid, 31)
	get_user_name(id, name, 31)
	
	show_activity_key("ADMIN_MAP_1", "ADMIN_MAP_2", name, arg);
	
	log_amx("Cmd: ^"%s<%d><%s><>^" changelevel ^"%s^"", name, get_user_userid(id), authid, arg)
	
	new _modName[10]
	get_modname(_modName, 9)
	
	if (!equal(_modName, "zp"))
	{
		message_begin(MSG_ALL, SVC_INTERMISSION)
		message_end()
	}
	
	set_task(2.0, "chMap", 0, arg, arglen + 1)
	
	return PLUGIN_HANDLED
}

stock bool:onlyRcon(const name[])
{
	new ptr=get_cvar_pointer(name);
	if (ptr && get_pcvar_flags(ptr) & FCVAR_PROTECTED)
	{
		return true;
	}
	return false;
}

public cmdCvar(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
		return PLUGIN_HANDLED
	
	new arg[32], arg2[64]
	
	read_argv(1, arg, 31)
	read_argv(2, arg2, 63)
	
	new pointer;
	
	if (equal(arg, "add") && (get_user_flags(id) & ADMIN_RCON))
	{
		if ((pointer=get_cvar_pointer(arg2))!=0)
		{
			new flags=get_pcvar_flags(pointer);
			
			if (!(flags & FCVAR_PROTECTED))
			{
				set_pcvar_flags(pointer,flags | FCVAR_PROTECTED);
			}
		}
		return PLUGIN_HANDLED
	}
	
	if ((pointer=get_cvar_pointer(arg))==0)
	{
		console_print(id, "[AMXX] %L", id, "UNKNOWN_CVAR", arg)
		return PLUGIN_HANDLED
	}
	
	if (onlyRcon(arg) && !(get_user_flags(id) & ADMIN_RCON))
	{
		// Exception for the new onlyRcon rules:
		//   sv_password is allowed to be modified by ADMIN_PASSWORD
		if (!(equali(arg,"sv_password") && (get_user_flags(id) & ADMIN_PASSWORD)))
		{
			console_print(id, "[AMXX] %L", id, "CVAR_NO_ACC")
			return PLUGIN_HANDLED
		}
	}
	
	if (read_argc() < 3)
	{
		get_pcvar_string(pointer, arg2, 63)
		console_print(id, "[AMXX] %L", id, "CVAR_IS", arg, arg2)
		return PLUGIN_HANDLED
	}

	new authid[32], name[32]
	
	get_user_authid(id, authid, 31)
	get_user_name(id, name, 31)
	
	log_amx("Cmd: ^"%s<%d><%s><>^" set cvar (name ^"%s^") (value ^"%s^")", name, get_user_userid(id), authid, arg, arg2)
	set_cvar_string(arg, arg2)
	
	
	// Display the message to all clients

	new cvar_val[64];
	new maxpl = get_maxplayers();
	for (new i = 1; i <= maxpl; i++)
	{
		if (is_user_connected(i) && !is_user_bot(i))
		{
			if (get_pcvar_flags(pointer) & FCVAR_PROTECTED || equali(arg, "rcon_password"))
			{
				formatex(cvar_val, charsmax(cvar_val), "*** %L ***", i, "PROTECTED");
			}
			else
			{
				copy(cvar_val, charsmax(cvar_val), arg2);
			}
			show_activity_id(i, id, name, "%L", i, "SET_CVAR_TO", "", arg, cvar_val);
		}
	}

	console_print(id, "[AMXX] %L", id, "CVAR_CHANGED", arg, arg2)
	
	return PLUGIN_HANDLED
}

public cmdPlugins(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
		return PLUGIN_HANDLED
		
	if (id==0) // If server executes redirect this to "amxx plugins" for more in depth output
	{
		server_cmd("amxx plugins");
		return PLUGIN_HANDLED;
	}

	new name[32], version[32], author[32], filename[32], status[32]
	new lName[32], lVersion[32], lAuthor[32], lFile[32], lStatus[32]

	format(lName, 31, "%L", id, "NAME")
	format(lVersion, 31, "%L", id, "VERSION")
	format(lAuthor, 31, "%L", id, "AUTHOR")
	format(lFile, 31, "%L", id, "FILE")
	format(lStatus, 31, "%L", id, "STATUS")

	new StartPLID=0;
	new EndPLID;

	new Temp[96]

	new num = get_pluginsnum()
	
	if (read_argc() > 1)
	{
		read_argv(1,Temp,sizeof(Temp)-1);
		StartPLID=str_to_num(Temp)-1; // zero-based
	}

	EndPLID=min(StartPLID + 10, num);
	
	new running = 0
	
	console_print(id, "----- %L -----", id, "LOADED_PLUGINS")
	console_print(id, "%-18.17s %-8.7s %-17.16s %-16.15s %-9.8s", lName, lVersion, lAuthor, lFile, lStatus)

	new i=StartPLID;
	while (i <EndPLID)
	{
		get_plugin(i++, filename, 31, name, 31, version, 31, author, 31, status, 31)
		console_print(id, "%-18.17s %-8.7s %-17.16s %-16.15s %-9.8s", name, version, author, filename, status)
		
		if (status[0]=='d' || status[0]=='r') // "debug" or "running"
			running++
	}
	console_print(id, "%L", id, "PLUGINS_RUN", EndPLID-StartPLID, running)
	console_print(id, "----- %L -----",id,"HELP_ENTRIES",StartPLID + 1,EndPLID,num);
	
	if (EndPLID < num)
	{
		formatex(Temp,sizeof(Temp)-1,"----- %L -----",id,"HELP_USE_MORE", EndPLID + 1);
		replace_all(Temp,sizeof(Temp)-1,"amx_help","amx_plugins");
		console_print(id,"%s",Temp);
	}
	else
	{
		formatex(Temp,sizeof(Temp)-1,"----- %L -----",id,"HELP_USE_BEGIN");
		replace_all(Temp,sizeof(Temp)-1,"amx_help","amx_plugins");
		console_print(id,"%s",Temp);
	}

	return PLUGIN_HANDLED
}

public cmdModules(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
		return PLUGIN_HANDLED

	new name[32], version[32], author[32], status, sStatus[16]
	new lName[32], lVersion[32], lAuthor[32]

	format(lName, 31, "%L", id, "NAME")
	format(lVersion, 31, "%L", id, "VERSION")
	format(lAuthor, 31, "%L", id, "AUTHOR")

	new num = get_modulesnum()
	
	console_print(id, "%L:", id, "LOADED_MODULES")
	console_print(id, "%-23.22s %-8.7s %-20.19s", lName, lVersion, lAuthor)
	
	for (new i = 0; i < num; i++)
	{
		get_module(i, name, 31, author, 31, version, 31, status)
		
		switch (status)
		{
			case module_loaded: copy(sStatus, 15, "running")
			default: copy(sStatus, 15, "error")
		}
		
		console_print(id, "%-23.22s %-8.7s %-20.19s", name, version, author)
	}
	console_print(id, "%L", id, "NUM_MODULES", num)

	return PLUGIN_HANDLED
}

public cmdCfg(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
		return PLUGIN_HANDLED
	
	new arg[128]
	read_argv(1, arg, 127)
	
	if (!file_exists(arg))
	{
		console_print(id, "[AMXX] %L", id, "FILE_NOT_FOUND", arg)
		return PLUGIN_HANDLED
	}
	
	new authid[32], name[32]
	
	get_user_authid(id, authid, 31)
	get_user_name(id, name, 31)
	
	log_amx("Cmd: ^"%s<%d><%s><>^" execute cfg (file ^"%s^")", name, get_user_userid(id), authid, arg)
	
	console_print(id, "[AMXX] Executing file ^"%s^"", arg)
	server_cmd("exec %s", arg)

	show_activity_key("ADMIN_CONF_1", "ADMIN_CONF_2", name, arg);

	return PLUGIN_HANDLED
}

public cmdLBack()
{
	if (!g_PauseAllowed)
		return PLUGIN_CONTINUE	

	new paused[25]
	
	format(paused, 24, "%L", g_pauseCon, g_Paused ? "UNPAUSED" : "PAUSED")
	set_cvar_float("pausable", g_pausAble)
	console_print(g_pauseCon, "[AMXX] Server %s", paused)
	g_PauseAllowed = false
	
	if (g_Paused)
		g_Paused = false
	else 
		g_Paused = true
	
	return PLUGIN_HANDLED
}

public cmdPause(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
		return PLUGIN_HANDLED 
	
	new authid[32], name[32], slayer = id
	
	get_user_authid(id, authid, 31) 
	get_user_name(id, name, 31) 
	if (pausable!=0)
	{
		g_pausAble = get_pcvar_float(pausable)
	}
	
	if (!slayer)
		slayer = find_player("h") 
	
	if (!slayer)
	{ 
		console_print(id, "[AMXX] %L", id, "UNABLE_PAUSE") 
		return PLUGIN_HANDLED
	}

	set_cvar_float("pausable", 1.0)
	g_PauseAllowed = true
	client_cmd(slayer, "pause;pauseAck")
	
	log_amx("Cmd: ^"%s<%d><%s><>^" %s server", name, get_user_userid(id), authid, g_Paused ? "unpause" : "pause")
	
	console_print(id, "[AMXX] %L", id, g_Paused ? "UNPAUSING" : "PAUSING")

	// Display the message to all clients

	new maxpl = get_maxplayers();
	for (new i = 1; i <= maxpl; i++)
	{
		if (is_user_connected(i) && !is_user_bot(i))
		{
			show_activity_id(i, id, name, "%L server", i, g_Paused ? "UNPAUSE" : "PAUSE");
		}
	}

	g_pauseCon = id
	
	return PLUGIN_HANDLED
} 

public cmdShowRcon(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
		return PLUGIN_HANDLED
		
	new password[64]
	
	get_pcvar_string(rcon_password, password, 63)
	
	if (!password[0])
	{
		cmdRcon(id, level, cid)
	} else {
		new args[128]
		
		read_args(args, 127)
		client_cmd(id, "rcon_password %s", password)
		client_cmd(id, "rcon %s", args)
	}
	
	return PLUGIN_HANDLED
}

public cmdRcon(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
		return PLUGIN_HANDLED
	
	new arg[128], authid[32], name[32]
	
	read_args(arg, 127)
	get_user_authid(id, authid, 31)
	get_user_name(id, name, 31)
	
	log_amx("Cmd: ^"%s<%d><%s><>^" server console (cmdline ^"%s^")", name, get_user_userid(id), authid, arg)
	
	console_print(id, "[AMXX] %L", id, "COM_SENT_SERVER", arg)
	server_cmd("%s", arg)
	
	return PLUGIN_HANDLED
}

public cmdWho(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
		return PLUGIN_HANDLED

	new players[32], inum, cl_on_server[64], authid[32], name[32], flags, sflags[32]
	new lImm[16], lRes[16], lAccess[16], lYes[16], lNo[16]
	
	format(lImm, 15, "%L", id, "IMMU")
	format(lRes, 15, "%L", id, "RESERV")
	format(lAccess, 15, "%L", id, "ACCESS")
	format(lYes, 15, "%L", id, "YES")
	format(lNo, 15, "%L", id, "NO")
	
	get_players(players, inum)
	format(cl_on_server, 63, "%L", id, "CLIENTS_ON_SERVER")
	console_print(id, "^n%s:^n #  %-16.15s %-20s %-8s %-4.3s %-4.3s %s", cl_on_server, "nick", "authid", "userid", lImm, lRes, lAccess)
	
	for (new a = 0; a < inum; ++a)
	{
		get_user_authid(players[a], authid, 31)
		get_user_name(players[a], name, 31)
		flags = get_user_flags(players[a])
		get_flags(flags, sflags, 31)
		console_print(id, "%2d  %-16.15s %-20s %-8d %-6.5s %-6.5s %s", players[a], name, authid, 
		get_user_userid(players[a]), (flags&ADMIN_IMMUNITY) ? lYes : lNo, (flags&ADMIN_RESERVATION) ? lYes : lNo, sflags)
	}
	
	console_print(id, "%L", id, "TOTAL_NUM", inum)
	get_user_authid(id, authid, 31)
	get_user_name(id, name, 31)
	log_amx("Cmd: ^"%s<%d><%s><>^" ask for players list", name, get_user_userid(id), authid) 
	
	return PLUGIN_HANDLED
}

hasTag(name[], tags[4][32], tagsNum)
{
	for (new a = 0; a < tagsNum; ++a)
		if (contain(name, tags[a]) != -1)
			return a
	return -1
}

public cmdLeave(id, level, cid)
{
	if (!cmd_access(id, level, cid, 2))
		return PLUGIN_HANDLED
	
	new argnum = read_argc()
	new ltags[4][32]
	new ltagsnum = 0
	
	for (new a = 1; a < 5; ++a)
	{
		if (a < argnum)
			read_argv(a, ltags[ltagsnum++], 31)
		else
			ltags[ltagsnum++][0] = 0
	}
	
	new nick[32], ires, pnum = get_maxplayers() + 1, count = 0, lReason[128]
	
	for (new b = 1; b < pnum; ++b)
	{
		if (!is_user_connected(b) && !is_user_connecting(b)) continue

		get_user_name(b, nick, 31)
		ires = hasTag(nick, ltags, ltagsnum)
		
		if (ires != -1)
		{
			console_print(id, "[AMXX] %L", id, "SKIP_MATCH", nick, ltags[ires])
			continue
		}
		
		if (get_user_flags(b) & ADMIN_IMMUNITY)
		{
			console_print(id, "[AMXX] %L", id, "SKIP_IMM", nick)
			continue
		}
		
		console_print(id, "[AMXX] %L", id, "KICK_PL", nick)
		
		if (is_user_bot(b))
			server_cmd("kick #%d", get_user_userid(b))
		else
		{
			format(lReason, 127, "%L", b, "YOU_DROPPED")
			server_cmd("kick #%d ^"%s^"", get_user_userid(b), lReason)
		}
		count++
	}
	
	console_print(id, "[AMXX] %L", id, "KICKED_CLIENTS", count)
	
	new authid[32], name[32]

	get_user_authid(id, authid, 31)
	get_user_name(id, name, 31)
	log_amx("Kick: ^"%s<%d><%s><>^" leave some group (tag1 ^"%s^") (tag2 ^"%s^") (tag3 ^"%s^") (tag4 ^"%s^")", name, get_user_userid(id), authid, ltags[0], ltags[1], ltags[2], ltags[3])

	show_activity_key("ADMIN_LEAVE_1", "ADMIN_LEAVE_2", name, ltags[0], ltags[1], ltags[2], ltags[3]);

	return PLUGIN_HANDLED
}

public cmdNick(id, level, cid)
{
	if (!cmd_access(id, level, cid, 3))
		return PLUGIN_HANDLED

	new arg1[32], arg2[32], authid[32], name[32], authid2[32], name2[32]

	read_argv(1, arg1, 31)
	read_argv(2, arg2, 31)

	new player = cmd_target(id, arg1, 1)
	
	if (!player)
		return PLUGIN_HANDLED

	get_user_authid(id, authid, 31)
	get_user_name(id, name, 31)
	get_user_authid(player, authid2, 31)
	get_user_name(player, name2, 31)

	client_cmd(player, "name ^"%s^"", arg2)

	log_amx("Cmd: ^"%s<%d><%s><>^" change nick to ^"%s^" ^"%s<%d><%s><>^"", name, get_user_userid(id), authid, arg2, name2, get_user_userid(player), authid2)

	show_activity_key("ADMIN_NICK_1", "ADMIN_NICK_2", name, name2, arg2);

	console_print(id, "[AMXX] %L", id, "CHANGED_NICK", name2, arg2)

	return PLUGIN_HANDLED
}

