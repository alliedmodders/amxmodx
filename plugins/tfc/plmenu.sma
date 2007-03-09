/* AMX Mod X
*   Players Menu Plugin
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
#include <tfcx>

new g_menuPosition[33]
new g_menuPlayers[33][32]
new g_menuPlayersNum[33]
new g_menuOption[33]
new g_menuSettings[33]

new g_menuSelect[33][64]
new g_menuSelectNum[33]

#define MAX_CLCMDS 24

new g_clcmdName[MAX_CLCMDS][32]
new g_clcmdCmd[MAX_CLCMDS][64]
new g_clcmdMisc[MAX_CLCMDS][2]
new g_clcmdNum

new g_coloredMenus

new g_teamNames[6][] = {"", "Blue", "Red", "Yellow", "Green", "Spectator"}

public plugin_init()
{
	register_plugin("Players Menu",AMXX_VERSION_STR,"AMXX Dev Team")

	register_dictionary("plmenu.txt")
	register_dictionary("common.txt")
	register_dictionary("admincmd.txt")

	register_clcmd("amx_kickmenu","cmdKickMenu",ADMIN_KICK,"- displays kick menu")
	register_clcmd("amx_banmenu","cmdBanMenu",ADMIN_BAN,"- displays ban menu")
	register_clcmd("amx_slapmenu","cmdSlapMenu",ADMIN_SLAY,"- displays slap/slay menu")
	register_clcmd("amx_teammenu","cmdTeamMenu",ADMIN_LEVEL_A,"- displays team menu")
	register_clcmd("amx_clcmdmenu","cmdClcmdMenu",ADMIN_LEVEL_A,"- displays client cmds menu")

	register_menucmd(register_menuid("Ban Menu"),1023,"actionBanMenu")
	register_menucmd(register_menuid("Kick Menu"),1023,"actionKickMenu")
	register_menucmd(register_menuid("Slap/Slay Menu"),1023,"actionSlapMenu")
	register_menucmd(register_menuid("Team Menu"),1023,"actionTeamMenu")
	register_menucmd(register_menuid("Client Cmds Menu"),1023,"actionClcmdMenu")

	g_coloredMenus = colored_menus()

	new clcmds_ini_file[64]
	get_configsdir(clcmds_ini_file, 63)
	format(clcmds_ini_file, 63, "%s/clcmds.ini", clcmds_ini_file)
	load_settings(clcmds_ini_file)
}

/* Ban menu */

public actionBanMenu(id, key)
{
	switch (key)
	{
		case 7:
		{
			++g_menuOption[id]
			g_menuOption[id] %= 3
      
			switch(g_menuOption[id])
			{
				case 0: g_menuSettings[id] = 0
				case 1: g_menuSettings[id] = 5
				case 2: g_menuSettings[id] = 60
			}     
      
			displayBanMenu(id, g_menuPosition[id])
		}
		
		case 8: displayBanMenu(id, ++g_menuPosition[id])
		case 9: displayBanMenu(id, --g_menuPosition[id])
		
		default:
		{
			new player = g_menuPlayers[id][g_menuPosition[id] * 7 + key]
      
			new name[32], name2[32], authid[32], authid2[32]
			get_user_name(player, name2, 31)
			get_user_authid(id, authid, 31)
			get_user_authid(player, authid2, 31)
			get_user_name(id, name, 31)
			new userid2 = get_user_userid(player)

			log_amx("Ban: ^"%s<%d><%s><>^" ban and kick ^"%s<%d><%s><>^" (minutes ^"%d^")", name, get_user_userid(id), authid, name2, userid2, authid2, g_menuSettings[id])

			switch (get_cvar_num("amx_show_activity"))
			{
				case 2: 
				{
					if (g_menuSettings[id]==0) // permanent
					{
						client_print(0, print_chat, "%L %s: %L %s %L", LANG_PLAYER, "ADMIN", name, LANG_PLAYER, "BAN", name2, LANG_PLAYER, "PERM");
					}
					else
					{
						new tempTime[32];
						formatex(tempTime,sizeof(tempTime)-1,"%d",g_menuSettings[id]);
						client_print(0, print_chat, "%L %s: %L %s %L", LANG_PLAYER, "ADMIN", name, LANG_PLAYER, "BAN", name2, LANG_PLAYER, "FOR_MIN", tempTime);
					}
				}
				case 1: 
				{
					if (g_menuSettings[id]==0) // permanent
					{
						client_print(0, print_chat, "%L: %L %s %L", LANG_PLAYER, "ADMIN", LANG_PLAYER, "BAN", name2, LANG_PLAYER, "PERM");
					}
					else
					{
						new tempTime[32];
						formatex(tempTime,sizeof(tempTime)-1,"%d",g_menuSettings[id]);
						client_print(0, print_chat, "%L: %L %s %L", LANG_PLAYER, "ADMIN", LANG_PLAYER, "BAN", name2, LANG_PLAYER, "FOR_MIN", tempTime);
					}
				}
			}

			if (equal("4294967295", authid2))
			{
				new ipa[32]
				get_user_ip(player, ipa, 31, 1)
				server_cmd("addip %d %s;writeip", g_menuSettings[id], ipa)
			}
			else
				server_cmd("banid %d #%d kick;writeid", g_menuSettings[id], userid2)

			server_exec()

			displayBanMenu(id, g_menuPosition[id])
		}
	}
	
	return PLUGIN_HANDLED
}

displayBanMenu(id, pos)
{
	if (pos < 0)
		return

	get_players(g_menuPlayers[id], g_menuPlayersNum[id])

	new menuBody[512]
	new b = 0
	new i
	new name[32]
	new start = pos * 7

	if (start >= g_menuPlayersNum[id])
		start = pos = g_menuPosition[id] = 0

	new len = format(menuBody, 511, g_coloredMenus ? "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n", id, "BAN_MENU", pos + 1, (g_menuPlayersNum[id] / 7 + ((g_menuPlayersNum[id] % 7) ? 1 : 0)))
	new end = start + 7
	new keys = MENU_KEY_0|MENU_KEY_8

	if (end > g_menuPlayersNum[id])
		end = g_menuPlayersNum[id]

	for (new a = start; a < end; ++a)
	{
		i = g_menuPlayers[id][a]
		get_user_name(i, name, 31)

		if (is_user_bot(i) || access(i, ADMIN_IMMUNITY))
		{
			++b
			
			if (g_coloredMenus)
				len += format(menuBody[len], 511-len, "\d%d. %s^n\w", b, name)
			else
				len += format(menuBody[len], 511-len, "#. %s^n", name)
		} else {
			keys |= (1<<b)
				
			if (is_user_admin(i))
				len += format(menuBody[len], 511-len, g_coloredMenus ? "%d. %s \r*^n\w" : "%d. %s *^n", ++b, name)
			else
				len += format(menuBody[len], 511-len, "%d. %s^n", ++b, name)
		}
	}

	if (g_menuSettings[id])
		len += format(menuBody[len], 511-len, "^n8. %L^n", id, "BAN_FOR_MIN", g_menuSettings[id])
	else
		len += format(menuBody[len], 511-len, "^n8. %L^n", id, "BAN_PERM")

	if (end != g_menuPlayersNum[id])
	{
		format(menuBody[len], 511-len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT")
		keys |= MENU_KEY_9
	}
	else
		format(menuBody[len], 511-len, "^n0. %L", id, pos ? "BACK" : "EXIT")

	show_menu(id, keys, menuBody, -1, "Ban Menu")
}

public cmdBanMenu(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
		return PLUGIN_HANDLED

	g_menuOption[id] = 1
	g_menuSettings[id] = 5
	displayBanMenu(id, g_menuPosition[id] = 0)

	return PLUGIN_HANDLED
}

/* Slap/Slay */

public actionSlapMenu(id,key) {
  switch (key) {
    case 7: {
      ++g_menuOption[id]
      g_menuOption[id] %= 4
      switch (g_menuOption[id]) {
        case 1: g_menuSettings[id] = 0
        case 2: g_menuSettings[id] = 1
        case 3: g_menuSettings[id] = 5
      }
      displaySlapMenu(id,g_menuPosition[id])
    }
    case 8: displaySlapMenu(id,++g_menuPosition[id])
    case 9: displaySlapMenu(id,--g_menuPosition[id])
    default: {
      new player = g_menuPlayers[id][g_menuPosition[id] * 7 + key]
      
      new name2[32]
      get_user_name(player,name2,31)
      
      if (!is_user_alive(player)) {
        client_print(id,print_chat,"%L",id,"CANT_PERF_DEAD",name2)
        displaySlapMenu(id,g_menuPosition[id])
        return PLUGIN_HANDLED
      }
            
      new authid[32],authid2[32], name[32]

      get_user_authid(id,authid,31)
      get_user_authid(player,authid2,31)
      get_user_name(id,name,31)
        
      if ( g_menuOption[id] ) {
        log_amx("Cmd: ^"%s<%d><%s><>^" slap with %d damage ^"%s<%d><%s><>^"", 
          name,get_user_userid(id),authid, g_menuSettings[id], name2,get_user_userid(player),authid2 )
        switch (get_cvar_num("amx_show_activity")) {
          case 2: client_print(0,print_chat,"%L",LANG_PLAYER,"ADMIN_SLAP_2",name,name2,g_menuSettings[id])
          case 1: client_print(0,print_chat,"%L",LANG_PLAYER,"ADMIN_SLAP_1",name2,g_menuSettings[id])
        }     
      }
      else {
        log_amx("Cmd: ^"%s<%d><%s><>^" slay ^"%s<%d><%s><>^"", 
          name,get_user_userid(id),authid, name2,get_user_userid(player),authid2 )
        switch(get_cvar_num("amx_show_activity")) {
          case 2: client_print(0,print_chat,"%L",LANG_PLAYER,"ADMIN_SLAY_2",name,name2)
          case 1: client_print(0,print_chat,"%L",LANG_PLAYER,"ADMIN_SLAY_1",name2)
        }
      }
      
      if ( g_menuOption[id])
        user_slap(player, ( get_user_health(player) >  g_menuSettings[id]  ) ? g_menuSettings[id] : 0 )
      else
        user_kill( player )
      
      displaySlapMenu(id,g_menuPosition[id])
    }
  }
  return PLUGIN_HANDLED
}

displaySlapMenu(id, pos)
{
	if (pos < 0)
		return

	get_players(g_menuPlayers[id], g_menuPlayersNum[id])

	new menuBody[512]
	new b = 0
	new i
	new name[32]
	new start = pos * 7

	if (start >= g_menuPlayersNum[id])
		start = pos = g_menuPosition[id] = 0

	new len = format(menuBody, 511, g_coloredMenus ? "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n", id, "SLAP_SLAY_MENU", pos + 1, (g_menuPlayersNum[id] / 7 + ((g_menuPlayersNum[id] % 7) ? 1 : 0)))
	new end = start + 7
	new keys = MENU_KEY_0|MENU_KEY_8

	if (end > g_menuPlayersNum[id])
		end = g_menuPlayersNum[id]

	for (new a = start; a < end; ++a)
	{
		i = g_menuPlayers[id][a]
		get_user_name(i, name, 31)
		new iteam = get_user_team(i)

		if (!is_user_alive(i) || access(i, ADMIN_IMMUNITY))
		{
			++b
		
			if (g_coloredMenus)
				len += format(menuBody[len], 511-len, "\d%d. %s\R%s^n\w", b, name, g_teamNames[iteam])
			else
				len += format(menuBody[len], 511-len, "#. %s   %s^n", name, g_teamNames[iteam])		
		} else {
			keys |= (1<<b)
				
			if (is_user_admin(i))
				len += format(menuBody[len], 511-len, g_coloredMenus ? "%d. %s \r*\y\R%s^n\w" : "%d. %s *   %s^n", ++b, name, g_teamNames[iteam])
			else
				len += format(menuBody[len], 511-len, g_coloredMenus ? "%d. %s\y\R%s^n\w" : "%d. %s   %s^n", ++b, name, g_teamNames[iteam])
		}
	}

	if (g_menuOption[id])
		len += format(menuBody[len], 511-len, "^n8. %L^n", id, "SLAP_WITH_DMG", g_menuSettings[id])
	else
		len += format(menuBody[len], 511-len, "^n8. %L^n", id, "SLAY")

	if (end != g_menuPlayersNum[id])
	{
		format(menuBody[len], 511-len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT")
		keys |= MENU_KEY_9
	}
	else
		format(menuBody[len], 511-len, "^n0. %L", id, pos ? "BACK" : "EXIT")

	show_menu(id, keys, menuBody, -1, "Slap/Slay Menu")
}

public cmdSlapMenu(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
		return PLUGIN_HANDLED

	g_menuOption[id] = 0
	g_menuSettings[id] = 0

	displaySlapMenu(id, g_menuPosition[id] = 0)

	return PLUGIN_HANDLED
}

/* Kick */

public actionKickMenu(id,key)
{
  switch (key) {
    case 8: displayKickMenu(id,++g_menuPosition[id])
    case 9: displayKickMenu(id,--g_menuPosition[id])
    default: {
      new player = g_menuPlayers[id][g_menuPosition[id] * 8 + key]

      new authid[32],authid2[32], name[32], name2[32]
      get_user_authid(id,authid,31)
      get_user_authid(player,authid2,31)
      get_user_name(id,name,31)
      get_user_name(player,name2,31)      
      new userid2 = get_user_userid(player)

      log_amx("Kick: ^"%s<%d><%s><>^" kick ^"%s<%d><%s><>^"", 
          name,get_user_userid(id),authid, name2,userid2,authid2 )

      switch (get_cvar_num("amx_show_activity")) {
        case 2: client_print(0,print_chat,"%L",LANG_PLAYER,"ADMIN_KICK_2",name,name2)
        case 1: client_print(0,print_chat,"%L",LANG_PLAYER,"ADMIN_KICK_1",name2)
      }

      server_cmd("kick #%d",userid2)
      server_exec()
            
      displayKickMenu(id,g_menuPosition[id])
    }
  }
  return PLUGIN_HANDLED
}

displayKickMenu(id, pos)
{
	if (pos < 0)
		return

	get_players(g_menuPlayers[id], g_menuPlayersNum[id])

	new menuBody[512]
	new b = 0
	new i
	new name[32]
	new start = pos * 8

	if (start >= g_menuPlayersNum[id])
		start = pos = g_menuPosition[id] = 0

	new len = format(menuBody, 511, g_coloredMenus ? "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n", id, "KICK_MENU", pos + 1, (g_menuPlayersNum[id] / 8 + ((g_menuPlayersNum[id] % 8) ? 1 : 0)))
	new end = start + 8
	new keys = MENU_KEY_0

	if (end > g_menuPlayersNum[id])
		end = g_menuPlayersNum[id]

	for (new a = start; a < end; ++a)
	{
		i = g_menuPlayers[id][a]
		get_user_name(i, name, 31)

		if (access(i, ADMIN_IMMUNITY))
		{
			++b
		
			if (g_coloredMenus)
				len += format(menuBody[len], 511-len, "\d%d. %s^n\w", b, name)
			else
				len += format(menuBody[len], 511-len, "#. %s^n", name)
		} else {
			keys |= (1<<b)
				
			if (is_user_admin(i))
				len += format(menuBody[len], 511-len, g_coloredMenus ? "%d. %s \r*^n\w" : "%d. %s *^n", ++b, name)
			else
				len += format(menuBody[len], 511-len, "%d. %s^n", ++b, name)
		}
	}

	if (end != g_menuPlayersNum[id])
	{
		format(menuBody[len], 511-len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT")
		keys |= MENU_KEY_9
	}
	else
		format(menuBody[len], 511-len, "^n0. %L", id, pos ? "BACK" : "EXIT")

	show_menu(id, keys, menuBody, -1, "Kick Menu")
}

public cmdKickMenu(id, level, cid)
{
	if (cmd_access(id, level, cid, 1))
		displayKickMenu(id, g_menuPosition[id] = 0)

	return PLUGIN_HANDLED
}

/* Team menu */

public actionTeamMenu(id,key) {
  switch (key) {
    case 7:{
      g_menuOption[id]++
      if (g_menuOption[id] > 5)
        g_menuOption[id] = 1
      displayTeamMenu(id,g_menuPosition[id])
    }
    case 8: displayTeamMenu(id,++g_menuPosition[id])
    case 9: displayTeamMenu(id,--g_menuPosition[id])
    default: {
      new player = g_menuPlayers[id][g_menuPosition[id] * 7 + key]
      new authid[32],authid2[32], name[32], name2[32]
      get_user_name(player,name2,31)
      get_user_authid(id,authid,31)
      get_user_authid(player,authid2,31)
      get_user_name(id,name,31)

      log_amx("Cmd: ^"%s<%d><%s><>^" transfer ^"%s<%d><%s><>^" (team ^"%s^")", 
          name,get_user_userid(id),authid, name2,get_user_userid(player),authid2, g_teamNames[g_menuOption[id]] )
      switch (get_cvar_num("amx_show_activity")) {
        case 2: client_print(0, print_chat, "%L", LANG_PLAYER, "ADMIN_TRANSF_2", name, name2, g_teamNames[g_menuOption[id]])
        case 1: client_print(0, print_chat, "%L", LANG_PLAYER, "ADMIN_TRANSF_1", name2, g_teamNames[g_menuOption[id]])
      }

      new szCmd[2]
      format(szCmd,2,"%d",g_menuOption[id])
      tfc_userkill(player)
      if (g_menuOption[id] == 5)
      {
        engclient_cmd(player, "spectate")
      } else {
        engclient_cmd(player, "jointeam", szCmd )
      }


      displayTeamMenu(id,g_menuPosition[id])
    }
  }
  return PLUGIN_HANDLED
}

displayTeamMenu(id, pos)
{
	if (pos < 0)
		return

	get_players(g_menuPlayers[id], g_menuPlayersNum[id])

	new menuBody[512]
	new b = 0
	new i, iteam
	new name[32]
	new start = pos * 7

	if (start >= g_menuPlayersNum[id])
		start = pos = g_menuPosition[id] = 0

	new len = format(menuBody, 511, g_coloredMenus ? "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n", id, "TEAM_MENU", pos + 1, (g_menuPlayersNum[id] / 7 + ((g_menuPlayersNum[id] % 7) ? 1 : 0)))
	new end = start + 7
	new keys = MENU_KEY_0|MENU_KEY_8

	if (end > g_menuPlayersNum[id])
		end = g_menuPlayersNum[id]

	for (new a = start; a < end; ++a)
	{
		i = g_menuPlayers[id][a]
		get_user_name(i, name, 31)
		iteam = get_user_team(i)

		if ((iteam == g_menuOption[id]) || access(i, ADMIN_IMMUNITY))
		{
			++b
			
			if (g_coloredMenus)
				len += format(menuBody[len], 511-len, "\d%d. %s\R%s^n\w", b, name, g_teamNames[iteam])
			else
				len += format(menuBody[len], 511-len, "#. %s   %s^n", name, g_teamNames[iteam])		
		} else {
			keys |= (1<<b)
				
			if (is_user_admin(i))
				len += format(menuBody[len], 511-len, g_coloredMenus ? "%d. %s \r*\y\R%s^n\w" : "%d. %s *   %s^n", ++b, name, g_teamNames[iteam])
			else
				len += format(menuBody[len], 511-len, g_coloredMenus ? "%d. %s\y\R%s^n\w" : "%d. %s   %s^n", ++b, name, g_teamNames[iteam])
		}
	}

	len += format(menuBody[len], 511-len, "^n8. %L^n", id, "TRANSF_TO", g_teamNames[g_menuOption[id]])

	if (end != g_menuPlayersNum[id])
	{
		format(menuBody[len], 511-len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT")
		keys |= MENU_KEY_9
	}
	else
		format(menuBody[len], 511-len, "^n0. %L", id, pos ? "BACK" : "EXIT")

	show_menu(id, keys, menuBody, -1, "Team Menu")
}

public cmdTeamMenu(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
		return PLUGIN_HANDLED

	g_menuOption[id] = 1

	displayTeamMenu(id, g_menuPosition[id] = 0)

	return PLUGIN_HANDLED
}

/* Client cmds menu */

public actionClcmdMenu(id,key) {
  switch (key) {
    case 7:{
      ++g_menuOption[id]
      g_menuOption[id] %= g_menuSelectNum[id]
      displayClcmdMenu(id,g_menuPosition[id])
    }
    case 8: displayClcmdMenu(id,++g_menuPosition[id])
    case 9: displayClcmdMenu(id,--g_menuPosition[id])
    default: {
      new player = g_menuPlayers[id][g_menuPosition[id] * 7 + key]
      new flags = g_clcmdMisc[g_menuSelect[id][g_menuOption[id]]][1]
      if (is_user_connected(player)) {
        new command[64], authid[32], name[32], userid[32]
        copy(command,63,g_clcmdCmd[g_menuSelect[id][g_menuOption[id]]])
        get_user_authid(player,authid,31)
        get_user_name(player,name,31)
        num_to_str(get_user_userid(player),userid,31)
        replace(command,63,"%userid%",userid)
        replace(command,63,"%authid%",authid)
        replace(command,63,"%name%",name)
        if (flags & 1) {
          server_cmd("%s", command)
          server_exec()
        }
        else if (flags & 2)
          client_cmd(id, "%s", command)
        else if (flags & 4)
          client_cmd(player, "%s", command)
      }
      if (flags & 8) displayClcmdMenu(id,g_menuPosition[id])
    }
  }
  return PLUGIN_HANDLED
}

displayClcmdMenu(id, pos)
{
	if (pos < 0)
		return

	get_players(g_menuPlayers[id], g_menuPlayersNum[id])

	new menuBody[512]
	new b = 0
	new i
	new name[32]
	new start = pos * 7

	if (start >= g_menuPlayersNum[id])
		start = pos = g_menuPosition[id] = 0

	new len = format(menuBody, 511, g_coloredMenus ? "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n", id, "CL_CMD_MENU", pos + 1, (g_menuPlayersNum[id] / 7 + ((g_menuPlayersNum[id] % 7) ? 1 : 0)))
	new end = start + 7
	new keys = MENU_KEY_0|MENU_KEY_8

	if (end > g_menuPlayersNum[id])
		end = g_menuPlayersNum[id]

	for (new a = start; a < end; ++a)
	{
		i = g_menuPlayers[id][a]
		get_user_name(i, name, 31)

		if (!g_menuSelectNum[id] || access(i, ADMIN_IMMUNITY))
		{
			++b
			
			if (g_coloredMenus)
				len += format(menuBody[len], 511-len, "\d%d. %s^n\w", b, name)
			else
				len += format(menuBody[len], 511-len, "#. %s^n", name)		
		} else {
			keys |= (1<<b)
				
			if (is_user_admin(i))
				len += format(menuBody[len], 511-len, g_coloredMenus ? "%d. %s \r*^n\w" : "%d. %s *^n", ++b, name)
			else
				len += format(menuBody[len], 511-len, "%d. %s^n", ++b, name)
		}
	}

	if (g_menuSelectNum[id])
		len += format(menuBody[len], 511-len, "^n8. %s^n", g_clcmdName[g_menuSelect[id][g_menuOption[id]]])
	else
		len += format(menuBody[len], 511-len, "^n8. %L^n", id, "NO_CMDS")

	if (end != g_menuPlayersNum[id])
	{
		format(menuBody[len], 511-len, "^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT")
		keys |= MENU_KEY_9
	}
	else
		format(menuBody[len], 511-len, "^n0. %L", id, pos ? "BACK" : "EXIT")

	show_menu(id, keys, menuBody, -1, "Client Cmds Menu")
}

public cmdClcmdMenu(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
		return PLUGIN_HANDLED

	new flags = get_user_flags(id)

	g_menuSelectNum[id] = 0

	for (new a = 0; a < g_clcmdNum; ++a)
		if (g_clcmdMisc[a][0] & flags)
			g_menuSelect[id][g_menuSelectNum[id]++] = a

	g_menuOption[id] = 0

	displayClcmdMenu(id, g_menuPosition[id] = 0)

	return PLUGIN_HANDLED
}

load_settings(szFilename[])
{
	if (!file_exists(szFilename))
		return 0

	new text[256], szFlags[32], szAccess[32]
	new a, pos = 0

	while (g_clcmdNum < MAX_CLCMDS && read_file(szFilename, pos++, text, 255, a))
	{
		if (text[0] == ';') continue

		if (parse(text, g_clcmdName[g_clcmdNum], 31, g_clcmdCmd[g_clcmdNum], 63, szFlags, 31, szAccess, 31) > 3)
		{
			while (replace(g_clcmdCmd[g_clcmdNum], 63, "\'", "^""))
			{
				// do nothing
			}

			g_clcmdMisc[g_clcmdNum][1] = read_flags(szFlags)
			g_clcmdMisc[g_clcmdNum][0] = read_flags(szAccess)
			g_clcmdNum++
		}
	}

	return 1
}
