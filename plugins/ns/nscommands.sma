/* AMX Mod X
*   Natural-Selection Admin Commands Plugin
*
* by the AMX Mod X Development Team
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

#pragma tabsize 0

new g_TeamOneAck[12];
new g_TeamTwoAck[12];
new g_ReadyRoomAck[12];
new g_AutoAssignAck[12];
new g_StopCommAck[12];


enum {
  PLAYERCLASS_NONE = 0,
  PLAYERCLASS_ALIVE_MARINE,
  PLAYERCLASS_ALIVE_JET_MARINE,
  PLAYERCLASS_ALIVE_HEAVY_MARINE,
  PLAYERCLASS_ALIVE_LEVEL1,
  PLAYERCLASS_ALIVE_LEVEL2,
  PLAYERCLASS_ALIVE_LEVEL3,
  PLAYERCLASS_ALIVE_LEVEL4,
  PLAYERCLASS_ALIVE_LEVEL5,
  PLAYERCLASS_ALIVE_DIGESTING,
  PLAYERCLASS_ALIVE_GESTATING,
  PLAYERCLASS_DEAD_MARINE,
  PLAYERCLASS_DEAD_ALIEN,
  PLAYERCLASS_COMMANDER,
  PLAYERCLASS_REINFORCING,
  PLAYERCLASS_SPECTATOR
};
enum {
  TEAM_UNKNOWN,
  TEAM_MARINE1,
  TEAM_ALIEN1,
  TEAM_MARINE2,
  TEAM_ALIEN2,
  TEAM_SPECTATOR
};


new g_Class[33]; // stored info from the "ScoreInfo" message
new g_Team[33];

new g_ScoreInfo_Class;
new g_ScoreInfo_Team;

public plugin_init() {
  register_plugin("NS Commands",AMXX_VERSION_STR,"AMXX Dev Team");
  // create our semi-random acknowledgement commands
  format(g_TeamOneAck,11,"namx_a%c%c%c%c%c",random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'));
  format(g_TeamTwoAck,11,"namx_b%c%c%c%c%c",random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'));
  format(g_ReadyRoomAck,11,"namx_c%c%c%c%c%c",random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'));
  format(g_AutoAssignAck,11,"namx_d%c%c%c%c%c",random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'));
  format(g_StopCommAck,11,"namx_e%c%c%c%c%c",random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'));
  
  // register them..
  register_clcmd(g_TeamOneAck,"ack_team_one");
  register_clcmd(g_TeamTwoAck,"ack_team_two");
  register_clcmd(g_ReadyRoomAck,"ack_ready_room");
  register_clcmd(g_AutoAssignAck,"ack_auto_assign");
  register_clcmd(g_StopCommAck,"ack_stop_comm");
  
  // register admin commands..
  register_concmd("amx_alien","cmdTeamTwo",ADMIN_LEVEL_H,"<name or #userid>");
  register_concmd("amx_marine","cmdTeamOne",ADMIN_LEVEL_H,"<name or #userid>");
  register_concmd("amx_uncomm","cmdUnComm",ADMIN_LEVEL_H," - force commander out of chair");
  register_concmd("amx_random","cmdRandom",ADMIN_LEVEL_H,"<name or #userid> - omit to do all in rr");
  register_concmd("amx_readyroom","cmdReadyRoom",ADMIN_LEVEL_H,"<name or #userid> - omit to do everybody");

  // clear class info..
  new i=0;
  while (i<33) {
    g_Class[i]=0;
    g_Team[i]=-1;
    i++;
  }
  
  if (cvar_exists("sv_structurelimit"))
  {
    // ns 3.2 beta
    g_ScoreInfo_Class=6;
    g_ScoreInfo_Team=8;
  }
  else
  {
    // ns 3.1
    g_ScoreInfo_Class=5;
    g_ScoreInfo_Team=7;
  }
  
  // register ScoreInfo message..
  register_event("ScoreInfo","msgScoreInfo","a")
}
public msgScoreInfo() {
  new id=read_data(1);
  if (id>32||id<1) {
    // just incase..
    return;
  }
  g_Class[id]=read_data(g_ScoreInfo_Class);
  g_Team[id]=read_data(g_ScoreInfo_Team);
}
public client_disconnect(id) {
  g_Class[id]=0;
  g_Team[id]=-1;
}
public client_connect(id) {
  g_Class[id]=0;
  g_Team[id]=-1;
}
stock UTIL_FindCommander() {
  new i=1;
  while (i<32) {
    if (g_Class[i]==PLAYERCLASS_COMMANDER) // this player is comm..
      return i;
    i++;
  }
  return 0;

}
stock UTIL_IsSpectator(id) {
  if (id<1||id>get_maxplayers())
    return -1;
  if (g_Class[id]==PLAYERCLASS_SPECTATOR)
    return 1;
  return 0;
}
// syntax: <optional: name/steamid>
public cmdRandom(id,level,cid) {
  if (!cmd_access(id,level,cid,1))
    return PLUGIN_HANDLED
  if (read_argc()>1) { // person is specified..
    new arg[32]
    read_argv(1,arg,31)
    new player = cmd_target(id,arg,CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF)
    if (!player) return PLUGIN_HANDLED
    new name[32],name_targ[32];
    new auth[32],auth_targ[32];
    get_user_name(id,name,31);
    get_user_name(player,name_targ,31);
    get_user_authid(id,auth,31);
    get_user_authid(player,auth_targ,31);
    log_amx("Cmd: ^"%s<%d><%s><>^" random ^"%s<%d><%s><>^"",name,get_user_userid(id),auth,  name_targ,get_user_userid(player),auth_targ);
	
	show_activity(id, name, "random %s", name_targ);

    client_cmd(player, "%s", g_AutoAssignAck);
  }
  else {
    new cur=0;
    new i=1;
    while (i<get_maxplayers()) {
      if (is_user_connected(i)) {
        if (!(get_user_flags(i) & ADMIN_IMMUNITY)) {
          if (g_Team[i] == 0) {
            cur++;
          }
        }
      }
      i++;
    }
    if (cur) {
      new name[32],auth[32];
      get_user_name(id,name,31);
      get_user_authid(id,auth,31);
      log_amx("Cmd: ^"%s<%d><%s><>^" random all",name,get_user_userid(id),auth);
	  
	  show_activity(id, name, "random all");

      randomStep(1);
    } else {
      client_print(id,print_chat,"[AMXX] There is nobody in the readyroom.");
    }
  }
  return PLUGIN_HANDLED_MAIN;
  
}

public randomStep(index) {
	new end = index+5
	while (index < end) {
    if (g_Team[index] == 0 && !(get_user_flags(index) & ADMIN_IMMUNITY)) {
      client_cmd(index, "%s", g_AutoAssignAck);
    }
    if (++index > get_maxplayers()) {
    	return PLUGIN_HANDLED_MAIN
    }
	}
	set_task(0.1, "randomStep", index)
	return PLUGIN_HANDLED_MAIN
}


// syntax: <optional: name/steamid>
public cmdReadyRoom(id,level,cid) {
  if (!cmd_access(id,level,cid,1))
    return PLUGIN_HANDLED
  if (read_argc()>1) { // person is specified..
    new arg[32]
    read_argv(1,arg,31)
    new player = cmd_target(id,arg,CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF)
    if (!player) return PLUGIN_HANDLED
    new name[32],name_targ[32];
    new auth[32],auth_targ[32];
    get_user_name(id,name,31);
    get_user_name(player,name_targ,31);
    get_user_authid(id,auth,31);
    get_user_authid(player,auth_targ,31);
    log_amx("Cmd: ^"%s<%d><%s><>^" ready room ^"%s<%d><%s><>^"",name,get_user_userid(id),auth,  name_targ,get_user_userid(player),auth_targ);
	
	show_activity(id, name, "ready room %s", name_targ);

    client_cmd(player, "%s", g_ReadyRoomAck);
  }
  else {
    new cur=0;
    new i=1;
    while (i<get_maxplayers()) {
      if (is_user_connected(i)) {
        if (UTIL_IsSpectator(i) == 1 || g_Team[i] != 0) {
          cur++;
        }
      }
      i++;
    }
    if (cur) {
      new name[32],auth[32];
      get_user_name(id,name,31);
      get_user_authid(id,auth,31);
      log_amx("Cmd: ^"%s<%d><%s><>^" ready room all",name,get_user_userid(id),auth);
	  
	  show_activity(id, name, "ready room all");
	  
      rrStep(1);
    } else {
      client_print(id,print_chat,"[AMXX] There is nobody on a team.");
    }
  }
  return PLUGIN_HANDLED_MAIN;
  
}

public rrStep(index) {
	new end = index+5
	while (index < end) {
    if (is_user_connected(index) && g_Team[index]!=0) {
      client_cmd(index, "%s", g_ReadyRoomAck)
    }
    if (++index > get_maxplayers()) {
    	return PLUGIN_HANDLED_MAIN
    }
	}
	set_task(0.1, "rrStep", index)
	return PLUGIN_HANDLED_MAIN
}


public cmdTeamTwo(id,level,cid) {
  if (!cmd_access(id,level,cid,2))
    return PLUGIN_HANDLED
  if (read_argc()>1) { // person is specified..
    new arg[32]
    read_argv(1,arg,31)
    new player = cmd_target(id,arg,CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF)
    if (!player) return PLUGIN_HANDLED
    if (g_Team[player]==2 || g_Team[player]==4 /*i think 4 is team 2 in ava..*/) {
      client_print(id,print_chat,"[AMXX] That user is already on team two.");
      return PLUGIN_HANDLED_MAIN;
    }
    new name[32],name_targ[32];
    new auth[32],auth_targ[32];
    get_user_name(id,name,31);
    get_user_name(player,name_targ,31);
    get_user_authid(id,auth,31);
    get_user_authid(player,auth_targ,31);
    log_amx("Cmd: ^"%s<%d><%s><>^" alien ^"%s<%d><%s><>^"",name,get_user_userid(id),auth,  name_targ,get_user_userid(player),auth_targ);
	
	show_activity(id, name, "alien %s", name_targ);

    client_cmd(player, "%s", g_TeamTwoAck);
  }
  return PLUGIN_HANDLED_MAIN;
  
}

public cmdTeamOne(id,level,cid) {
  if (!cmd_access(id,level,cid,2))
    return PLUGIN_HANDLED
  if (read_argc()>1) { // person is specified..
    new arg[32]
    read_argv(1,arg,31)
    new player = cmd_target(id,arg,CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF)
    if (!player) return PLUGIN_HANDLED
    if (g_Team[player]==1 || g_Team[player]==3 /*i think 3 is team 2 in mvm..*/) {
      client_print(id,print_chat,"[AMXX] That user is already on team one.");
      return PLUGIN_HANDLED_MAIN;
    }
    new name[32],name_targ[32];
    new auth[32],auth_targ[32];
    get_user_name(id,name,31);
    get_user_name(player,name_targ,31);
    get_user_authid(id,auth,31);
    get_user_authid(player,auth_targ,31);
    log_amx("Cmd: ^"%s<%d><%s><>^" marine ^"%s<%d><%s><>^"",name,get_user_userid(id),auth,  name_targ,get_user_userid(player),auth_targ);
	
	show_activity(id, name, "marine %s", name_targ);

    client_cmd(player, "%s", g_TeamOneAck);
  }
  return PLUGIN_HANDLED_MAIN;
  
}


// syntax: none - auto detect comm
public cmdUnComm(id,level,cid) {
  if (!cmd_access(id,level,cid,1))
    return PLUGIN_HANDLED
  new comm = UTIL_FindCommander();
  if (comm>0&&comm<=get_maxplayers()) {
    client_cmd(comm, "%s", g_StopCommAck);
    new name[32],name_targ[32];
    new auth[32],auth_targ[32];
    get_user_name(id,name,31);
    get_user_name(comm,name_targ,31);
    get_user_authid(id,auth,31);
    get_user_authid(comm,auth_targ,31);
    log_amx("Cmd: ^"%s<%d><%s><>^" uncomm ^"%s<%d><%s><>^"",name,get_user_userid(id),auth,  name_targ,get_user_userid(comm),auth_targ);

	show_activity(id, name, "uncomm %s", name_targ);

  } else {
    client_print(id,print_chat,"[AMXX] There is no commander to eject.");
  }
  return PLUGIN_HANDLED_MAIN;
}
public ack_team_one(id) {
  engclient_cmd(id,"readyroom");
  engclient_cmd(id,"readyroom");
  engclient_cmd(id,"jointeamone");
  return PLUGIN_HANDLED_MAIN;
}
public ack_team_two(id) {
  engclient_cmd(id,"readyroom");
  engclient_cmd(id,"readyroom");
  engclient_cmd(id,"jointeamtwo");
  return PLUGIN_HANDLED_MAIN;
}
public ack_ready_room(id) {
  engclient_cmd(id,"readyroom");
  engclient_cmd(id,"readyroom");
  return PLUGIN_HANDLED_MAIN;
}
public ack_auto_assign(id) {
  engclient_cmd(id,"readyroom");
  engclient_cmd(id,"readyroom");
  engclient_cmd(id,"autoassign");
  return PLUGIN_HANDLED_MAIN;
}
public ack_stop_comm(id) {
  engclient_cmd(id,"stopcommandermode");
  return PLUGIN_HANDLED_MAIN;
}
