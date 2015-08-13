// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Admin Commands Plugin
//

#include <amxmodx>
#include <amxmisc>

#pragma tabsize 4

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

new g_Class[MAX_PLAYERS + 1] = {0, ...}; // stored info from the "ScoreInfo" message
new g_Team[MAX_PLAYERS + 1] = {1, ...};

new g_ScoreInfo_Class;
new g_ScoreInfo_Team;

public plugin_init() {
  register_plugin("NS Commands",AMXX_VERSION_STR,"AMXX Dev Team");
  // create our semi-random acknowledgement commands
  format(g_TeamOneAck,charsmax(g_TeamOneAck),"namx_a%c%c%c%c%c",random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'));
  format(g_TeamTwoAck,charsmax(g_TeamTwoAck),"namx_b%c%c%c%c%c",random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'));
  format(g_ReadyRoomAck,charsmax(g_ReadyRoomAck),"namx_c%c%c%c%c%c",random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'));
  format(g_AutoAssignAck,charsmax(g_AutoAssignAck),"namx_d%c%c%c%c%c",random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'));
  format(g_StopCommAck,charsmax(g_StopCommAck),"namx_e%c%c%c%c%c",random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'),random_num('a','z'));
  
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
  g_Class[id]=read_data(g_ScoreInfo_Class);
  g_Team[id]=read_data(g_ScoreInfo_Team);
}
public client_disconnected(id) {
  g_Class[id]=0;
  g_Team[id]=-1;
}
public client_connect(id) {
  g_Class[id]=0;
  g_Team[id]=-1;
}
stock UTIL_FindCommander() {
  new i=1;
  while (i<=MAX_PLAYERS) {
    if (g_Class[i]==PLAYERCLASS_COMMANDER) // this player is comm..
      return i;
    i++;
  }
  return 0;

}
stock UTIL_IsSpectator(id) {
  if (id<1||id>MaxClients)
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
    read_argv(1,arg,charsmax(arg))
    new player = cmd_target(id,arg,CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF)
    if (!player) return PLUGIN_HANDLED
    new name[MAX_NAME_LENGTH],name_targ[MAX_NAME_LENGTH];
    new auth[32],auth_targ[32];
    get_user_name(id,name,charsmax(name));
    get_user_name(player,name_targ,charsmax(name_targ));
    get_user_authid(id,auth,charsmax(auth));
    get_user_authid(player,auth_targ,charsmax(auth_targ));
    log_amx("Cmd: ^"%s<%d><%s><>^" random ^"%s<%d><%s><>^"",name,get_user_userid(id),auth,  name_targ,get_user_userid(player),auth_targ);
	
	show_activity(id, name, "random %s", name_targ);

    client_cmd(player, "%s", g_AutoAssignAck);
  }
  else {
    new cur=0;
    new i=1;
    while (i<MaxClients) {
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
      new name[MAX_NAME_LENGTH],auth[32];
      get_user_name(id,name,charsmax(name));
      get_user_authid(id,auth,charsmax(auth));
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
    if (++index >= MaxClients) {
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
    read_argv(1,arg,charsmax(arg))
    new player = cmd_target(id,arg,CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF)
    if (!player) return PLUGIN_HANDLED
    new name[MAX_NAME_LENGTH],name_targ[MAX_NAME_LENGTH];
    new auth[32],auth_targ[32];
    get_user_name(id,name,charsmax(name));
    get_user_name(player,name_targ,charsmax(name_targ));
    get_user_authid(id,auth,charsmax(auth));
    get_user_authid(player,auth_targ,charsmax(auth_targ));
    log_amx("Cmd: ^"%s<%d><%s><>^" ready room ^"%s<%d><%s><>^"",name,get_user_userid(id),auth,  name_targ,get_user_userid(player),auth_targ);
	
	show_activity(id, name, "ready room %s", name_targ);

    client_cmd(player, "%s", g_ReadyRoomAck);
  }
  else {
    new cur=0;
    new i=1;
    while (i<=MaxClients) {
      if (is_user_connected(i)) {
        if (UTIL_IsSpectator(i) == 1 || g_Team[i] != 0) {
          cur++;
        }
      }
      i++;
    }
    if (cur) {
      new name[MAX_NAME_LENGTH],auth[32];
      get_user_name(id,name,charsmax(name));
      get_user_authid(id,auth,charsmax(auth));
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
    if (++index >= MaxClients) {
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
    read_argv(1,arg,charsmax(arg))
    new player = cmd_target(id,arg,CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF)
    if (!player) return PLUGIN_HANDLED
    if (g_Team[player]==2 || g_Team[player]==4 /*i think 4 is team 2 in ava..*/) {
      client_print(id,print_chat,"[AMXX] That user is already on team two.");
      return PLUGIN_HANDLED_MAIN;
    }
    new name[MAX_NAME_LENGTH],name_targ[MAX_NAME_LENGTH];
    new auth[32],auth_targ[32];
    get_user_name(id,name,charsmax(name));
    get_user_name(player,name_targ,charsmax(name_targ));
    get_user_authid(id,auth,charsmax(auth));
    get_user_authid(player,auth_targ,charsmax(auth_targ));
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
    read_argv(1,arg,charsmax(arg))
    new player = cmd_target(id,arg,CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF)
    if (!player) return PLUGIN_HANDLED
    if (g_Team[player]==1 || g_Team[player]==3 /*i think 3 is team 2 in mvm..*/) {
      client_print(id,print_chat,"[AMXX] That user is already on team one.");
      return PLUGIN_HANDLED_MAIN;
    }
    new name[MAX_NAME_LENGTH],name_targ[MAX_NAME_LENGTH];
    new auth[32],auth_targ[32];
    get_user_name(id,name,charsmax(name));
    get_user_name(player,name_targ,charsmax(name_targ));
    get_user_authid(id,auth,charsmax(auth));
    get_user_authid(player,auth_targ,charsmax(auth_targ));
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
  if (comm>0&&comm<=MaxClients) {
    client_cmd(comm, "%s", g_StopCommAck);
    new name[MAX_NAME_LENGTH],name_targ[MAX_NAME_LENGTH];
    new auth[32],auth_targ[32];
    get_user_name(id,name,charsmax(name));
    get_user_name(comm,name_targ,charsmax(name_targ));
    get_user_authid(id,auth,charsmax(auth));
    get_user_authid(comm,auth_targ,charsmax(auth_targ));
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
