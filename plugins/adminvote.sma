/* AMX Mod X
*   Admin Votes Plugin
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

#include <amxmod>
#include <amxmisc>

new g_Answer[128] 
new g_optionName[4][32] 
new g_voteCount[4] 
new g_validMaps
new g_yesNoVote
new g_cstrikeRunning
new g_voteCaller
new g_Execute[256]
new g_execLen
new g_alredyVoting[]= "There is already one voting..."
new g_notAllowed[] = "Voting not allowed at this time"
new g_votingStarted[] = "Voting has started..."
new g_playerTag[] = "PLAYER"
new g_adminTag[] = "ADMIN"

new bool:g_execResult
new Float:g_voteRatio 

public plugin_init() { 
  register_plugin("Admin Votes","0.1","AMXX Dev Team")
  register_menucmd(register_menuid("Change map to ") ,(1<<0)|(1<<1),"voteCount") 
  register_menucmd(register_menuid("Choose map: ") ,(1<<0)|(1<<1)|(1<<2)|(1<<3),"voteCount") 
  register_menucmd(register_menuid("Kick ") ,(1<<0)|(1<<1),"voteCount") 
  register_menucmd(register_menuid("Ban ") ,(1<<0)|(1<<1),"voteCount") 
  register_menucmd(register_menuid("Vote: ") ,(1<<0)|(1<<1),"voteCount") 
  register_menucmd(register_menuid("The result: ") ,(1<<0)|(1<<1),"actionResult")
  register_concmd("amx_votemap","cmdVoteMap",ADMIN_VOTE,"<map> [map] [map] [map]")
  register_concmd("amx_votekick","cmdVoteKickBan",ADMIN_VOTE,"<name or #userid>")
  register_concmd("amx_voteban","cmdVoteKickBan",ADMIN_VOTE,"<name or #userid>")
  register_concmd("amx_vote","cmdVote",ADMIN_VOTE,"<question> <answer#1> <answer#2>")
  register_concmd("amx_cancelvote","cmdCancelVote",ADMIN_VOTE,"- cancels last vote")
  register_cvar("amx_votekick_ratio","0.40") 
  register_cvar("amx_voteban_ratio","0.40") 
  register_cvar("amx_votemap_ratio","0.40") 
  register_cvar("amx_vote_ratio","0.02") 
  register_cvar("amx_vote_time","10") 
  register_cvar("amx_vote_answers","1") 
  register_cvar("amx_vote_delay","60")
  register_cvar("amx_last_voting","0")
  set_cvar_float("amx_last_voting",0.0)
  register_cvar("amx_show_activity","2")
  g_cstrikeRunning = is_running("cstrike")
}

public cmdCancelVote(id,level,cid){
  if (!cmd_access(id,level,cid,0))
    return PLUGIN_HANDLED
  if ( task_exists( 99889988 , 1 ) ) {
    new authid[32],name[32]
    get_user_authid(id,authid,31)
    get_user_name(id,name,31)
    log_amx("Vote: ^"%s<%d><%s><>^" cancel vote session", name,get_user_userid(id),authid)
    switch(get_cvar_num("amx_show_activity")) {
    case 2: client_print(0,print_chat,"%s %s: cancel vote", (get_user_flags(id) & ADMIN_USER) ? g_playerTag : g_adminTag, name)
    case 1: client_print(0,print_chat,"%s: cancel vote", (get_user_flags(id) & ADMIN_USER) ? g_playerTag : g_adminTag)
    }
    console_print(id, "Voting canceled" )
    client_print(0,print_chat,"Voting canceled")
    remove_task( 99889988 , 1 )
    set_cvar_float( "amx_last_voting" , get_gametime()  )    
  }
  else
    console_print(id, "There is no voting to cancel or the vote session can't be canceled with that command" ) 
  return PLUGIN_HANDLED
}
    
public delayedExec(cmd[]) 
  server_cmd(cmd)

new g_resultRef[] = "Result refused"
new g_resultAcpt[] = "Result accepted"
new g_votingFailed[] = "Voting failed"
new g_votingSuccess[] = "Voting successful"

public autoRefuse(){
  log_amx("Vote: %s" , g_resultRef)
  client_print(0,print_chat,g_resultRef )
}

public actionResult(id,key) {
  remove_task( 4545454 )
  switch(key){
    case 0: {
      set_task(2.0,"delayedExec",0,g_Execute,g_execLen)
      log_amx("Vote: %s",g_resultAcpt)
      client_print(0,print_chat,g_resultAcpt )
    }
    case 1: autoRefuse()
  }
  return PLUGIN_HANDLED
}

public checkVotes() {
  new best = 0
  if ( !g_yesNoVote ) {
    for(new a = 0; a < 4; ++a)
      if (g_voteCount[a] > g_voteCount[best])
        best = a
  }
  new votesNum = g_voteCount[0] + g_voteCount[1] + g_voteCount[2]  + g_voteCount[3]
  new iRatio = votesNum ? floatround( g_voteRatio * float( votesNum ) ,floatround_ceil) : 1
  new iResult = g_voteCount[best]
  if ( iResult < iRatio ){
    if (g_yesNoVote)
      client_print(0,print_chat,"%s (yes ^"%d^") (no ^"%d^") (needed ^"%d^")",g_votingFailed, g_voteCount[0], g_voteCount[1] , iRatio  )
    else
      client_print(0,print_chat,"%s (got ^"%d^") (needed ^"%d^")",g_votingFailed,iResult , iRatio )
    log_amx("Vote: %s (got ^"%d^") (needed ^"%d^")",g_votingFailed,iResult , iRatio )  
    return PLUGIN_CONTINUE
  }
  g_execLen = format(g_Execute,255,g_Answer,g_optionName[best]) + 1
  if (g_execResult){
    g_execResult = false
    if ( is_user_connected(g_voteCaller) )  {
      new menuBody[512]
      new len = format(menuBody,511,g_cstrikeRunning ? "\yThe result: \w%s^n^n" :  "The result: %s^n^n", g_Execute )
      len += copy( menuBody[len] ,511 - len, g_cstrikeRunning ? "\yDo you want to continue?^n\w" : "Do you want to continue?^n" )
      copy( menuBody[len] ,511 - len, "^n1. Yes^n2. No")
      show_menu( g_voteCaller ,0x03 ,menuBody, 10 )
      set_task(10.0,"autoRefuse",4545454)
    }
    else
      set_task(2.0,"delayedExec",0,g_Execute,g_execLen)
  }
  client_print(0,print_chat,"%s (got ^"%d^") (needed ^"%d^"). The result: %s", g_votingSuccess, iResult , iRatio , g_Execute ) 
  log_amx("Vote: %s (got ^"%d^") (needed ^"%d^") (result ^"%s^")", g_votingSuccess, iResult , iRatio , g_Execute )
  return PLUGIN_CONTINUE
} 

public voteCount(id,key){ 
  if ( get_cvar_num("amx_vote_answers") ) { 
    new name[32]
    get_user_name(id,name,31)
    if (g_yesNoVote)
      client_print(0,print_chat,"%s voted %s",name,key ? "against" : "for" )
    else
      client_print(0,print_chat,"%s voted for option #%d",name,key+1)
  }
  ++g_voteCount[key]
  return PLUGIN_HANDLED 
} 

public cmdVoteMap(id,level,cid) { 
  if (!cmd_access(id,level,cid,2))
    return PLUGIN_HANDLED
  new Float:voting = get_cvar_float("amx_last_voting")
  if (voting > get_gametime()){ 
    console_print(id, g_alredyVoting ) 
    return PLUGIN_HANDLED 
  } 
  if (voting && voting + get_cvar_float("amx_vote_delay") > get_gametime()) { 
    console_print(id, g_notAllowed ) 
    return PLUGIN_HANDLED 
  } 
  new argc = read_argc() 
  if (argc > 5) argc = 5
  g_validMaps = 0
  g_optionName[0][0] = 0
  g_optionName[1][0] = 0
  g_optionName[2][0] = 0
  g_optionName[3][0] = 0
  for(new i = 1; i < argc; ++i){ 
    read_argv(i,g_optionName[g_validMaps],31) 
    if (is_map_valid(g_optionName[g_validMaps])) 
      g_validMaps++; 
  } 
  if (g_validMaps == 0) { 
    console_print(id,"Given %s not valid",(argc==2)?"map is":"maps are") 
    return PLUGIN_HANDLED 
  } 
  new menu_msg[256] 
  new keys = 0  
  if (g_validMaps > 1){
    keys = (1<<9)
    copy(menu_msg,255,g_cstrikeRunning ? "\yChoose map: \w^n^n" : "Choose map: ^n^n") 
    new temp[128] 
    for(new a = 0; a < g_validMaps; ++a){ 
      format(temp,127,"%d.  %s^n",a+1,g_optionName[a]) 
      add(menu_msg,255,temp) 
      keys |= (1<<a) 
    } 
    add(menu_msg,255,"^n0.  None")
    g_yesNoVote = 0 
  }
  else{ 
    format(menu_msg,255,g_cstrikeRunning ? "\yChange map to %s?\w^n^n1.  Yes^n2.  No"
        : "Change map to %s?^n^n1.  Yes^n2.  No",g_optionName[0]) 
    keys = (1<<0)|(1<<1)
    g_yesNoVote = 1
  }
  new authid[32],name[32] 
  get_user_authid(id,authid,31) 
  get_user_name(id,name,31)
  if (argc==2)
    log_amx("Vote: ^"%s<%d><%s><>^" vote map (map ^"%s^")",
      name,get_user_userid(id),authid,g_optionName[0])
  else
    log_amx("Vote: ^"%s<%d><%s><>^" vote maps (map#1 ^"%s^") (map#2 ^"%s^") (map#3 ^"%s^") (map#4 ^"%s^")",
      name,get_user_userid(id),authid,g_optionName[0],g_optionName[1],g_optionName[2],g_optionName[3])
      
  switch(get_cvar_num("amx_show_activity")) {
  case 2: client_print(0,print_chat,"%s %s: vote map(s)",name,
      (get_user_flags(id) & ADMIN_USER) ? g_playerTag : g_adminTag)
  case 1: client_print(0,print_chat,"%s: vote map(s)",
      (get_user_flags(id) & ADMIN_USER) ? g_playerTag : g_adminTag)
  }
      
  g_execResult = true
  new Float:vote_time = get_cvar_float("amx_vote_time") + 2.0 
  set_cvar_float("amx_last_voting",  get_gametime() + vote_time )
  g_voteRatio = get_cvar_float("amx_votemap_ratio") 
  g_Answer = "changelevel %s"
  show_menu(0,keys,menu_msg,floatround(vote_time)) 
  set_task(vote_time,"checkVotes" , 99889988 )
  g_voteCaller = id
  console_print(id, g_votingStarted ) 
  g_voteCount = { 0,0, 0,0 }
  return PLUGIN_HANDLED 
} 

public cmdVote(id,level,cid) { 
  if (!cmd_access(id,level,cid,4))
    return PLUGIN_HANDLED
  new Float:voting = get_cvar_float("amx_last_voting")
  if (voting > get_gametime()){ 
    console_print(id, g_alredyVoting ) 
    return PLUGIN_HANDLED 
  } 
  if (voting && voting + get_cvar_float("amx_vote_delay") > get_gametime()) { 
    console_print(id, g_notAllowed ) 
    return PLUGIN_HANDLED 
  }
  new quest[48] 
  read_argv(1,quest,47) 
  if ((contain(quest,"sv_password")!=-1)||(contain(quest,"rcon_password")!=-1)||
    (contain(quest,"kick")!=-1)||(contain(quest,"addip")!=-1)||(contain(quest,"ban")!=-1)){ 
    console_print(id,"Voting for that has been forbidden")
    return PLUGIN_HANDLED
  } 
  read_argv(2,g_optionName[0],31) 
  read_argv(3,g_optionName[1],31) 

  new authid[32],name[32] 
  get_user_authid(id,authid,31) 
  get_user_name(id,name,31) 
  log_amx("Vote: ^"%s<%d><%s><>^" vote custom (question ^"%s^") (option#1 ^"%s^") (option#2 ^"%s^")", 
    name,get_user_userid(id),authid,quest,g_optionName[0],g_optionName[1]) 

  switch(get_cvar_num("amx_show_activity")) {
  case 2: client_print(0,print_chat,"%s %s: vote custom",
    (get_user_flags(id) & ADMIN_USER) ? g_playerTag : g_adminTag,name )
  case 1: client_print(0,print_chat,"%s: vote custom",
    (get_user_flags(id) & ADMIN_USER) ? g_playerTag : g_adminTag)
  }

  new menu_msg[256] 
  new keys = (1<<0)|(1<<1) 
  format(menu_msg,255, g_cstrikeRunning ? "\yVote: %s\w^n^n1.  %s^n2.  %s"
      : "Vote: %s^n^n1.  %s^n2.  %s",quest,g_optionName[0],g_optionName[1]) 
  g_execResult = false
  new Float:vote_time = get_cvar_float("amx_vote_time") + 2.0 
  set_cvar_float("amx_last_voting",  get_gametime() + vote_time )
  g_voteRatio = get_cvar_float("amx_vote_ratio")
  format(g_Answer,127,"%s - %%s",quest)
  show_menu(0,keys,menu_msg,floatround(vote_time)) 
  set_task(vote_time,"checkVotes" , 99889988 ) 
  g_voteCaller = id
  console_print(id, g_votingStarted ) 
  g_voteCount ={ 0,0,0,0}
  g_yesNoVote = 0 
  return PLUGIN_HANDLED 
} 

public cmdVoteKickBan(id,level,cid) { 
  if (!cmd_access(id,level,cid,2))
    return PLUGIN_HANDLED
  new Float:voting = get_cvar_float("amx_last_voting")
  if (voting > get_gametime()){ 
    console_print(id, g_alredyVoting ) 
    return PLUGIN_HANDLED 
  } 
  if (voting && voting + get_cvar_float("amx_vote_delay") > get_gametime()) { 
    console_print(id, g_notAllowed ) 
    return PLUGIN_HANDLED 
  } 
  new cmd[32] 
  read_argv(0,cmd,31)
  new voteban = equal(cmd,"amx_voteban")
  new arg[32] 
  read_argv(1,arg,31)
  new player = cmd_target(id,arg,1)
  if (!player) return PLUGIN_HANDLED
  if (voteban && is_user_bot(player))   { 
    new imname[32]
    get_user_name(player,imname,31)
    console_print(id,"That action can't be performed on bot ^"%s^"",imname)
    return PLUGIN_HANDLED
  }

  new keys = (1<<0)|(1<<1)
  new menu_msg[256]
  get_user_name(player,arg,31) 
  format(menu_msg,255,g_cstrikeRunning ? "\y%s %s?\w^n^n1.  Yes^n2.  No"
    : "%s %s?^n^n1.  Yes^n2.  No", voteban ? "Ban" : "Kick", arg)
  g_yesNoVote = 1   
  if (voteban) 
    get_user_authid(player,g_optionName[0],31) 
  else 
    numtostr(get_user_userid(player),g_optionName[0],31) 
  new authid[32],name[32]
  get_user_authid(id,authid,31) 
  get_user_name(id,name,31) 
  log_amx("Vote: ^"%s<%d><%s><>^" vote %s (target ^"%s^")", 
    name,get_user_userid(id),authid,voteban ? "ban" : "kick",arg)
  
  switch(get_cvar_num("amx_show_activity")) {
  case 2: client_print(0,print_chat,"%s %s: vote %s for %s",
    (get_user_flags(id) & ADMIN_USER) ? g_playerTag : g_adminTag,name,voteban ? "ban" : "kick",arg )
  case 1: client_print(0,print_chat,"%s: vote %s for %s",
    (get_user_flags(id) & ADMIN_USER) ? g_playerTag : g_adminTag,voteban ? "ban" : "kick",arg)
  }
  
  g_execResult = true
  new Float:vote_time = get_cvar_float("amx_vote_time") + 2.0 
  set_cvar_float("amx_last_voting",  get_gametime() + vote_time )
  g_voteRatio = get_cvar_float(voteban ? "amx_voteban_ratio" : "amx_votekick_ratio") 
  g_Answer = voteban ? "banid 30.0 %s kick" : "kick #%s"
  show_menu(0,keys,menu_msg,floatround(vote_time)) 
  set_task(vote_time,"checkVotes" , 99889988 )
  g_voteCaller = id
  console_print(id, g_votingStarted ) 
  g_voteCount = {0,0,0,0}
  return PLUGIN_HANDLED 
} 
