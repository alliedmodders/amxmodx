/* AMX Mod X
*   CS Stats Plugin
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

// You can also manualy enable these options by setting them to 1
// For example:
// public ShowAttackers = 1
// However amx_statscfg command is recommended

public ShowAttackers      // shows attackers
public ShowVictims        // shows victims
public ShowKiller         // shows killer
public EndPlayer          // displays player stats at the end of map
public EndTop15           // displays top15 at the end of map
public KillerHpAp         // displays killer hp&ap to victim console and screen
public SpecRankInfo       // displays rank info when spectating

public SayHP              // displays information about user killer
public SayStatsAll        // displays players stats and rank
public SayTop15           // displays first 15. players
public SayRank            // displays user position in rank
public SayStatsMe         // displays user stats

public EndTeamScore       // displays at the end of round team score
public EndMostKills       // displays at the end of who made most kills
public EndMostDamage      // displays at the end of who made most damage

new g_Killers[33][4]
new g_Buffer[2048]
new g_userPosition[33]
new g_userState[33]
new g_userPlayers[33][32]
new g_bodyParts[8][] = {"whole body","head","chest","stomach","left arm","right arm","left leg","right leg"}
new bool:g_specMode[33]
new g_teamScore[2]

new g_disabledMsg[] = "Server has disabled that option"

public plugin_init() {
  register_plugin("CS Stats","0.1","AMXX Dev Team")
  register_event("CS_DeathMsg","eCSDeathMsg","a")
  register_event("ResetHUD","eResetHud","b")
  register_event("SendAudio","eRoundEnd","a","2=%!MRAD_terwin","2=%!MRAD_ctwin","2=%!MRAD_rounddraw") 
  register_event("30","eInterMission","a")
  register_clcmd("say /hp","cmdKiller",0,"- displays info. about your killer")
  register_clcmd("say /statsme","cmdStatsMe",0,"- displays your stats")
  register_clcmd("say /stats","cmdStats",0,"- displays others stats")
  register_clcmd("say /top15","cmdTop15",0,"- displays top 15 players")
  register_clcmd("say /rank","cmdRank",0,"- displays your server stats")
  register_menucmd(register_menuid("Server Stats"),1023,"actionStatsMenu")
  register_event("TextMsg","setSpecMode","bd","2&ec_Mod") 
  register_event("StatusValue","showRank","bd","1=2")
  register_event( "TeamScore", "eTeamScore", "a" )
}

public plugin_cfg(){
  new g_addStast[] = "amx_statscfg add ^"%s^" %s"
  server_cmd(g_addStast,"Show Attackers","ShowAttackers") 
  server_cmd(g_addStast,"Show Victims","ShowVictims") 
  server_cmd(g_addStast,"Show killer","ShowKiller")
  server_cmd(g_addStast,"Stats at the end of map","EndPlayer")  
  server_cmd(g_addStast,"Top15 at the end of map","EndTop15")  
  server_cmd(g_addStast,"Show killer hp&ap","KillerHpAp")
  server_cmd(g_addStast,"Say /hp","SayHP")  
  server_cmd(g_addStast,"Say /stats","SayStatsAll") 
  server_cmd(g_addStast,"Say /top15","SayTop15")
  server_cmd(g_addStast,"Say /rank","SayRank") 
  server_cmd(g_addStast,"Say /statsme","SayStatsMe")
  server_cmd(g_addStast,"Spec. Rank Info","SpecRankInfo")
  server_cmd(g_addStast,"Team Score","EndTeamScore")
  server_cmd(g_addStast,"Most Kills","EndMostKills")
  server_cmd(g_addStast,"Most Damage","EndMostDamage")  
}

public eTeamScore(){ 
  new team[2]
  read_data( 1, team, 1 )
  g_teamScore[ (team[0]=='C') ? 1 : 0 ] = read_data(2)
}

public setSpecMode(id) { 
  new arg[12] 
  read_data( 2 , arg , 11 ) 
  g_specMode[ id ] = ( arg[10] == '2' )
} 

public showRank(id)
  if ( SpecRankInfo && g_specMode[id] ){
    new a = read_data(2)
    if ( is_user_connected( a ) ){
      new name[32], data[8]
      get_user_name( a ,name,31)
      new pos = get_user_stats( a ,data,data)
      set_hudmessage(255,255,255,0.02,0.85,2, 0.05, 0.1, 0.01, 3.0, 1)
      show_hudmessage(id,"%s's rank is %d of %d",name,pos,get_statsnum())
    }
  }

/* build list of attackers */
getAttackers(id) {
  new name[32],wpn[32], stats[8],body[8],found=0
  new pos = copy(g_Buffer,2047,"Attackers:^n")
  new amax = get_maxplayers()
  for(new a = 1; a <= amax; ++a){
    if(get_user_astats(id,a,stats,body,wpn,31)){
      found = 1
      if (stats[0])
        format(wpn,31," -- %s",wpn)
      else
        wpn[0] = 0
      get_user_name(a,name,31)
      pos += format(g_Buffer[pos],2047-pos,"%s --  %d hit%s / %d dmg %s^n",name,stats[5],(stats[5]==1)?"":"s",stats[6],wpn)
    }
  }
  return found
}

/* build list of victims */
getVictims(id) {
  new name[32],wpn[32], stats[8],body[8],found=0
  new pos = copy(g_Buffer,2047,"Victims:^n")
  new amax = get_maxplayers()
  for(new a = 1; a <= amax; ++a){
    if(get_user_vstats(id,a,stats,body,wpn,31)){
      found = 1
      if (stats[1])
        format(wpn,31," -- %s",wpn)
      else
        wpn[0] = 0
      get_user_name(a,name,31)
      pos += format(g_Buffer[pos],2047-pos,"%s -- %d hit%s / %d dmg %s^n",name,stats[5],(stats[5]==1)?"":"s",stats[6],wpn)
    }
  }
  return found
}

/* build list of hita for AV List */
getHits(id,killer) {
  new stats[8], body[8], pos = 0
  g_Buffer[0] = 0
  get_user_astats(id,killer,stats,body)
  for(new a = 1; a < 8; ++a)
    if(body[a])
      pos += format(g_Buffer[pos],2047-pos,"%s: %d^n",g_bodyParts[a],body[a])
}

/* get top 15 */
getTop15(){
  new pos=0, stats[8], body[8], name[32]
#if !defined NO_STEAM
  pos = format(g_Buffer,2047,"<html><head><style type=^"text/css^">pre{color:#FFB000;}body{background:#000000;margin-left:8px;margin-top:0px;}</style></head><pre><body>")
#endif
  pos += format(g_Buffer[pos],2047-pos," #  %-28.27s %6s %6s %6s %6s %6s^n",
  	"nick", "kills" , "deaths" , "hits","shots","hs" )
  new imax = get_statsnum()
  if (imax > 15) imax = 15
  for(new a = 0; a < imax; ++a){
    get_stats(a,stats,body,name,31)
    pos += format(g_Buffer[pos],2047-pos,"%2d. %-28.27s %6d %6d %6d %6d %6d^n",a+1,name,stats[0],stats[1],stats[5],stats[4],stats[2])
  }
#if !defined NO_STEAM
  format(g_Buffer[pos],2047-pos,"</pre></body></html>")
#endif
}

/* build list of hits for say hp */
getMyHits(id,killed) {
  new name[32], stats[8], body[8]
  get_user_name(killed,name,31)
  new pos = format(g_Buffer,2047,"You hit %s in:",name)
  get_user_vstats(id,killed,stats,body)
  for(new a = 1; a < 8; ++a){
    if(body[a])
      pos += format(g_Buffer[pos],2047-pos," %s: %d ",g_bodyParts[a],body[a])
  }
}

/* save hits and damage */
public eCSDeathMsg() {
    new killer = read_data(1)
    new victim = read_data(2)
    if ( killer == victim ) return
    new vorigin[3], korigin[3]
    get_user_origin(victim,vorigin)
    get_user_origin(killer,korigin)
    g_Killers[victim][0] = killer
    g_Killers[victim][1] = get_user_health(killer)
    g_Killers[victim][2] = get_user_armor(killer)      
    g_Killers[victim][3] = get_distance(vorigin,korigin)
    if ( ShowKiller ){
      new name[32], stats[8], body[8], wpn[33], mstats[8], mbody[8]
      get_user_name(killer,name,31)
      get_user_astats(victim,killer,stats,body,wpn,31)
      if ( !get_user_vstats(victim,killer,mstats,mbody) )
        mstats[5] = mstats[6] = 0
      set_hudmessage(220,80,0,0.05,0.15,0, 6.0, 12.0, 1.0, 2.0, 1)
      getHits(victim,killer)
      show_hudmessage(victim,"%s killed you with %s^nfrom distance of %.2f meters.^nHe did %d damage to you with %d hit%s^nand still has %dhp and %dap.^nYou did %d damage to him with %d hit%s.^nHe hits you in:^n%s",
          name,wpn,float(g_Killers[victim][3]) * 0.0254,  stats[6],stats[5], (stats[5]==1) ? "":"s",   g_Killers[victim][1],g_Killers[victim][2],
            mstats[6],mstats[5],(mstats[5]==1) ? "" : "s",g_Buffer )
    }
    if ( ShowVictims && getVictims(victim) ){
      set_hudmessage(0,80,220,0.55,0.60,0, 6.0, 12.0, 1.0, 2.0, 4)
      show_hudmessage(victim,g_Buffer)
    }
    if ( ShowAttackers  && getAttackers(victim)){
      set_hudmessage(220,80,0,0.55,0.35,0, 6.0, 12.0, 1.0, 2.0, 3)
      show_hudmessage(victim,g_Buffer)
    }
    if (  KillerHpAp ){
      new name[32], kmsg[128]
      get_user_name(killer,name,31)
      format(kmsg,127,"%s still has %dhp and %dap",name,g_Killers[victim][1],g_Killers[victim][2])
      client_print(victim,print_console,kmsg)
      set_hudmessage(255,255,255,0.02,0.85,2, 1.5, 3.0, 0.02, 5.0, 1)
      show_hudmessage(victim,kmsg)
    }
}

public eResetHud( id )
  g_Killers[ id ][0] = 0

public eRoundEnd()
  set_task( 0.3 , "eRoundEndTask" )

public eRoundEndTask() {
  if (  ShowVictims || ShowAttackers ) {
    new players[32], pnum
    get_players( players , pnum, "a"  ) 
    for(new i = 0; i < pnum; ++i ) {
      if ( ShowVictims &&getVictims( players[ i ] )){
        set_hudmessage(0,80,220,0.55,0.60,0, 6.0, 12.0, 1.0, 2.0, 4)
        show_hudmessage( players[ i ] ,g_Buffer)
      }
      if ( ShowAttackers && getAttackers( players[ i ] ) ){
        set_hudmessage(220,80,0,0.55,0.35,0, 6.0, 12.0, 1.0, 2.0, 3)
        show_hudmessage( players[ i ] ,g_Buffer)
      }
    }
  }
  if ( EndMostKills || EndTeamScore || EndMostDamage  ){
    new players[32], pnum, stats[8],bodyhits[8], len = 0
    get_players( players , pnum ) 
    g_Buffer[0] = 0
    if ( EndMostKills ){
      new kills = 0, who = 0, hs = 0
      for(new i = 0; i < pnum; ++i){
        get_user_rstats( players[i],stats, bodyhits )
        if ( stats[0] > kills ){
          who = players[i]
          kills = stats[0]
          hs = stats[2]
        }  
      }
      if ( is_user_connected(who) ) {
        new name[32]
        get_user_name( who, name, 31 )
        len += format(g_Buffer[len] , 512 - len ,
          "Most kills: %s^n%d kill%s / %d headshot%s^n", name , kills , (kills == 1) ? "": "s"  , 
            hs , (hs == 1) ? "": "s" )
      }
    }
    if ( EndMostDamage ){
      new damage = 0, who = 0, hits = 0
      for(new i = 0; i < pnum; ++i){
        get_user_rstats( players[i],stats, bodyhits )
        if ( stats[6] > damage ){
          who = players[i]
          hits = stats[5]
          damage = stats[6]
        }  
      }
      if ( is_user_connected(who) ) {
        new name[32]
        get_user_name( who, name, 31 )
        len += format(g_Buffer[len] , 512 - len ,
          "Most damage: %s^n%d damage / %d hit%s^n", name , damage , hits, (hits == 1) ? "": "s" )
      }
    }
    if ( EndTeamScore )
      format(g_Buffer[len] , 512 - len , "TERRORISTs %d -- %d CTs^n", g_teamScore[0] , g_teamScore[1] )
    set_hudmessage(100,200,0,0.02,0.65,2, 0.01, 5.0, 0.01, 0.01, 2 )
    show_hudmessage( 0 , g_Buffer )
  }
}

public cmdKiller(id) {
  if ( !SayHP ){
    client_print(id,print_chat, g_disabledMsg )
    return PLUGIN_HANDLED
  }
  if (g_Killers[id][0]) {
    new name[32], stats[8], body[8], wpn[33], mstats[8], mbody[8]
    get_user_name(g_Killers[id][0],name,31)
    get_user_astats(id,g_Killers[id][0],stats,body,wpn,31)
    client_print(id,print_chat,"%s killed you with %s from distance of %.2f meters",  name,wpn,float(g_Killers[id][3]) * 0.0254 )
    client_print(id,print_chat,"He did %d damage to you with %d hit%s and still had %dhp and %dap",
      stats[6],stats[5],(stats[5]==1)?"":"s" , g_Killers[id][1],g_Killers[id][2] )
    if ( get_user_vstats(id,g_Killers[id][0],mstats,mbody) )  {
      client_print(id,print_chat,"You did %d damage to him with %d hit%s",mstats[6], mstats[5],(mstats[5]==1)?"":"s" )
      getMyHits(id,g_Killers[id][0])
      client_print(id,print_chat,g_Buffer)
    }
    else client_print(id,print_chat,"You did no damage to him")
  }
  else {
    client_print(id,print_chat,"You have no killer...")
  }
  return PLUGIN_CONTINUE
}

public cmdStatsMe(id){
  if ( !SayStatsMe ){
    client_print(id,print_chat, g_disabledMsg )
    return PLUGIN_HANDLED
  }
  displayStats(id,id)
  return PLUGIN_CONTINUE
}

public displayStats(id,dest) {
  new pos=0, name[32], stats[8], body[8]
  get_user_wstats(id,0,stats,body)
#if !defined NO_STEAM
  pos = format(g_Buffer,2047,"<html><head><style type=^"text/css^">pre{color:#FFB000;}body{background:#000000;margin-left:8px;margin-top:0px;}</style></head><pre><body>")
#endif
  pos += format(g_Buffer[pos],2047-pos,"%6s: %d^n%6s: %d^n%6s: %d^n%6s: %d^n%6s: %d^n^n",
    "Kills",stats[0],"Deaths",stats[1],"Damage",stats[6],"Hits",stats[5],"Shots",stats[4])
  pos += format(g_Buffer[pos],2047-pos, "%-12.11s  %6s  %6s  %6s  %6s  %6s^n",
  	"weapon","shots","hits","damage","kills","deaths")
  for(new a = 1; a < 31; ++a) {
    if (get_user_wstats(id,a,stats,body)){
      get_weaponname(a,name,31)
      pos += format(g_Buffer[pos],2047-pos,"%-12.11s  %6d  %6d  %6d  %6d  %6d^n",
        name[7],stats[4],stats[5],stats[6],stats[0],stats[1])
    }
  }
  get_user_name(id,name,31)
#if !defined NO_STEAM
  format(g_Buffer[pos],2047-pos,"</pre></body></html>")
#endif
  show_motd(dest,g_Buffer,name)
  return PLUGIN_CONTINUE
}

public cmdRank(id){
  if ( !SayRank ){
    client_print(id,print_chat, g_disabledMsg )
    return PLUGIN_HANDLED
  }
  displayRank(id,id)
  return PLUGIN_CONTINUE
}

displayRank(id,dest) {
  new pos=0, name[32], stats[8], body[8]
  new rank_pos = get_user_stats(id,stats,body)
#if !defined NO_STEAM
  pos = format(g_Buffer,2047,"<html><head><style type=^"text/css^">pre{color:#FFB000;}body{background:#000000;margin-left:8px;margin-top:0px;}</style></head><pre><body>")
#endif
  pos += format(g_Buffer[pos],2047-pos,"%s rank is %d of %d^n^n",(id==dest)?"Your":"His", rank_pos,get_statsnum())
  pos += format(g_Buffer[pos],2047-pos,"%10s: %d^n%10s: %d^n%10s: %d^n%10s: %d^n%10s: %d^n^n",
    "Kills",stats[0],"Deaths",stats[1],"Damage",stats[6],"Hits",stats[5],"Shots",stats[4])
  pos += format(g_Buffer[pos],2047-pos,"%10s:^n%10s: %d^n%10s: %d^n%10s: %d^n%10s: %d^n%10s: %d^n%10s: %d^n%10s: %d",
    "Hits",g_bodyParts[1],body[1],g_bodyParts[2],body[2],g_bodyParts[3],body[3], g_bodyParts[4],body[4],
    g_bodyParts[5],body[5],g_bodyParts[6],body[6],g_bodyParts[7],body[7])
#if !defined NO_STEAM
  format(g_Buffer[pos],2047-pos,"</pre></body></html>")
#endif
  get_user_name(id,name,31)
  show_motd(dest,g_Buffer,name)
}

public cmdTop15(id) {
  if ( !SayTop15 ){
    client_print(id,print_chat, g_disabledMsg )
    return PLUGIN_HANDLED
  }
  getTop15()
  show_motd(id,g_Buffer,"Top 15")
  return PLUGIN_CONTINUE
}

public endGameStats(){
  if ( EndPlayer ){
    new players[32], inum
    get_players(players,inum)
    for(new i = 0; i < inum; ++i)
      displayStats(players[i],players[i])
  } 
  else if ( EndTop15 ) {
    new players[32], inum
    get_players(players,inum)
    getTop15()
    for(new i = 0; i < inum; ++i)
      show_motd(players[i],g_Buffer,"Top 15")
  }
}

public eInterMission()
  set_task(1.0,"endGameStats")
  
public cmdStats(id){
  if ( !SayStatsAll ){
    client_print(id,print_chat, g_disabledMsg )
    return PLUGIN_HANDLED
  }
  showStatsMenu(id,g_userPosition[id]=0)
  return PLUGIN_CONTINUE
}

public actionStatsMenu(id,key){
  switch(key){
  case 7: {
    g_userState[id] = 1 - g_userState[id]
    showStatsMenu(id,g_userPosition[id])
  }
  case 8: showStatsMenu(id,++g_userPosition[id])
  case 9: showStatsMenu(id,--g_userPosition[id])
  default:{
    new option = g_userPosition[id] * 7 + key
    new index = g_userPlayers[id][option]
    if (is_user_connected(index)){
      if (g_userState[id])
        displayRank(index,id)
      else
        displayStats(index,id)
    }
    showStatsMenu(id,g_userPosition[id])
    }
  }
  return PLUGIN_HANDLED
}

showStatsMenu(id,pos){
  if (pos < 0) return PLUGIN_HANDLED
  new menu_body[512], inum, k = 0, start = pos * 7
  get_players(g_userPlayers[id],inum)
  if (start >= inum) start = pos = g_userPosition[id] = 0
  new len = format(menu_body,511,"\yServer Stats\R%d/%d^n\w^n",pos + 1,((inum/7)+((inum%7)?1:0)))
  new name[32], end = start + 7, keys = (1<<9)|(1<<7)
  if (end > inum) end = inum
  for(new a = start; a < end; ++a){
    get_user_name(g_userPlayers[id][a],name,31)
    keys |= (1<<k)
    len += format(menu_body[len],511-len,"%d. %s^n\w",++k,name)
  }
  len += format(menu_body[len],511-len,"^n8. %s^n\w",g_userState[id] ? "Show rank" : "Show stats" )
  if (end != inum){
    format(menu_body[len],511-len,"^n9. More...^n0. %s" , pos ? "Back" : "Exit" )
    keys |= (1<<8)
  }
  else format(menu_body[len],511-len,"^n0. %s" , pos ? "Back" : "Exit" )
  show_menu(id,keys,menu_body)
  return PLUGIN_HANDLED
}
