/* AMX Mod script.
*
* (c) 2004, SidLuke
* This file is provided as is (no warranties).
*
* Plugin works with Stats Settings Plugin. Just run both of them.
* By amx_statscfg command (from Stats Settings Plugin)
* you will be able to set all settings and save them to a file.
* 
* Example of usage for some options:
* amx_statscfg on SayTop15
* amx_statscfg on SayRank
* 
* Accept able are also parts of name:
* amx_statscfg off say
* amx_statscfg on End
*/

#include <amxmodx>
#include <amxmisc>
#include <dodx>

public EndPlayer          // displays player stats at the end of map
public EndTop15           // displays top15 at the end of map

public SayStatsAll        // displays players stats and rank
public SayTop15           // displays first 15. players ,cvar dodstats_topvalue really:)
public SayRank            // displays user position in rank
public SayStatsMe         // displays user stats

public ShowAttackers  // shows attackers 
public ShowVictims    // shows victims 
public ShowKiller     // shows killer 
public KillerHp       // displays killer hp to victim console and screen 
public SayHP          // displays information about user killer 
public SayFF          // displays friendly fire status

public MultiKill
public MultiKillSound
public KnifeKill
public KnifeKillSound
public GrenadeKill
public GrenadeSuicide
public HeadShotKill
public HeadShotKillSound
public KillingStreak
public KillingStreakSound
public DoubleKill
public DoubleKillSound
public BulletDamage
public TAInfo
public RoundScore
public EndRoundStats
public GreCatch
public GreCatchSound
public EnemyGreKill
public EnemyGreKillSound
public LeadSounds
public MortarKill

new g_streakKills[33][2]
new g_multiKills[33][2]
new Float:g_prevKill
new g_prevKillerId
new g_KillCount;
new g_RoundScore[2]

new g_userPosition[33]
new g_userState[33]
new g_userPlayers[33][32]
new g_Buffer[2048]

new g_Killers[33][3]
new Float:g_DeathStats[33]

new g_damage_sync
new g_center1_sync
new g_center2_sync
new g_left_sync

new g_bodyParts[8][] = { 
                        "WHOLEBODY",
                        "HEAD",
                        "CHEST",
                        "STOMACH",
                        "LEFTARM",
                        "RIGHTARM",
                        "LEFTLEG",
                        "RIGHTLEG"
}
new g_MultiKillMsg[7][] = { 
  "MULTI_MSG", 
  "ULTRA_MSG", 
  "SPREE_MSG",  
  "RAMPAGE_MSG" ,
  "UNSTOPPABLE_MSG" ,
  "MONSTER_MSG",
  "GODLIKE_MSG"
}
new g_Sounds[7][] = { 
  "multikill", 
  "ultrakill", 
  "killingspree", 
  "rampage",    
  "unstoppable",   
  "monsterkill",
  "godlike"  
}
new g_KillingMsg[7][] = {
  "MULTI_SMALL",
  "ULTRA_SMALL",
  "SPREE_SMALL",
  "RAMPAGE_SMALL",  
  "UNSTOPPABLE_SMALL",  
  "MONSTER_SMALL",
  "GODLIKE_SMALL"
}
new g_KnifeMsg[4][] = { 
  "KNIFE_MSG1", 
  "KNIFE_MSG2", 
  "KNIFE_MSG3", 
  "KNIFE_MSG4"
}
new g_HeMessages[4][] = { 
  "HE_MSG1",   
  "HE_MSG2",   
  "HE_MSG3",   
  "HE_MSG4"
}
new g_SHeMessages[4][] = { 
  "SHE_MSG1",   
  "SHE_MSG2",   
  "SHE_MSG3",   
  "SHE_MSG4"
}
new g_HeadShots[7][] = { 
  "HEAD_MSG1",   
  "HEAD_MSG2",   
  "HEAD_MSG3",   
  "HEAD_MSG4",
  "HEAD_MSG5",
  "HEAD_MSG6",
  "HEAD_MSG7"
}

new g_DoubleKillMsg[3][] = {
  "DOUBLE_MSG1",
  "DOUBLE_MSG2",
  "DOUBLE_MSG3"
}

new g_DoubleKillSound[3][] = {
  "doublekill",
  "multikill",
  "godlike"
}

new mortarmsg[2][]={
	"MORTAR_MSG1",
	"MORTAR_MSG2"
}

new g_addStast[] = "amx_statscfg add ^"%s^" %s"
new g_disabledMsg[] = "DISABLED_MSG"

public plugin_init() {
  register_plugin("DoD Stats",AMXX_VERSION_STR,"AMXX Dev Team")

  register_dictionary("common.txt")
  register_dictionary("stats_dod.txt")

  register_event("30","eInterMission","a")
  register_event("ResetHUD","eResetHud","b") 

  register_event("RoundState","round_end","a","1=3","1=4")
  register_event("RoundState","show_score","a","1=1")
  register_event("CurWeapon","NadeCatch","b","1=1","2=15","2=16")
  register_event("ObjScore","get_score","a") 

  register_clcmd("say /hp","cmdKiller",0,"- displays info. about your killer") 
  register_clcmd("say /stats","cmdStats",0,"- displays others stats")
  register_clcmd("say /statsme","cmdStatsMe",0,"- displays your stats")
  register_clcmd("say /top15","cmdTop15",0,"- displays top 15 players")
  register_clcmd("say /top10","cmdTop15",0,"- displays top 15 players") // for statsme users :)
  register_clcmd("say /topx","cmdTop15",0,"- displays top X players")
  register_clcmd("say /rank","cmdRank",0,"- displays your server stats")
  register_clcmd("say /ff","cmdFF",0,"- displays friendly fire status")

  register_cvar("dodstats_topvalue","10")
  register_cvar("dodstats_maxmenupos","7")
  register_cvar("dodstats_statstime","5.0")

  register_statsfwd(XMF_DAMAGE)
  register_statsfwd(XMF_DEATH)
  
  register_menucmd(register_menuid("Server Stats"),1023,"actionStatsMenu")

  g_damage_sync = CreateHudSyncObj()
  g_center1_sync = CreateHudSyncObj()
  g_center2_sync = CreateHudSyncObj()
  g_left_sync = CreateHudSyncObj()
}

public plugin_cfg(){
  server_cmd(g_addStast,"Stats at the end of map","EndPlayer")  
  server_cmd(g_addStast,"Top15 at the end of map","EndTop15")  
  server_cmd(g_addStast,"Say /stats","SayStatsAll") 
  server_cmd(g_addStast,"Say /top15","SayTop15")
  server_cmd(g_addStast,"Say /rank","SayRank") 
  server_cmd(g_addStast,"Say /statsme","SayStatsMe")
  server_cmd(g_addStast,"Show Attackers","ShowAttackers") 
  server_cmd(g_addStast,"Show Victims","ShowVictims") 
  server_cmd(g_addStast,"Show killer","ShowKiller") 
  server_cmd(g_addStast,"Show killer hp","KillerHp") 
  server_cmd(g_addStast,"Say /hp","SayHP") 
  server_cmd(g_addStast,"Say /ff","SayFF") 

  server_cmd(g_addStast,"MultiKill","MultiKill") 
  server_cmd(g_addStast,"MultiKill Sound","MultiKillSound") 
  server_cmd(g_addStast,"Knife Kill","KnifeKill")  
  server_cmd(g_addStast,"Knife Kill Sound","KnifeKillSound")    
  server_cmd(g_addStast,"Grenade Kill","GrenadeKill")
  server_cmd(g_addStast,"Grenade Suicide","GrenadeSuicide")
  server_cmd(g_addStast,"HeadShot Kill","HeadShotKill")  
  server_cmd(g_addStast,"HeadShot Kill Sound","HeadShotKillSound")
  server_cmd(g_addStast,"Killing Streak","KillingStreak")  
  server_cmd(g_addStast,"Killing Streak Sound","KillingStreakSound")  
  server_cmd(g_addStast,"Double Kill","DoubleKill")    
  server_cmd(g_addStast,"Double Kill Sound","DoubleKillSound") 
  server_cmd(g_addStast,"Bullet Damage","BulletDamage") 
  server_cmd(g_addStast,"TA/TK Info","TAInfo") 
  server_cmd(g_addStast,"Round Score","RoundScore") 
  server_cmd(g_addStast,"End Round Stats","EndRoundStats") 
  server_cmd(g_addStast,"Grenade Catch","GreCatch") 
  server_cmd(g_addStast,"Grenade Catch Sound","GreCatchSound") 
  server_cmd(g_addStast,"Enemy Grenade Kill","EnemyGreKill") 
  server_cmd(g_addStast,"Enemy Grenade Kill Sound","EnemyGreKillSound") 
  server_cmd(g_addStast,"Lead Sounds","LeadSounds") 
  server_cmd(g_addStast,"Mortar Kill","MortarKill")
}

public cmdFF(id){
  if ( !SayFF ){
    client_print(id,print_chat,"%L",id,g_disabledMsg )
    return PLUGIN_HANDLED
  }
  client_print( 0, print_chat,"%L^t%L",LANG_PLAYER,"FFIRE_IS",LANG_PLAYER, ( get_cvar_num( "mp_friendlyfire" ) ) ? "ON" : "OFF" )
  return PLUGIN_CONTINUE
}

public endGameStats(){
  new i
  if ( EndPlayer ){
    new players[32], inum
    get_players(players,inum)
    for(i = 0; i < inum; ++i){
        displayStats_steam(players[i],players[i])
    }
  } 
  else if ( EndTop15 ){
    new players[32], inum
    get_players(players,inum)

    new g_Top[8], top = get_cvar_num("dodstats_topvalue") 
    for(i = 0; i < inum; ++i){
      format(g_Top,15,"%L",i,"TOPX",top)
      getTop15_steam(i)
      show_motd(players[i],g_Buffer,g_Top)
    }
  }
}

public eInterMission()
  if ( isDSMActive() )
    set_task(1.0,"endGameStats")

public cmdStats(id){
  if ( !SayStatsAll || !isDSMActive() ){
    client_print(id,print_chat,"%L",id,g_disabledMsg )
    return PLUGIN_HANDLED
  }
  showStatsMenu(id,g_userPosition[id]=0)
  return PLUGIN_CONTINUE
}


/* build list of attackers */ 
getAttackers(id) { 
  new name[32],wpn[32], stats[9],body[8],found=0 
  new pos = format(g_Buffer,2047,"%L^n",id,"ATTACKERS") 
  new amax = get_maxplayers() 
  for(new a = 1; a <= amax; ++a){ 

    if(get_user_astats(id,a,stats,body,wpn,31))
    { 
      found = 1 
      if (stats[0]) 
        format(wpn,31," -- %s",wpn) 
      else 
        wpn[0] = 0 
      get_user_name(a,name,31) 
      pos += format(g_Buffer[pos],2047-pos,"%s -- %d %L / %d %L%s^n",name,stats[6],id,"DMG",stats[5],id,"HIT_S",wpn) 
    } 
  } 
  return found 

} 

/* build list of victims */ 
getVictims(id) { 
  new name[32],wpn[32], stats[9],body[8],found=0 
  new pos = format(g_Buffer,2047,"%L^n",id,"VICTIMS") 
  new amax = get_maxplayers() 
  for(new a = 1; a <= amax; ++a){ 
    if(get_user_vstats(id,a,stats,body,wpn,31))
    { 
      found = 1 
      if (stats[1]) 
        format(wpn,31," -- %s",wpn) 
      else 
        wpn[0] = 0 
      get_user_name(a,name,31) 
      pos += format(g_Buffer[pos],2047-pos,"%s -- %d %L / %d %L%s^n",name,stats[6],id,"DMG",stats[5],id,"HITS",wpn) 
    } 
  } 
  return found 
} 

/* build list of hita for AV List */ 
getHits(id,killer) { 
  new stats[9], body[8], pos = 0 
  g_Buffer[0] = 0 
  get_user_astats(id,killer,stats,body) 
  for(new a = 1; a < 8; ++a) 
    if(body[a]) 
      pos += format(g_Buffer[pos],2047-pos,"%L: %d^n",id,g_bodyParts[a],body[a]) 
} 

/* build list of hits for say hp */ 
getMyHits(id,killed) { 
  new name[32], stats[9], body[8], found = 0 
  get_user_name(killed,name,31) 
  new pos = format(g_Buffer,2047,"%L",id,"YOU_HIT",name) 
  get_user_vstats(id,killed,stats,body) 
  for(new a = 1; a < 8; ++a){ 
    if(body[a]){ 
      found = 1 
      pos += format(g_Buffer[pos],2047-pos," %L: %d ",id,g_bodyParts[a],body[a]) 
    } 
  } 
  return found 

} 

public eResetHud( id ) 
  g_Killers[ id ][0] = 0

public cmdKiller(id) { 
  if ( !SayHP || !isDSMActive() ){ 
    client_print(id,print_chat,"%L",id,g_disabledMsg )
    return PLUGIN_HANDLED 
  } 
  if (g_Killers[id][0]) { 
    new name[32], stats[9], body[8], wpn[33], mstats[9], mbody[8] 
    get_user_name(g_Killers[id][0],name,31) 
    get_user_astats(id,g_Killers[id][0],stats,body,wpn,31) 
    get_user_vstats(id,g_Killers[id][0],mstats,mbody) 
    client_print(id,print_chat,"%L",id,"KILL_INFO1", name,wpn,float(g_Killers[id][2]) * 0.0254 ) 
    client_print(id,print_chat,"%L",id,"KILL_INFO2", stats[6],stats[5], g_Killers[id][1] ) 
    client_print(id,print_chat,"%L",id,"KILL_INFO3", mstats[6], mstats[5] ) 

    if (getMyHits(id,g_Killers[id][0])) client_print(id,print_chat,"%L",id,"KILL_INFO4",g_Buffer) 
  } 
  else { 
    client_print(id,print_chat,"%L",id,"NO_KILLER") 
  } 
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
    new option = g_userPosition[id] * get_cvar_num("dodstats_maxmenupos") + key
    new index = g_userPlayers[id][option]
    if (is_user_connected(index)){
      if (g_userState[id]){
        displayRank_steam(index,id)
      }
      else{
        displayStats_steam(index,id)
      }
    }
    showStatsMenu(id,g_userPosition[id])
    }
  }
  return PLUGIN_HANDLED
}

showStatsMenu(id,pos){
  if (pos < 0) return PLUGIN_HANDLED
  new max_menupos = get_cvar_num("dodstats_maxmenupos")
  new menu_body[512], inum, k = 0, start = pos * max_menupos 
  get_players(g_userPlayers[id],inum)
  if (start >= inum) start = pos = g_userPosition[id] = 0

  new len = format(menu_body,511,"\y%L\R%d/%d^n\w^n",id,"SERVER_STATS",pos + 1,((inum/max_menupos)+((inum%max_menupos)?1:0)))
  new name[32], end = start + max_menupos, keys = (1<<9)|(1<<7)
  if (end > inum) end = inum
  for(new a = start; a < end; ++a){
    get_user_name(g_userPlayers[id][a],name,31)
    keys |= (1<<k)
    len += format(menu_body[len],511-len,"%d. %s^n",++k,name)
  }
  len += format(menu_body[len],511-len,"^n8. %L^n",id,g_userState[id] ? "SHOW_RANK" : "SHOW_STATS" )
  if (end != inum){
    len += format(menu_body[len],511-len,"^n9. More...^n0. %s" , pos ? "Back" : "Exit" )
    keys |= (1<<8)

  }

  else len += format(menu_body[len],511-len,"^n0. %s" , pos ? "Back" : "Exit" )
  show_menu(id,keys,menu_body,-1,"Server Stats")
  return PLUGIN_HANDLED
}


public NadeCatch(id){
  if ( !isDSMActive() )
    return PLUGIN_CONTINUE

  if ( GreCatch || GreCatchSound ){
    new GreId = read_data(2)
    new catch = ( ( get_user_team(id) == 1 && GreId == 15 ) ||  ( get_user_team(id) == 2 && GreId == 16 ) ) ? 1:0
    if ( catch ) {
      if ( GreCatch ){
        new player_name[32] 
        get_user_name(id,player_name,32)
        set_hudmessage(200, 100, 0, -1.0, 0.20, 0, 6.0, 6.0, 0.5, 0.15, -1)
        for (new i=1;i<=get_maxplayers();i++){
          if ( g_Killers[i][0] && g_DeathStats[i] > get_gametime() )
            continue
          show_hudmessage(i,"%L",i,"NADE_CAUGHT",player_name) 
        } 
      }
      if ( GreCatchSound ) client_cmd(0,"spk misc/impressive")
    }
  }
  return PLUGIN_CONTINUE
}

public show_score(){
  if ( RoundScore && isDSMActive() ){
    set_hudmessage( 255, 100, 50, -1.0, 0.30, 0, 4.0, 5.0, 0.5, 0.15, -1 )
    show_hudmessage( 0 ,"Allies %d -- %d Axis^n(%d--%d)", g_RoundScore[0] , g_RoundScore[1] , dod_get_team_score(ALLIES) , dod_get_team_score(AXIS) )
  }
  return PLUGIN_CONTINUE
}

public round_end(){
  if ( !isDSMActive() )
    return PLUGIN_CONTINUE

  if ( RoundScore ){
    new result = read_data(1)
    g_RoundScore[result-3]++ 
  }

  if ( !EndRoundStats ) return PLUGIN_CONTINUE

  new g_Buffer2[1024], len, players[32], pnum, stats[9],bodyhits[8]
  get_players( players , pnum ) 


  new score = 0, kills = 0, hs =0 , damage = 0, hits = 0, who1 = 0, who2 = 0, who3 = 0
  new name1[32],name2[32],name3[32]

  for(new i = 0; i < pnum; ++i){
     get_user_rstats( players[i],stats, bodyhits )
     if ( stats[7] > score ){
        who1 = players[i]
        score = stats[7]
     }  
  }
  for(new i = 0; i < pnum; ++i){
     get_user_rstats( players[i],stats, bodyhits )
     if ( stats[0] > kills ){
        who2 = players[i]
        kills = stats[0]
        hs = stats[2]
     }  
  }
  for(new i = 0; i < pnum; ++i){
     get_user_rstats( players[i],stats, bodyhits )
     if ( stats[6] > damage ){
        who3 = players[i]
        hits = stats[5]
        damage = stats[6]
     }  
  }

  if ( is_user_connected(who1) ) {
     get_user_name( who1, name1, 31 )
  }
  if ( is_user_connected(who2) ) {
     get_user_name( who2, name2, 31 )

  }
  if ( is_user_connected(who3) ) {
     get_user_name( who3, name3, 31 )
  }

  get_players(players,pnum,"c")
  for (new i=0;i<pnum;i++) {
     len = 0
     len += format(g_Buffer2[len] , 1023 - len ,
     "%L: %s^n%d %L^n",players[i],"BEST_SCORE", name1 , score,players[i],"POINTS" )
     len += format(g_Buffer2[len] , 1023 - len ,
     "%L: %s^n%d %L / %d %L^n",players[i],"MOST_KILLS",name2,kills,players[i],(kills == 1) ? "KILL":"KILLS",hs,players[i],(hs == 1) ? "HEADSHOT":"HEADSHOTS" )
     len += format(g_Buffer2[len] , 1023 - len ,
     "%L: %s^n%d %L / %d %L^n",players[i],"MOST_DAMAGE",name3 , damage,players[i],"DAMAGE",hits,players[i],(hits == 1) ? "HIT": "HITS" )
     set_hudmessage(100,200,0,0.02,0.40,2, 0.01, 5.0, 0.01, 0.01, -1 )
     show_hudmessage( players[i] , "%s", g_Buffer2 )
  }

  return PLUGIN_CONTINUE
}

public client_putinserver(id)
{
  g_multiKills[id] = { 0 , 0 }
  g_streakKills[ id ] = { 0 , 0 }
}

public client_damage(attacker,victim,damage,wpnindex,hitplace,TA) 
{ 
  if ( TA ){
    if ( TAInfo && is_user_alive(victim) ){
      new attacker_name[32]
      get_user_name(attacker,attacker_name,31) 
      client_print(0,print_chat,"%L",LANG_PLAYER,"TA_MSG",attacker_name)
    }
    return PLUGIN_CONTINUE
  }
  if ( BulletDamage ) { 
    if ( attacker==victim || xmod_is_melee_wpn(wpnindex) ) return PLUGIN_CONTINUE
    set_hudmessage(0, 100, 200, 0.45, 0.85, 2, 0.1, 4.0, 0.02, 0.02)
    ShowSyncHudMsg(attacker,g_damage_sync,"%i",damage)
    set_hudmessage(200, 0, 0, 0.55, 0.85, 2, 0.1, 4.0, 0.02, 0.02)
    ShowSyncHudMsg(victim,g_damage_sync,"%i",damage)
  } 
  return PLUGIN_CONTINUE 
}

/* save state at death */ 
public client_death(killer,victim,wpnindex,hitplace,TK)
{  
  if (!is_user_connected(killer) || !is_user_connected(victim))
    return PLUGIN_CONTINUE

  new killer_name[32]
  get_user_name(killer,killer_name,31) 
  
  new enemygre = ( ( (wpnindex == DODW_HANDGRENADE || wpnindex == DODW_MILLS_BOMB) && get_user_team(killer) == 2 ) || ( wpnindex == DODW_STICKGRENADE && get_user_team(killer) == 1 ) ) ? 1:0

  if ( KillingStreak || KillingStreakSound ){ 
    g_streakKills[ victim ][ 1 ]++
    g_streakKills[ victim ][ 0 ] = 0
  }

  if ( TK && TAInfo ){
    client_print(0,print_chat,"%L",LANG_PLAYER,"TK_MSG",killer_name)
    if ( enemygre ){
      set_hudmessage(255, 100, 100, -1.0, 0.15, 1, 6.0, 6.0, 0.5, 0.15, -1)
      ShowSyncHudMsg(victim, g_center1_sync, "%L",victim,"NADE_FAILEDTK",killer_name)
    }
  }

  new grenade = ( wpnindex == DODW_HANDGRENADE || wpnindex == DODW_STICKGRENADE || wpnindex == DODW_MILLS_BOMB ) ? 1:0
  new headshot = ( hitplace == HIT_HEAD ) ? 1:0
  new selfKill = ( killer == victim ) ? 1:0

  new victim_name[32] 
  get_user_name(victim,victim_name,31) 

  new Float:statstime = get_cvar_float("dodstats_statstime")

  if ( ShowVictims && getVictims(victim) ){ 
    set_hudmessage(0,80,220,0.55,0.60,0, statstime, 12.0, 1.0, 2.0, -1) 
    show_hudmessage(victim, "%s", g_Buffer) 
  } 
  if ( ShowAttackers  && getAttackers(victim)){ 
    set_hudmessage(220,80,0,0.55,0.35,0, statstime, 12.0, 1.0, 2.0, -1) 
    show_hudmessage(victim,"%s", g_Buffer) 
  } 

  if ( selfKill && grenade && GrenadeSuicide ){ 
    set_hudmessage(255, 100, 100, -1.0, 0.15, 1, 6.0, 6.0, 0.5, 0.15, -1)
    if ( !enemygre ) ShowSyncHudMsg(0, g_center1_sync,"%L",LANG_PLAYER,g_SHeMessages[ random_num(0,3) ],victim_name) 
    else
      for (new i=1;i<=get_maxplayers();i++){
        if ( g_Killers[i][0] && g_DeathStats[i] > get_gametime() )
          continue
        ShowSyncHudMsg(i, g_center1_sync, "%L",i,"NADE_FAILED",victim_name) 
      }

  }

  if ( selfKill || TK )
    return PLUGIN_CONTINUE

  new vorigin[3], korigin[3] 

  get_user_origin(victim,vorigin) 
  get_user_origin(killer,korigin) 
  g_Killers[victim][0] = killer 
  g_Killers[victim][1] = get_user_health(killer) 
  g_Killers[victim][2] = get_distance(vorigin,korigin) 

  g_DeathStats[victim] = get_gametime() + statstime 


  if ( ShowKiller && !(!get_cvar_num("dodstats_rankbots") &&  (is_user_bot(killer) || is_user_bot(victim)))  ){ 
    new stats[9], body[8], wpn[33], mstats[9], mbody[8] 
  
    get_user_astats(victim,killer,stats,body,wpn,31) 
    get_user_vstats(victim,killer,mstats,mbody) 
    set_hudmessage(220,80,0,0.05,0.15,0, statstime, 12.0, 1.0, 2.0, -1) 
    getHits(victim,killer) 
    show_hudmessage(victim,"%L%L%L%L",victim,"KILL_INFO1",killer_name,wpn,float(g_Killers[victim][2]) * 0.0254,
			/*2*/ victim,"KILL_INFO2",stats[6],stats[5],g_Killers[victim][1],
			/*3*/ victim,"KILL_INFO3",mstats[6],mstats[5],
			/*4*/ victim,"KILL_INFO4",g_Buffer ) 
  } 

  if ( KillerHp ){
    new kmsg[128]
    format(kmsg,127,"%L",victim,"STILL_HAS",killer_name,g_Killers[victim][1])
    client_print(victim,print_console,"%s^n",kmsg)
    set_hudmessage(255,255,255,0.02,0.9,2, 1.5, 3.0, 0.02, 5.0, -1)
    show_hudmessage(victim, "%s", kmsg)
  }

  if ( KillingStreak || KillingStreakSound ){    
    g_streakKills[ killer ][ 0 ]++
    g_streakKills[ killer ][ 1 ] = 0
    new a = g_streakKills[ killer ][ 0 ] - 3
    if ( (a > -1) && !( a % 2 ) ) {
      if ( (a >>= 1) > 6 ) a = 6
      if ( KillingStreak ){
        set_hudmessage(0, 100, 255, 0.05, 0.55, 2, 0.02, 6.0, 0.01, 0.1, -1)
        for (new i=1;i<=get_maxplayers();i++){
          if ( g_Killers[i][0] && g_DeathStats[i] > get_gametime() )
            continue
          ShowSyncHudMsg(i, g_left_sync, "%L",i,g_KillingMsg[ a ], killer_name) 
        }
      }
      if (  KillingStreakSound )  client_cmd( 0 ,  "spk misc/%s" , g_Sounds[ a ] )


    }
  }

  if ( MultiKill || MultiKillSound ) {
      g_multiKills[killer][0]++ 
      g_multiKills[killer][1] += headshot
      new param[2]
      param[0] = killer 
      param[1] = g_multiKills[killer][0] 
      set_task( 4.0 + float( param[1] ) ,"checkKills",0,param,2)
  }

  if ( xmod_is_melee_wpn(wpnindex) && ( KnifeKill || KnifeKillSound )  ){
    if ( KnifeKill ){


      set_hudmessage(255, 100, 100, -1.0, 0.15, 1, 6.0, 6.0, 0.5, 0.15, -1)
      for (new i=1;i<=get_maxplayers();i++){
        if ( g_Killers[i][0] && g_DeathStats[i] > get_gametime() )

          continue
        ShowSyncHudMsg(i, g_center1_sync, "%L",i,g_KnifeMsg[ random_num(0,3) ],killer_name,victim_name) 
      } 
    }
    if ( KnifeKillSound ) client_cmd(0,"spk misc/humiliation") 
  }
  else if ( grenade ){
    if ( enemygre ){
      if ( EnemyGreKill ){
        set_hudmessage(255, 100, 100, -1.0, 0.15, 1, 6.0, 6.0, 0.5, 0.15, -1) 
        for (new i=1;i<=get_maxplayers();i++){
          if ( g_Killers[i][0] && g_DeathStats[i] > get_gametime() )
            continue
          ShowSyncHudMsg(i, g_center1_sync, "%L",LANG_PLAYER,"NADE_MASTER",killer_name) 
        } 
      }
      if ( EnemyGreKillSound ) client_cmd(0,"spk misc/godlike")
    }
    else if ( GrenadeKill ){
      set_hudmessage(255, 100, 100, -1.0, 0.15, 1, 6.0, 6.0, 0.5, 0.15, -1) 
      for (new i=1;i<=get_maxplayers();i++){
        if ( g_Killers[i][0] && g_DeathStats[i] > get_gametime() )
          continue
        ShowSyncHudMsg(i, g_center1_sync, "%L",i,g_HeMessages[ random_num(0,3)],killer_name,victim_name) 
      }   
    }
  }

  if ( headshot && (HeadShotKill || HeadShotKillSound) && !xmod_is_melee_wpn(wpnindex) ){
    if ( HeadShotKill ){
      new weapon[32], message[256], players[32], pnum
      xmod_get_wpnname(wpnindex,weapon,31) 

      get_players(players,pnum,"c")
      for (new i=0;i<pnum;i++) {
        if ( g_Killers[i][0] && g_DeathStats[i] > get_gametime() )
          continue
        format( message, sizeof(message)-1, "%L",players[i],g_HeadShots[ random_num(0,6) ] )
        replace( message, sizeof(message)-1, "$vn", victim_name )
        replace( message, sizeof(message)-1, "$wn", weapon )    
        replace( message, sizeof(message)-1, "$kn", killer_name )
        set_hudmessage(100, 100, 255, -1.0, 0.19, 0, 6.0, 6.0, 0.5, 0.15, -1)  
        ShowSyncHudMsg(players[i], g_center2_sync, "%s", message) 
      }
    }
    if ( HeadShotKillSound ) client_cmd(0,"spk misc/headshot") 
  }

  if ( wpnindex == DODW_MORTAR && MortarKill ){
    set_hudmessage(100, 100, 255, -1.0, 0.19, 0, 6.0, 6.0, 0.5, 0.15, -1) 
    for (new i=1;i<=get_maxplayers();i++){
      if ( g_Killers[i][0] && g_DeathStats[i] > get_gametime() )
        continue
      ShowSyncHudMsg(i, g_center2_sync, "%L",i,mortarmsg[random_num(0,1)],killer_name,victim_name)
    } 
  }

  if ( DoubleKill || DoubleKillSound ){
    new Float:nowtime = get_gametime()
    if ( g_prevKill == nowtime && g_prevKillerId == killer ){
      g_KillCount++
      if ( DoubleKill || DoubleKillSound){
        set_task(0.01,"showDoubleKill")
      }
    }
    else g_KillCount = 1

    g_prevKill = nowtime
    g_prevKillerId = killer
  }

  return PLUGIN_CONTINUE
}

public showDoubleKill(){ 
  if (g_KillCount < 2)
    return
  new pos = g_KillCount - 2
  if ( pos > 2 ) pos = 2
  if ( DoubleKill ) {
    new name[32]
    get_user_name(g_prevKillerId,name,31)
    if ( pos == 2 ){
      new kills[3]
      num_to_str(g_KillCount,kills,2)
    }
    set_hudmessage(65, 102, 158, -1.0, 0.25, 0, 6.0, 6.0, 0.5, 0.15, -1)
    for (new i=1;i<=get_maxplayers();i++){
      if ( g_Killers[i][0] && g_DeathStats[i] > get_gametime() )
        continue
      show_hudmessage(i,"%L",i,g_DoubleKillMsg[pos],name,g_KillCount) 
    } 
  }
  if ( DoubleKillSound ) {
    client_cmd(0,"spk misc/%s",g_DoubleKillSound[pos])
  }
}

public checkKills(param[]){ 
  new id = param[0]
  new a = param[1]
  if (a == g_multiKills[id][0]){
    a -= 3 
    if ( a > -1 ){
      if ( MultiKill ) {
        new name[32]
        get_user_name(id,name,31)
        set_hudmessage(255, 0, 100, 0.05, 0.65, 2, 0.02, 6.0, 0.01, 0.1, -1)
        if ( a > 6 ) a = 6
        for (new i=1;i<=get_maxplayers();i++){
          if ( g_Killers[i][0] && g_DeathStats[i] > get_gametime() )
            continue
          ShowSyncHudMsg(i, g_left_sync, "%L",i,g_MultiKillMsg[a],name,g_multiKills[id][0],g_multiKills[id][1]) 
        } 
      }
      if ( MultiKillSound ) client_cmd(0,"spk misc/%s",g_Sounds[a])
    }
    g_multiKills[id] = { 0,0 }
  }
}

/***************
  LeadSounds
****************/

new LeaderScore 
new NumOfLeaders 
new LeaderID 
new PScore[33] 

public client_disconnect(id) { 
  if ( !LeadSounds || isDSMActive() ) return PLUGIN_CONTINUE
  if ( PScore[id] == LeaderScore && LeaderScore > 0 ){ 
    NumOfLeaders -- 
    PScore[id] = 0 
    if ( NumOfLeaders == 0 ){ 
      LeaderScore = 0 
      for ( new i=1; i<33; i++ ) 
        if ( PScore[i] > LeaderScore ){ 


          LeaderScore = PScore[i] 
          NumOfLeaders = 1 
          LeaderID = i 
        } 
        else if ( PScore[i] == LeaderScore ) 
          NumOfLeaders ++ 

      if ( LeaderScore == 0 ) 
        NumOfLeaders = 0 
      else if ( NumOfLeaders == 1 ) 
        client_cmd( LeaderID,"spk misc/takenlead" ) 
      else if ( NumOfLeaders > 1 ) 
        for ( new i=1; i<33; i++ ) 
          if ( PScore[i] == LeaderScore ) 
            client_cmd( i,"spk misc/tiedlead") 
      //else no players on server or have 0 score 
    } 

    else if ( NumOfLeaders == 1 ) 
      if ( LeaderID != id ){ 
        client_cmd( LeaderID,"spk misc/takenlead" ) 
      } 
      else { 
        for ( new i=1; i<33; i++ ) 
        if ( PScore[i] == LeaderScore ) client_cmd( i,"spk misc/takenlead" ) 
      } 
  } 
  else PScore[id] = 0 
  return PLUGIN_CONTINUE
}
 
public get_score(){
  if ( !LeadSounds || !isDSMActive() ) return PLUGIN_CONTINUE 
  new PlayerID = read_data(1) 
  new PlayerScore = read_data(2) 

  if ( PlayerScore > PScore[PlayerID] ){ 
    PScore[PlayerID] = PlayerScore 

    if ( PlayerScore > LeaderScore  ){ 
      if ( NumOfLeaders == 1 ){ 
        if ( LeaderID != PlayerID ){  
          client_cmd( LeaderID,"spk misc/lostlead" ) 
          client_cmd( PlayerID,"spk misc/takenlead" ) 
        } 
      } 
      else if ( NumOfLeaders > 1 ){ 

        for ( new i=1; i<33; i++ ) 
          if ( PScore[i] == LeaderScore  && i != PlayerID ) 
            client_cmd( i,"spk misc/lostlead" ) 
        client_cmd( PlayerID,"spk misc/takenlead" ) 
      } 
      else if ( NumOfLeaders == 0 ){ // start 
        for ( new i=1; i<33; i++ ) 
          if ( i != PlayerID && is_user_connected(i) ) client_cmd( i,"spk misc/lostlead" ) 
        client_cmd( PlayerID,"spk misc/takenlead" ) 
      } 

      LeaderScore = PlayerScore 
      LeaderID = PlayerID    
      NumOfLeaders = 1 
    } 
    else if ( PlayerScore == LeaderScore ){ 
      if ( NumOfLeaders == 1 )
		client_cmd( LeaderID,"spk misc/tiedlead" ) 
      client_cmd( PlayerID,"spk misc/tiedlead" ) 
      NumOfLeaders++ 
    } 
  } 
  return PLUGIN_CONTINUE 
}

isDSMActive(){
  if ( get_cvar_num("dodstats_pause") ) 
    return 0
  return 1
}

public cmdStatsMe(id){
  if ( !SayStatsMe || !isDSMActive() ){
    client_print(id,print_chat,"%L",id,g_disabledMsg )
    return PLUGIN_HANDLED
  }
  displayStats_steam(id,id)
  return PLUGIN_CONTINUE
}

displayStats_steam(id,dest) {
 new name[32], stats[9], body[8]
 get_user_wstats(id,0,stats,body)

 new pos = copy(g_Buffer,2047,"<html><head><style type=^"text/css^">pre{color:#FFB000;}body{background:Black;margin-left:8px;margin-top:0px; color:#FFB000;}</style></head><pre><body>")
 pos += format(g_Buffer[pos],2047-pos,"<table><tr><td>%L</td><td>%L</td><td>%L</td><td>%L</td><td>%L</td><td>%L</td><td>%L</td></tr>",
               dest,"M_KILLS",dest,"M_DEATHS",dest,"M_SCORE",dest,"M_TKS",dest,"M_HITS",dest,"M_SHOTS",dest,"M_HS")

 pos += format(g_Buffer[pos],2047-pos,"<tr><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td></tr></table><br><br><br>",
		stats[0],stats[1],stats[7],stats[3],stats[5],stats[4],stats[2])

 pos += format(g_Buffer[pos],2047-pos,"<table><tr><td>%L</td><td>%L</td><td>%L</td><td>%L</td><td>%L</td><td>%L<td></tr>",
               dest,"M_WEAPON",dest,"M_SHOTS",dest,"M_HITS",dest,"M_DAMAGE",dest,"M_KILLS",dest,"M_DEATHS")

 for(new a = 1; a < DODMAX_WEAPONS; ++a) {
   if (get_user_wstats(id,a,stats,body)){
     if ( xmod_is_melee_wpn(a) )
       stats[4] = -1;
     xmod_get_wpnname(a,name,31)
     pos += format(g_Buffer[pos],2047-pos,"<tr><td>%s</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td></tr>^n",
		name,stats[4],stats[5],stats[6],stats[0],stats[1])
   }
 }
 copy(g_Buffer[pos],2047-pos,"</table></pre></body></html>")

 get_user_name(id,name,31)
 show_motd(dest,g_Buffer,name)
}

public cmdRank(id){
  if ( !SayRank || !isDSMActive() ){
    client_print(id,print_chat,"%L",id,g_disabledMsg )
    return PLUGIN_HANDLED
  }
  displayRank_steam(id,id)

  return PLUGIN_CONTINUE
}

displayRank_steam(id,dest) {
 new name[32], stats[9], body[8]
 new rank_pos = get_user_stats(id,stats,body)

 new pos = copy(g_Buffer,2047,"<html><head><style type=^"text/css^">pre{color:#FFB000;}body{background:Black;margin-left:8px;margin-top:0px;color:#FFB000;}</style></head><pre><body>")

 pos += format(g_Buffer[pos],2047-pos,
               "<table><tr><td>%L</td><td>%L</td><td>%L</td><td>%L</td><td>%L</td><td>%L</td><td>%L</td></tr>",dest,"M_KILLS",dest,"M_DEATHS",dest,"M_SCORE",dest,"M_TKS",dest,"M_HITS",dest,"M_SHOTS",dest,"M_HS")

 pos += format(g_Buffer[pos],2047-pos,
               "<tr><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td></tr></table><br><br>",
               stats[0],stats[1],stats[7],stats[3],stats[5],stats[4],stats[2])

 pos += format(g_Buffer[pos],2047-pos,"%L^n%L: %d^n%L: %d^n%L: %d^n%L: %d^n%L: %d^n%L: %d^n%L: %d^n",dest,"M_HITS",dest,g_bodyParts[1],body[1],dest,g_bodyParts[2],body[2],dest,g_bodyParts[3],body[3],dest,g_bodyParts[4],body[4],dest,g_bodyParts[5],body[5],dest,g_bodyParts[6],body[6],dest,g_bodyParts[7],body[7])

 pos += format(g_Buffer[pos],2047-pos,"%L <b>%d</b> %L <b>%d</b>",dest,(id==dest)?"M_YOUR_RANK_IS":"M_THEIR_RANK_IS",
											rank_pos,dest,"M_OF",get_statsnum())

 pos += format(g_Buffer[pos],2047-pos,"</pre></body></html>")

 get_user_name(id,name,31)

 show_motd(dest,g_Buffer,name)
}

public cmdTop15(id) {
  if ( !SayTop15 || !isDSMActive() ){
    client_print(id,print_chat,"%L",id,g_disabledMsg )
    return PLUGIN_HANDLED
  }
  getTop15_steam(id)
  new g_Top[8]
  format(g_Top,7,"%L",id,"TOPX",get_cvar_num("dodstats_topvalue")) 

  show_motd(id,g_Buffer,g_Top)
  return PLUGIN_CONTINUE
}

/* get top 15 */
getTop15_steam(id){
  new stats[9], body[8], name[32]

  new pos = copy(g_Buffer,2047,"<html><head><style type=^"text/css^">pre{color:#FFB000;}body{background:Black;margin-left:8px;margin-top:0px;color:#FFB000;}</style></head><pre><body>")

  pos += format(g_Buffer[pos],2047-pos,"<table><tr><td>#</td><td>%L</td><td>%L</td><td>%L</td><td>%L</td><td>%L</td><td>%L</td><td>%L</td><td>%L</td></tr>",
		id,"M_NICK",id,"M_KILLS",id,"M_DEATHS",id,"M_SCORE",id,"M_TKS",id,"M_HITS",id,"M_SHOTS",id,"M_HS")
  new imax = get_statsnum()
  new itmax =  get_cvar_num("dodstats_topvalue")
  if (imax > itmax ) 
    imax = itmax
  for(new a = 0; a < imax; ++a){
    get_stats(a,stats,body,name,31);
    replace_all(name, 31, "<", "[")
    replace_all(name, 31, ">", "]")
    pos += format(g_Buffer[pos],2047-pos,"<tr><td>%d.</td><td>%s</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td>%d</td></tr>^n",
		a+1,name,stats[0],stats[1],stats[7],stats[3],stats[5],stats[4],stats[2])
  }
  pos += format(g_Buffer[pos],2047-pos,"</table></pre></body></html>") 

}