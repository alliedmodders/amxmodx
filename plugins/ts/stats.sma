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
* tsstatscfg on SayTop15
* tsstatscfg on SayRank
* 
* Accept able are also parts of name:
* tsstatscfg off say
* tsstatscfg on End
*/

#include <amxmodx>
#include <tsconst>
#include <tsx>
#include <tsfun>

#define MAX_MENUPOS		7 // 7 positions in tsstatsmenu
#define StatsTime			5.0 // 5 sec hold time for ShowAttackers , ShowVictims list and ShowKiller

public EndPlayer          // displays player stats at the end of map
public EndTop15           // displays top15 at the end of map

public SayStatsAll        // displays players stats and rank
public SayTop15           // displays first 15. players
public SayRank            // displays user position in rank
public SayStatsMe         // displays user stats

public ShowAttackers  // shows attackers 
public ShowVictims    // shows victims 
public ShowKiller     // shows killer 
public KillerHp       // displays killer hp to victim console and screen 
public SayHP          // displays information about user killer 
public SayFF          // displays friendly fire status

public GrenadeKill
public GrenadeSuicide
public HeadShotKill
public HeadShotKillSound
public DoubleKill
public DoubleKillSound
public BulletDamage
public TAInfo
public FragInfo 

new g_userPosition[33]
new g_userState[33]
new g_userPlayers[33][32]
new g_Buffer[2048]
new g_Killers[33][3] 
new Float:g_DeathStats[33]

new g_center1_sync
new g_damage_sync

new g_bodyParts[8][] = {"whole body","head","chest","stomach","left arm","right arm","left leg","right leg"}

new g_HeMessages[4][] = { 
  "%s sends a little gift to %s",   
  "%s throws a small present to %s",   
  "%s made a precision throw to %s",   
  "%s got a big explosion for %s"
}
new g_SHeMessages[4][] = { 
  "%s detonated himself with a grenade",   
  "%s trys the effect of a grenade",   
  "%s kicked a grenade into his own ass",   
  "%s explodes!"
}
new g_HeadShots[7][] = { 
  "$kn killed $vn with a well^nplaced shot to the head!",   
  "$kn removed $vn's^nhead with the $wn",   
  "$kn turned $vn's head^ninto pudding with the $wn",   
  "$vn got pwned by $kn",
  "$vn's head has been^nturned into red jello",
  "$kn has superb aim with the $wn,^nas $vn well knows.",
  "$vn's head stayed in $kn's^ncrosshairs a bit too long..."
}

public plugin_precache(){
  precache_sound( "misc/headshot.wav")
  precache_sound( "misc/doublekill.wav")
  return PLUGIN_CONTINUE
}

new g_disabledMsg[] = "Server has disabled that option"

public plugin_init() {
  register_plugin("TS Stats",AMXX_VERSION_STR,"AMXX Dev Team")
  register_event("30","eInterMission","a")
  register_event("ResetHUD","eResetHud","b") 
  register_clcmd("say /hp","cmdKiller",0,"- displays info. about your killer") 
  register_clcmd("say /stats","cmdStats",0,"- displays others stats")
  register_clcmd("say /statsme","cmdStatsMe",0,"- displays your stats")
  register_clcmd("say /top15","cmdTop15",0,"- displays top 15 players")
  register_clcmd("say /rank","cmdRank",0,"- displays your server stats")
  register_clcmd("say /ff","cmdFF",0,"- displays friendly fire status")
  register_menucmd(register_menuid("Server Stats"),1023,"actionStatsMenu")

  register_statsfwd(XMF_DAMAGE)
  register_statsfwd(XMF_DEATH)

  g_damage_sync = CreateHudSyncObj()
  g_center1_sync = CreateHudSyncObj()
}



public plugin_cfg(){
  new g_addStast[] = "amx_statscfg add ^"%s^" %s"
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

  server_cmd(g_addStast,"Grenade Kill","GrenadeKill")
  server_cmd(g_addStast,"Grenade Suicide","GrenadeSuicide")
  server_cmd(g_addStast,"HeadShot Kill","HeadShotKill")  
  server_cmd(g_addStast,"HeadShot Kill Sound","HeadShotKillSound")
  server_cmd(g_addStast,"Double Kill","DoubleKill")    
  server_cmd(g_addStast,"Double Kill Sound","DoubleKillSound") 
  server_cmd(g_addStast,"Bullet Damage","BulletDamage") 
  server_cmd(g_addStast,"TA/TK Info","TAInfo") 
  server_cmd(g_addStast,"Frag Info","FragInfo") 
}

public cmdFF(id){
  if ( !SayFF ){
    client_print(id,print_chat, "%s", g_disabledMsg )
    return PLUGIN_HANDLED
  }
  client_print( 0, print_chat, "Friendly fire:  %s", ( get_cvar_num( "mp_friendlyfire" ) ) ? "ON" : "OFF" )
  return PLUGIN_CONTINUE
}

public cmdStatsMe(id){
  if ( !SayStatsMe ){
    client_print(id,print_chat, "%s", g_disabledMsg )
    return PLUGIN_HANDLED
  }
  displayStats(id,id)
  return PLUGIN_CONTINUE
}

public displayStats(id,dest) {
  new name[32], stats[8], body[8]
  get_user_wstats(id,0,stats,body)
  new pos = format(g_Buffer,2047,"Kills: %d^nDeaths: %d^nTKs: %d^nDamage: %d^nHits: %d^nShots: %d^n^n",
    stats[0],stats[1],stats[3],stats[6],stats[5],stats[4])
  for(new a = 1; a < TSMAX_WEAPONS; ++a) {
    if (get_user_wstats(id,a,stats,body)){
      if ( xmod_is_melee_wpn(a) )
        stats[4] = -1;
      xmod_get_wpnname(a,name,31)
      pos += format(g_Buffer[pos],2047-pos,"%s shots: %d  hits: %d  damage: %d  kills: %d  deaths: %d^n",
        name,stats[4],stats[5],stats[6],stats[0],stats[1])
    }
  }
  get_user_name(id,name,31)
  show_motd(dest,g_Buffer,name)
  return PLUGIN_CONTINUE
}

public cmdRank(id){
  if ( !SayRank ){
    client_print(id,print_chat, "%s", g_disabledMsg )
    return PLUGIN_HANDLED
  }
  displayRank(id,id)
  return PLUGIN_CONTINUE
}

displayRank(id,dest) {
  new name[32], stats[8], body[8]
  new rank_pos = get_user_stats(id,stats,body)
  new pos = format(g_Buffer,2047,"Kills: %d^nDeaths: %d^nTKs: %d^nDamage: %d^nHits: %d^nShots: %d^n^n",
    stats[0],stats[1],stats[3],stats[6],stats[5],stats[4])
  pos += format(g_Buffer[pos],2047-pos,"Hits:^n%s: %d^n%s: %d^n%s: %d^n%s: %d^n%s: %d^n%s: %d^n%s: %d^n^n",
    g_bodyParts[1],body[1],g_bodyParts[2],body[2],g_bodyParts[3],body[3], g_bodyParts[4],body[4],
    g_bodyParts[5],body[5],g_bodyParts[6],body[6],g_bodyParts[7],body[7])
  format(g_Buffer[pos],2047-pos,"%s rank is %d of %d",(id==dest)?"Your":"His", rank_pos,get_statsnum())
  get_user_name(id,name,31)
  show_motd(dest,g_Buffer,name)
}

public cmdTop15(id) {
  if ( !SayTop15 ){
    client_print(id,print_chat, "%s", g_disabledMsg )
    return PLUGIN_HANDLED
  }
  getTop15()
  show_motd(id,g_Buffer,"Top 15")
  return PLUGIN_CONTINUE
}

/* get top 15 */
getTop15(){
  new stats[8], body[8], name[32]
  new pos = copy(g_Buffer,2047,"#   nick                           kills/deaths    TKs      hits/shots/headshots^n")
  new imax = get_statsnum()
  if (imax > 15) imax = 15
  for(new a = 0; a < imax; ++a){
    get_stats(a,stats,body,name,31)
    replace_all(name, 31, "<", "[")
    replace_all(name, 31, ">", "]")
    pos += format(g_Buffer[pos],2047-pos,"%2d.  %-28.27s    %d/%d          %d            %d/%d/%d^n",a+1,name,stats[0],stats[1],stats[3],stats[5],stats[4],stats[2])
  }
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
    client_print(id,print_chat, "%s", g_disabledMsg )
    return PLUGIN_HANDLED
  }
  showStatsMenu(id,g_userPosition[id]=0)
  return PLUGIN_CONTINUE
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
      pos += format(g_Buffer[pos],2047-pos,"%s -- %d dmg / %d hit(s)%s^n",name,stats[6],stats[5],wpn) 
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
      pos += format(g_Buffer[pos],2047-pos,"%s -- %d dmg / %d hit(s)%s^n",name,stats[6],stats[5],wpn) 
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


/* build list of hits for say hp */ 
getMyHits(id,killed) { 
  new name[32], stats[8], body[8], found = 0 
  get_user_name(killed,name,31) 
  new pos = format(g_Buffer,2047,"You hit %s in:",name) 
  get_user_vstats(id,killed,stats,body) 
  for(new a = 1; a < 8; ++a){ 
    if(body[a]){ 
      found = 1 
      pos += format(g_Buffer[pos],2047-pos," %s: %d ",g_bodyParts[a],body[a]) 
    } 
  } 
  return found 
} 

public client_damage(attacker,victim,damage,wpnindex,hitplace,TA) { 
  if ( TA ){
    if ( TAInfo && is_user_alive(victim) ){
      new attacker_name[32]
      get_user_name(attacker,attacker_name,31) 
      client_print(0,print_chat,"%s attacked a teammate",attacker_name)
    }
    return PLUGIN_CONTINUE
  }
  if ( BulletDamage ) { 
    if ( attacker==victim ) return PLUGIN_CONTINUE
    set_hudmessage(0, 100, 200, 0.45, 0.85, 2, 0.1, 4.0, 0.02, 0.02, -1) 
    ShowSyncHudMsg(attacker,g_damage_sync,"%i",damage)
    set_hudmessage(200, 0, 0, 0.55, 0.85, 2, 0.1, 4.0, 0.02, 0.02, -1) 
    ShowSyncHudMsg(victim,g_damage_sync,"%i",damage)
  } 
  return PLUGIN_CONTINUE 
}

public client_death(killer,victim,wpnindex,hitplace,TK){
  new killer_name[32]
  get_user_name(killer,killer_name,31) 

  if ( TK ){
    if ( TAInfo ){
      client_print(0,print_chat,"%s killed a teammate !",killer_name)
    }
    return PLUGIN_CONTINUE
  }

  new killFlags = ts_getuserkillflags(killer)
  new grenade = ( wpnindex == TSW_M61GRENADE ) ? 1:0
  new headshot = ( hitplace == HIT_HEAD ) ? 1:0

  new victim_name[32] 
  get_user_name(victim,victim_name,31) 

  if ( killer == victim ){
    if ( grenade && GrenadeSuicide ){ 
      set_hudmessage(255, 100, 100, -1.0, 0.25, 1, 6.0, 6.0, 0.5, 0.15, -1)
      show_hudmessage(0, g_SHeMessages[ random_num(0,3) ],victim_name) 
    }
    return PLUGIN_CONTINUE
  }

  new vorigin[3], korigin[3] 
  get_user_origin(victim,vorigin) 
  get_user_origin(killer,korigin) 
  g_Killers[victim][0] = killer 
  g_Killers[victim][1] = get_user_health(killer) 
  g_Killers[victim][2] = get_distance(vorigin,korigin) 
  g_DeathStats[victim] = get_gametime() + StatsTime 

  if (g_Killers[victim][0]) { 
    DisplayKillInfo(victim)
    DisplayAVList(victim)
    DisplayKillerHp(victim,1)  
  } 

  if ( grenade && GrenadeKill ){
      set_hudmessage(255, 100, 100, -1.0, 0.25, 1, 6.0, 6.0, 0.5, 0.15, -1) 
      for (new i=1;i<=get_maxplayers();i++){
        if ( g_Killers[i][0] && g_DeathStats[i] > get_gametime() )
          continue
        show_hudmessage(i, g_HeMessages[ random_num(0,3)],killer_name,victim_name) 
      }   
  }
  else if ( headshot && (HeadShotKill || HeadShotKillSound) && !xmod_is_melee_wpn(wpnindex) ){
    if ( HeadShotKill ){
      new weapon[32], message[256]
      xmod_get_wpnname(wpnindex,weapon,31) 
      copy( message, sizeof(message)-1, g_HeadShots[ random_num(0,6) ] )
      replace( message, sizeof(message)-1, "$vn", victim_name )
      replace( message, sizeof(message)-1 , "$wn", weapon )    
      replace( message, sizeof(message)-1, "$kn", killer_name )
      set_hudmessage(100, 100, 255, -1.0, 0.29, 0, 6.0, 6.0, 0.5, 0.15, -1)  
      for (new i=1;i<=get_maxplayers();i++){
        if ( g_Killers[i][0] && g_DeathStats[i] > get_gametime() )
          continue
        ShowSyncHudMsg(i, g_center1_sync, "%s", message) 
      }  
    }
    if ( HeadShotKillSound ) client_cmd(0,"spk misc/headshot") 
  }

  if ( killFlags & TSKF_DOUBLEKILL ){
    if ( DoubleKill ){
      set_hudmessage(65, 102, 158, -1.0, 0.35, 0, 6.0, 6.0, 0.5, 0.15, -1)
      for (new i=1;i<=get_maxplayers();i++){
        if ( g_Killers[i][0] && g_DeathStats[i] > get_gametime() )
          continue
        show_hudmessage(i,"Wow! %s made a double kill !!!",killer_name) 
      } 
    }
    if ( DoubleKillSound )
      client_cmd(0,"spk misc/doublekill")
  }

  if ( FragInfo ){
     new kmsg[128]
     new pos = format(kmsg,63,"LastKill: %d frag(s)^n",ts_getuserlastfrag(killer))
     if ( killFlags ){
       pos += format(kmsg[pos],128-pos,"[")     

       if ( killFlags & TSKF_STUNTKILL )
         pos += format(kmsg[pos],128-pos," stunt ")

       if ( killFlags & TSKF_SLIDINGKILL )
         pos += format(kmsg[pos],128-pos," sliding ")

       if ( killFlags & TSKF_DOUBLEKILL )
         pos += format(kmsg[pos],128-pos," double ")

       if ( killFlags & TSKF_ISSPEC )
         pos += format(kmsg[pos],128-pos," spec ")

       if ( killFlags & TSKF_KILLEDSPEC )
         pos += format(kmsg[pos],128-pos," kspec ")

       pos += format(kmsg[pos],128-pos,"]")
     }
     set_hudmessage(255,255,255,0.02,0.85,2, 1.5, 3.0, 0.02, 5.0, -1)
     show_hudmessage(killer, "%s", kmsg)
  }

  return PLUGIN_CONTINUE
}

DisplayKillInfo(victim){
  if ( ShowKiller ){ 
    new name[32], stats[8], body[8], wpn[33], mstats[8], mbody[8] 
    get_user_name(g_Killers[victim][0],name,31) 
    get_user_astats(victim,g_Killers[victim][0],stats,body,wpn,31) 
    get_user_vstats(victim,g_Killers[victim][0],mstats,mbody) 
    set_hudmessage(220,80,0,0.05,0.15,0, 6.0, 12.0, 1.0, 2.0, -1) 
    getHits(victim,g_Killers[victim][0]) 
    show_hudmessage(victim,"%s killed you with %s^nfrom distance of %.2f meters.^nHe did %d damage to you with %d hit(s)^nand still has %dhp.^nYou did %d damage to him with %d hit(s).^nHe hits you in:^n%s", 
                                name,wpn,float(g_Killers[victim][2]) * 0.0254,  stats[6],stats[5],
                                g_Killers[victim][1], mstats[6],mstats[5],g_Buffer ) 
  }  
}

DisplayAVList(victim){
  if ( ShowVictims && getVictims(victim) ){ 
    set_hudmessage(0,80,220,0.55,0.60,0, 6.0, 12.0, 1.0, 2.0, -1) 
    show_hudmessage(victim, "%s", g_Buffer) 
  } 
  if ( ShowAttackers  && getAttackers(victim)){ 
    set_hudmessage(220,80,0,0.55,0.35,0, 6.0, 12.0, 1.0, 2.0, -1) 
    show_hudmessage(victim, "%s", g_Buffer) 
  } 
}

DisplayKillerHp(victim,con){
  if ( KillerHp ){
    new name[32], kmsg[128]
    get_user_name(g_Killers[victim][0],name,31)
    format(kmsg,127,"%s still has %dhp",name,g_Killers[victim][1])
    if ( con ) client_print(victim,print_console, "%s", kmsg)
    set_hudmessage(255,255,255,0.02,0.9,2, 1.5, 3.0, 0.02, 5.0, -1)
    show_hudmessage(victim, "%s", kmsg)
  }
}

public eResetHud( id ){ 
  if ( !is_user_alive(id) ){
    if ( id != g_Killers[id][0] && g_Killers[id][0] ){
      DisplayKillInfo(id)
      DisplayAVList(id)
      DisplayKillerHp(id,0)
    }
  }
  else {
    g_Killers[ id ][0] = 0
  }
}

public cmdKiller(id) { 
  if ( !SayHP ){ 
    client_print(id,print_chat, "%s", g_disabledMsg ) 
    return PLUGIN_HANDLED 
  } 
  if (g_Killers[id][0]) { 
    new name[32], stats[8], body[8], wpn[33], mstats[8], mbody[8] 
    get_user_name(g_Killers[id][0],name,31) 
    get_user_astats(id,g_Killers[id][0],stats,body,wpn,31) 
    get_user_vstats(id,g_Killers[id][0],mstats,mbody) 
    client_print(id,print_chat,"%s killed you with %s from distance of %.2f meters",  name,wpn,float(g_Killers[id][2]) * 0.0254 ) 
    client_print(id,print_chat,"He did %d damage to you with %d hit(s) and still had %dhp", 
      stats[6],stats[5], g_Killers[id][1] ) 
    client_print(id,print_chat,"You did %d damage to him with %d hit(s)",mstats[6], mstats[5] ) 
    if (getMyHits(id,g_Killers[id][0])) client_print(id,print_chat, "%s", g_Buffer) 
  } 
  else { 
    client_print(id,print_chat,"You have no killer...") 
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
    new option = g_userPosition[id] * MAX_MENUPOS + key
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
  new menu_body[512], inum, k = 0, start = pos * MAX_MENUPOS
  get_players(g_userPlayers[id],inum)
  if (start >= inum) start = pos = g_userPosition[id] = 0
  new len = format(menu_body,511,"Server Stats %d/%d^n^n",pos + 1,((inum/MAX_MENUPOS)+((inum%MAX_MENUPOS)?1:0)))
  new name[32], end = start + MAX_MENUPOS, keys = (1<<9)|(1<<7)
  if (end > inum) end = inum
  for(new a = start; a < end; ++a){
    get_user_name(g_userPlayers[id][a],name,31)
    keys |= (1<<k)
    len += format(menu_body[len],511-len,"%d. %s^n",++k,name)
  }
  len += format(menu_body[len],511-len,"^n8. %s^n",g_userState[id] ? "Show rank" : "Show stats" )
  if (end != inum){
    len += format(menu_body[len],511-len,"^n9. More...^n0. %s" , pos ? "Back" : "Exit" )
    keys |= (1<<8)
  }
  else len += format(menu_body[len],511-len,"^n0. %s" , pos ? "Back" : "Exit" )
  show_menu(id,keys,menu_body)
  return PLUGIN_HANDLED
}
