// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2003 JustinHoMi.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// DoD Stats Logging Plugin
//

#include <amxmodx>
#include <dodx>

new g_pingSum[MAX_PLAYERS + 1]
new g_pingCount[MAX_PLAYERS + 1]

public plugin_init()
  register_plugin("Stats Logging",AMXX_VERSION_STR,"AMXX Dev Team")

public client_disconnected(id) {
  if ( is_user_bot( id ) || !is_user_connected(id) || !isDSMActive() ) return PLUGIN_CONTINUE
  remove_task( id )
  new szTeam[16],szName[MAX_NAME_LENGTH],szAuthid[32], iStats[DODX_MAX_STATS], iHits[MAX_BODYHITS], szWeapon[16]
  new iUserid = get_user_userid( id )
  get_user_info(id,"team", szTeam, charsmax(szTeam) )
  szTeam[0] -= 32;
  get_user_name(id, szName ,charsmax(szName) )
  get_user_authid(id, szAuthid , charsmax(szAuthid) )
  for(new i = 1 ; i < DODMAX_WEAPONS; ++i ) {
    if( get_user_wstats( id , i ,iStats , iHits ) )   {
      xmod_get_wpnlogname( i , szWeapon , charsmax(szWeapon) )
      log_message("^"%s<%d><%s><%s>^" triggered ^"weaponstats^" (weapon ^"%s^") (shots ^"%d^") (hits ^"%d^") (kills ^"%d^") (headshots ^"%d^") (tks ^"%d^") (damage ^"%d^") (deaths ^"%d^") (score ^"%d^")",
        szName,iUserid,szAuthid,szTeam,szWeapon,iStats[DODX_SHOTS],iStats[DODX_HITS],iStats[DODX_KILLS], iStats[DODX_HEADSHOTS],iStats[DODX_TEAMKILLS],iStats[DODX_DAMAGE],iStats[DODX_DEATHS],iStats[DODX_POINTS])
      log_message("^"%s<%d><%s><%s>^" triggered ^"weaponstats2^" (weapon ^"%s^") (head ^"%d^") (chest ^"%d^") (stomach ^"%d^") (leftarm ^"%d^") (rightarm ^"%d^") (leftleg ^"%d^") (rightleg ^"%d^")",
        szName,iUserid,szAuthid,szTeam,szWeapon,iHits[HIT_HEAD],iHits[HIT_CHEST],iHits[HIT_STOMACH],  iHits[HIT_LEFTARM],iHits[HIT_RIGHTARM],iHits[HIT_LEFTLEG],iHits[HIT_RIGHTLEG])
    }
  }
  new iTime = get_user_time( id , 1 )
  log_message("^"%s<%d><%s><%s>^" triggered ^"time^" (time ^"%d:%02d^")",
    szName,iUserid,szAuthid,szTeam, (iTime / 60),  (iTime % 60) )
  log_message("^"%s<%d><%s><%s>^" triggered ^"latency^" (ping ^"%d^")",
    szName,iUserid,szAuthid,szTeam, (g_pingSum[id] / ( g_pingCount[id] ? g_pingCount[id] : 1 ) ) )
  return PLUGIN_CONTINUE
}

public client_putinserver(id) {
  if ( !is_user_bot( id ) ){
    g_pingSum[ id ] = g_pingCount[ id ] = 0
    set_task( 19.5 , "getPing" , id , "" , 0 , "b" )
  }
}

public getPing( id ) {
  new iPing, iLoss
  get_user_ping( id , iPing, iLoss)
  g_pingSum[ id ] += iPing
  ++g_pingCount[ id ]
}

isDSMActive(){
  if ( get_cvar_num("dodstats_pause") ) 
    return 0
  return 1
}
