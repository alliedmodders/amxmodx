/* AMX Mod X
*   Stats Logging Plugin
*
* by the AMX Mod X Development Team
*  originally developed by JustinHoMi
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
#include <tfcx>

new g_pingSum[33]
new g_pingCount[33]

public plugin_init()
  register_plugin("TFC Stats Logging",AMXX_VERSION_STR,"AMXX Dev Team")

public client_disconnect(id) {
  if ( is_user_bot( id ) ) return PLUGIN_CONTINUE
  remove_task( id )
  new szTeam[16],szName[32],szAuthid[32], iStats[8], iHits[8], szWeapon[24]
  new iUserid = get_user_userid( id )
  get_user_team(id, szTeam, 15 )
  get_user_name(id, szName ,31 )
  get_user_authid(id, szAuthid , 31 )
  for(new i = 1 ; i < TFCMAX_WEAPONS ; ++i ) {
    if( get_user_wstats( id , i ,iStats , iHits ) )   {
        xmod_get_wpnlogname( i , szWeapon , 23 )

        log_message("^"%s<%d><%s><%s>^" triggered ^"weaponstats^" (weapon ^"%s^") (shots ^"%d^") (hits ^"%d^") (kills ^"%d^") (headshots ^"%d^") (tks ^"%d^") (damage ^"%d^") (deaths ^"%d^")",
        szName,iUserid,szAuthid,szTeam,szWeapon,iStats[4],iStats[5],iStats[0], iStats[2],iStats[3],iStats[6],iStats[1])
        log_message("^"%s<%d><%s><%s>^" triggered ^"weaponstats2^" (weapon ^"%s^") (head ^"%d^") (chest ^"%d^") (stomach ^"%d^") (leftarm ^"%d^") (rightarm ^"%d^") (leftleg ^"%d^") (rightleg ^"%d^")",
        szName,iUserid,szAuthid,szTeam,szWeapon,iHits[1],iHits[2],iHits[3],  iHits[4],iHits[5],iHits[6],iHits[7])
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
