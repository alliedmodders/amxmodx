/* AMX Mod X
*   Maps Menu Plugin
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

#define MAX_MAPS 64

new g_mapName[MAX_MAPS][32]
new g_mapNums
new g_menuPosition[33]

new g_voteCount[5]

new g_voteSelected[33][4]
new g_voteSelectedNum[33]

new g_coloredMenus

new g_choosed

public plugin_init() {
  register_plugin("Maps Menu",AMXX_VERSION_STR,"AMXX Dev Team")
  register_dictionary("mapsmenu.txt")
  register_dictionary("common.txt")
  register_clcmd("amx_mapmenu","cmdMapsMenu",ADMIN_MAP,"- displays changelevel menu")
  register_clcmd("amx_votemapmenu","cmdVoteMapMenu",ADMIN_MAP,"- displays votemap menu")

  register_menucmd(register_menuid("Changelevel Menu"),1023,"actionMapsMenu")
  register_menucmd(register_menuid("Which map do you want?"),527,"voteCount")
  register_menucmd(register_menuid("Change map to"),527,"voteCount")
  register_menucmd(register_menuid("Votemap Menu"),1023,"actionVoteMapMenu")
  register_menucmd(register_menuid("The winner: ") ,3,"actionResult")

  new maps_ini_file[64];
  get_configsdir(maps_ini_file, 63);
  format(maps_ini_file, 63, "%s/maps.ini", maps_ini_file);
  if (!file_exists(maps_ini_file))
     format(maps_ini_file, 63, "mapcycle.txt")
  load_settings(maps_ini_file)

  g_coloredMenus = colored_menus()
}

public autoRefuse() {
  log_amx("Vote: %L" , "en", "RESULT_REF")
  client_print(0,print_chat, "%L", LANG_PLAYER, "RESULT_REF")
}

public actionResult(id,key) {
  remove_task( 4545454 )
  switch (key){
    case 0: {
      message_begin(MSG_ALL, SVC_INTERMISSION)
      message_end()
      set_task(2.0,"delayedChange",0, g_mapName[ g_choosed ] , strlen(g_mapName[ g_choosed ]) + 1 )
      log_amx("Vote: %L" , "en", "RESULT_ACC")
      client_print(0,print_chat, "%L", LANG_PLAYER, "RESULT_ACC")
    }
    case 1: autoRefuse()
  }
  return PLUGIN_HANDLED
}

public checkVotes( id ) {
  id -= 34567
  new num, ppl[32],a = 0
  get_players(ppl,num,"c")
  if (num == 0) num = 1
  g_choosed = -1
  for (new i = 0; i < g_voteSelectedNum[id]; ++i)
    if ( g_voteCount[a] < g_voteCount[i] )
      a = i
  if ( 100 * g_voteCount[a] / num >  50 )  {   
    g_choosed = g_voteSelected[id][a]
    client_print(0,print_chat, "%L %s", LANG_PLAYER, "VOTE_SUCCESS", g_mapName[ g_choosed  ]  )
    log_amx("Vote: %L %s", "en", "VOTE_SUCCESS", g_mapName[ g_choosed ] )
  }
  if ( g_choosed != -1 ) {  
    if ( is_user_connected( id ) ) {
      new menuBody[512]
      new len = format(menuBody,511,g_coloredMenus ? "\y%L: \w%s^n^n" :  "%L: %s^n^n", id, "THE_WINNER", g_mapName[ g_choosed  ] )
      len += format( menuBody[len] ,511 - len, g_coloredMenus ? "\y%L^n\w" : "%L^n", id, "WANT_CONT" )
      format( menuBody[len], 511-len, "^n1. %L^n2. %L",id,"YES",id,"NO")
      new menuName[64]
      format(menuName,63,"%L","en","THE_WINNER")
      show_menu( id, 0x03, menuBody, 10, menuName )
      set_task(10.0,"autoRefuse",4545454)
    }
    else {
      message_begin(MSG_ALL, SVC_INTERMISSION)
      message_end()
      set_task(2.0,"delayedChange",0, g_mapName[ g_choosed ] , strlen(g_mapName[ g_choosed ]) + 1 )
    }
  }
  else {
    client_print(0,print_chat, "%L", LANG_PLAYER, "VOTE_FAILED" )
    log_amx("Vote: %L", "en", "VOTE_FAILED")
  }
  remove_task(34567 + id)
}

public voteCount(id,key) {
  if (key > 3) {
    client_print(0,print_chat,"%L",LANG_PLAYER,"VOT_CANC")
    remove_task(34567 + id)
    set_cvar_float( "amx_last_voting" , get_gametime()  )
    log_amx("Vote: Cancel vote session")
    return PLUGIN_HANDLED
  }
  if (get_cvar_float("amx_vote_answers")) {
    new name[32]
    get_user_name(id,name,31)
    client_print(0,print_chat,"%L", LANG_PLAYER, "X_VOTED_FOR", name , key + 1 )
  }
  ++g_voteCount[key]
  return PLUGIN_HANDLED
}

isMapSelected( id , pos ) {
  for ( new a = 0 ; a < g_voteSelectedNum[ id ]; ++a )
    if ( g_voteSelected[ id ][ a ] == pos  )
      return 1
  return 0
}

displayVoteMapsMenu(id,pos) {
  if (pos < 0)
    return

  new menuBody[512], b = 0 , start = pos * 7 

  if (start >= g_mapNums)
    start = pos = g_menuPosition[id] = 0

  new len = format(menuBody,511, g_coloredMenus ? 
    "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n",
    id,"VOTEMAP_MENU",pos+1,(  g_mapNums / 7 + (( g_mapNums % 7) ? 1 : 0 )) )

  new end = start + 7, keys = MENU_KEY_0

  if (end > g_mapNums)
    end = g_mapNums

  for (new a = start; a < end; ++a) {
    if ( g_voteSelectedNum[id]==4 || isMapSelected( id , pos * 7 + b ) ) {
      ++b   
      if ( g_coloredMenus) 
        len += format(menuBody[len],511-len,"\d%d. %s^n\w",  b ,g_mapName[ a ])
      else
        len += format(menuBody[len],511-len,"#. %s^n",  g_mapName[ a ])
    }
    else {
      keys |= (1<<b)
      len += format(menuBody[len],511-len,"%d. %s^n",  ++b ,g_mapName[ a ])
    }
  }

  if ( g_voteSelectedNum[id] ) {
    keys |= MENU_KEY_8
    len += format(menuBody[len],511-len,"^n8. %L^n",id,"START_VOT")
  }
  else
    len += format(menuBody[len],511-len, g_coloredMenus ? 
      "^n\d8. %L^n\w" : "^n#. %L^n",id,"START_VOT")

  if (end != g_mapNums) {
    len += format(menuBody[len],511-len,"^n9. %L...^n0. %L^n", id, "MORE", id, pos ? "BACK" : "EXIT")
    keys |= MENU_KEY_9
  }
  else
    len += format(menuBody[len],511-len,"^n0. %L^n", id, pos ? "BACK" : "EXIT")

  if (g_voteSelectedNum[id])
    len += format(menuBody[len],511-len, g_coloredMenus ? "^n\y%L:^n\w" : "^n%L:^n",id,"SEL_MAPS")
  else
    len += format(menuBody[len],511-len, "^n^n")

  for (new c = 0; c < 4; c++) {
    if ( c < g_voteSelectedNum[id] )
      len += format(menuBody[len],511-len,"%s^n", g_mapName[  g_voteSelected[id][ c ]  ] )
    else
      len += format(menuBody[len],511-len,"^n" )
  }

  new menuName[64]
  format(menuName,63,"%L","en","VOTEMAP_MENU")

  show_menu(id,keys,menuBody,-1,menuName)
}


public cmdVoteMapMenu(id,level,cid) {
  if (!cmd_access(id,level,cid,1))
    return PLUGIN_HANDLED

  if ( get_cvar_float("amx_last_voting") > get_gametime() ) {
    client_print(id,print_chat,"%L",id,"ALREADY_VOT")
    return PLUGIN_HANDLED
  }

  g_voteSelectedNum[id] = 0
  
  if ( g_mapNums ) {
    displayVoteMapsMenu(id,g_menuPosition[id] = 0)
  }
  else {
    console_print(id,"%L",id,"NO_MAPS_MENU")
    client_print(id,print_chat,"%L",id,"NO_MAPS_MENU")
  }

  return PLUGIN_HANDLED
}

public cmdMapsMenu(id,level,cid) {
  if (!cmd_access(id,level,cid,1))
    return PLUGIN_HANDLED
  
  if ( g_mapNums ) {
    displayMapsMenu(id,g_menuPosition[id] = 0)
  }
  else {
    console_print(id,"%L",id,"NO_MAPS_MENU")
    client_print(id,print_chat,"%L",id,"NO_MAPS_MENU")
  }

  return PLUGIN_HANDLED 
}

public delayedChange(mapname[])
  server_cmd("changelevel %s",mapname)


public actionVoteMapMenu(id,key) {
  switch (key) {
    case 7: {
      new Float:voting = get_cvar_float("amx_last_voting")
      if ( voting > get_gametime() ) { 
        client_print(id,print_chat,"%L",id,"ALREADY_VOT")
        return PLUGIN_HANDLED
      } 
      if (voting && voting + get_cvar_float("amx_vote_delay") > get_gametime()) {
        client_print(id,print_chat,"%L",id,"VOT_NOW_ALLOW")
        return PLUGIN_HANDLED
      }

      g_voteCount = { 0 , 0 , 0 , 0 , 0 }

      new Float:vote_time = get_cvar_float("amx_vote_time") + 2.0 
      set_cvar_float("amx_last_voting",  get_gametime() + vote_time )      
      new iVoteTime = floatround( vote_time )

      set_task( vote_time , "checkVotes",34567 + id)

      new menuBody[512]
      new players[32]
      new pnum, keys, len

      get_players(players,pnum)

      if ( g_voteSelectedNum[id] > 1 ) {
        len = format(menuBody,511,g_coloredMenus ? 
          "\y%L^n\w^n" : "%L^n^n", id, "WHICH_MAP")  
        for (new c = 0; c < g_voteSelectedNum[id] ; ++c) {
          len += format(menuBody[len],511,"%d. %s^n", c + 1 , g_mapName[  g_voteSelected[id][ c ]  ] )
          keys |= (1<<c)
        }
        keys |= (1<<8)
        len += format(menuBody[len],511,"^n9. None^n")
      }
      else {
        len = format(menuBody,511, g_coloredMenus ? "\y%L^n%s?^n\w^n1. %L^n2. %L^n"
          : "%L^n%s?^n^n1. %L^n2. %L^n", id, "CHANGE_MAP_TO", g_mapName[ g_voteSelected[id][ 0 ] ],
          id, "YES", id, "NO")
        keys = MENU_KEY_1|MENU_KEY_2
      }

      new menuName[64]
      format(menuName,63,"%L","en","WHICH_MAP")

      for (new b = 0; b < pnum; ++b)
        if ( players[b] != id )
            show_menu(players[b],keys,menuBody, iVoteTime, menuName)

      format(menuBody[len],511,"^n0. %L",id,"CANC_VOTE")
      keys |= MENU_KEY_0
      show_menu(id,keys,menuBody, iVoteTime, menuName)
      
      new authid[32],name[32]
      get_user_authid(id,authid,31)
      get_user_name(id,name,31)

      switch (get_cvar_num("amx_show_activity")) {
        case 2: client_print(0,print_chat,"%L",LANG_PLAYER,"ADMIN_V_MAP_2",name)
        case 1: client_print(0,print_chat,"%L",LANG_PLAYER,"ADMIN_V_MAP_1")
      }
      
      log_amx("Vote: ^"%s<%d><%s><>^" vote maps (map#1 ^"%s^") (map#2 ^"%s^") (map#3 ^"%s^") (map#4 ^"%s^")",
        name,get_user_userid(id),authid,
        g_voteSelectedNum[id] > 0 ? g_mapName[ g_voteSelected[id][ 0 ] ] : "" ,
        g_voteSelectedNum[id] > 1 ? g_mapName[ g_voteSelected[id][ 1 ] ] : "" ,
        g_voteSelectedNum[id] > 2 ? g_mapName[ g_voteSelected[id][ 2 ] ] : "",
        g_voteSelectedNum[id] > 3 ? g_mapName[ g_voteSelected[id][ 3 ] ] : "")
    }
    case 8: displayVoteMapsMenu(id,++g_menuPosition[id])
    case 9: displayVoteMapsMenu(id,--g_menuPosition[id])
    default: {
      g_voteSelected[id][  g_voteSelectedNum[id]++  ] = g_menuPosition[id] * 7 + key

      displayVoteMapsMenu(id,g_menuPosition[id])
    }
  }
  return PLUGIN_HANDLED
}


public actionMapsMenu(id,key) {
  switch (key) {
    case 8: displayMapsMenu(id,++g_menuPosition[id])
    case 9: displayMapsMenu(id,--g_menuPosition[id])
    default: 
    {
      new a = g_menuPosition[id] * 8 + key
      
      message_begin(MSG_ALL, SVC_INTERMISSION)
      message_end()

      new authid[32],name[32]
      get_user_authid(id,authid,31)
      get_user_name(id,name,31)

      switch (get_cvar_num("amx_show_activity")) {
        case 2: client_print(0,print_chat,"%L",LANG_PLAYER,"ADMIN_CHANGEL_2",name,g_mapName[  a  ])
        case 1: client_print(0,print_chat,"%L",LANG_PLAYER,"ADMIN_CHANGEL_2",g_mapName[  a  ])
      }
      
      log_amx("Cmd: ^"%s<%d><%s><>^" changelevel ^"%s^"",
        name,get_user_userid(id),authid, g_mapName[  a  ] ) 
            
      set_task(2.0,"delayedChange",0, g_mapName[  a  ] , strlen(g_mapName[  a  ]) + 1 )
      
      /* displayMapsMenu(id,g_menuPosition[id]) */
    }
  }
  return PLUGIN_HANDLED
}

displayMapsMenu(id,pos) {
  if (pos < 0)
    return

  new menuBody[512]
  new start = pos * 8 
  new b = 0

  if (start >= g_mapNums)
    start = pos = g_menuPosition[id] = 0

  new len = format(menuBody,511, g_coloredMenus ? 
    "\y%L\R%d/%d^n\w^n" : "%L %d/%d^n^n",
    id,"CHANGLE_MENU",pos+1,(  g_mapNums / 8 + (( g_mapNums % 8) ? 1 : 0 )) )

  new end = start + 8
  new keys = MENU_KEY_0

  if (end > g_mapNums)
    end = g_mapNums

  for (new a = start; a < end; ++a) {   
    keys |= (1<<b)
    len += format(menuBody[len],511-len,"%d. %s^n",++b,g_mapName[ a ])
  }

  if (end != g_mapNums) {
    format(menuBody[len],511-len,"^n9. %L...^n0. %L", id, "MORE", id, pos ? "BACK" : "EXIT")
    keys |= MENU_KEY_9
  }
  else format(menuBody[len],511-len,"^n0. %L", id, pos ? "BACK" : "EXIT")

  new menuName[64]
  format(menuName,63,"%L","en","CHANGLE_MENU")

  show_menu(id,keys,menuBody,-1,menuName)
}

load_settings(filename[]) {
  if (!file_exists(filename)) 
    return 0

  new text[256]
  new a , pos = 0
  
  while ( g_mapNums < MAX_MAPS && read_file(filename,pos++,text,255,a) ) {
    if ( text[0] == ';' ) continue

    if ( parse(text,g_mapName[g_mapNums],31) < 1 ) continue

    if ( !is_map_valid( g_mapName[g_mapNums] ) ) continue

    g_mapNums++
  }

  return 1
}