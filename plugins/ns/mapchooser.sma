// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Nextmap Chooser Plugin
//

#include <amxmodx>
#include <amxmisc>

#define MAX_MAPS    128
#define SELECTMAPS  5

new g_mapName[MAX_MAPS][32]
new g_mapNums

new g_nextName[SELECTMAPS]
new g_voteCount[SELECTMAPS+2]
new g_mapVoteNum
new g_lastMap[32]

new bool:g_selected = false

public plugin_init()
{
  register_plugin("Nextmap Chooser",AMXX_VERSION_STR,"AMXX Dev Team")
  register_menucmd(register_menuid("AMX Choose nextmap:"),(-1^(-1<<(SELECTMAPS+2))),"countVote")
  register_cvar("amx_extendmap_max","90")
  register_cvar("amx_extendmap_step","15")

  get_localinfo("lastMap",g_lastMap,charsmax(g_lastMap))
  set_localinfo("lastMap","")

  new maps_ini_file[64];
  get_configsdir(maps_ini_file, charsmax(maps_ini_file));
  format(maps_ini_file, charsmax(maps_ini_file), "%s/maps.ini", maps_ini_file);
  if ( loadSettings(maps_ini_file) )
    set_task(15.0,"voteNextmap",987456,"",0,"b")
}

public checkVotes(){
   new b = 0
   for(new a = 0; a < g_mapVoteNum; ++a)
      if (g_voteCount[b] < g_voteCount[a])
         b = a
   if ( g_voteCount[SELECTMAPS] > g_voteCount[b] ) {
      new mapname[32]
      get_mapname(mapname,charsmax(mapname))
      new Float:steptime = get_cvar_float("amx_extendmap_step")
      set_cvar_float("mp_timelimit", get_cvar_float("mp_timelimit") + steptime )
      client_print(0,print_chat,"Choosing finished. Current map will be extended to next %.0f minutes", steptime )
      log_amx("Vote: Voting for the nextmap finished. Map %s will be extended to next %.0f minutes",
         mapname , steptime )
      return
   }
   if ( g_voteCount[b] && g_voteCount[SELECTMAPS+1] <= g_voteCount[b] )
      set_cvar_string("amx_nextmap", g_mapName[g_nextName[b]] )
   new smap[32]
   get_cvar_string("amx_nextmap",smap,charsmax(smap))
   client_print(0,print_chat,"Choosing finished. The nextmap will be %s", smap )
   log_amx("Vote: Voting for the nextmap finished. The nextmap will be %s", smap)
}

public countVote(id,key){
   if ( get_cvar_float("amx_vote_answers") ) {
      new name[MAX_NAME_LENGTH]
      get_user_name(id,name,charsmax(name))
      if ( key == SELECTMAPS )
         client_print(0,print_chat,"%s chose map extending", name )
      else if ( key < SELECTMAPS )
         client_print(0,print_chat,"%s chose %s", name, g_mapName[g_nextName[key]] )
   }
   ++g_voteCount[key]
   return PLUGIN_HANDLED
}

bool:isInMenu(id){
   for(new a=0; a<g_mapVoteNum; ++a)
      if (id==g_nextName[a])
         return true
   return false
}

public voteNextmap(){
  new timeleft = get_timeleft()
  if (timeleft<1||timeleft>129){
      g_selected = false
      return
  }

  if (g_selected)
    return
  g_selected = true
  new menu[512], a, mkeys = (1<<SELECTMAPS+1)
  new pos = copy(menu,charsmax(menu),"AMX Choose nextmap:^n^n")
  new dmax = (g_mapNums > SELECTMAPS) ? SELECTMAPS : g_mapNums
  for(g_mapVoteNum = 0;g_mapVoteNum<dmax;++g_mapVoteNum){
    a=random_num(0,g_mapNums-1)
    while( isInMenu(a) )
      if (++a >= g_mapNums) a = 0
    g_nextName[g_mapVoteNum] = a
    pos += format(menu[pos], charsmax(menu) - pos, "%d. %s^n", g_mapVoteNum + 1, g_mapName[a])
    mkeys |= (1<<g_mapVoteNum)
    g_voteCount[g_mapVoteNum] = 0
  }
  menu[pos++]='^n'
  g_voteCount[SELECTMAPS] = 0
  g_voteCount[SELECTMAPS+1] = 0
  new mapname[32]
  get_mapname(mapname,charsmax(mapname))

  if ( get_cvar_float("mp_timelimit") < get_cvar_float("amx_extendmap_max") ){
    pos += format(menu[pos], charsmax(menu) - pos, "%d. Extend map %s^n", SELECTMAPS+1, mapname)
    mkeys |= (1<<SELECTMAPS)
  }

  format(menu[pos],charsmax(menu),"%d. None",SELECTMAPS+2)
  show_menu(0,mkeys,menu,15)
  set_task(15.0,"checkVotes")
  client_print(0,print_chat,"It's time to choose the nextmap...")
  client_cmd(0,"spk Gman/Gman_Choose2")
  log_amx("Vote: Voting for the nextmap started")
}
stock bool:ValidMap(mapname[])
{
	if ( is_map_valid(mapname) )
	{
		return true;
	}
	// If the is_map_valid check failed, check the end of the string
	new len = strlen(mapname) - 4;
	
	// The mapname was too short to possibly house the .bsp extension
	if (len < 0)
	{
		return false;
	}
	if ( equali(mapname[len], ".bsp") )
	{
		// If the ending was .bsp, then cut it off.
		// the string is byref'ed, so this copies back to the loaded text.
		mapname[len] = '^0';
		
		// recheck
		if ( is_map_valid(mapname) )
		{
			return true;
		}
	}
	
	return false;
}

loadSettings(filename[])
{
  if (!file_exists(filename)) return 0

  new szText[32]
  new a, pos = 0
  new currentMap[32]
  get_mapname(currentMap,charsmax(currentMap))

  while ( (g_mapNums < MAX_MAPS) && read_file(filename,pos++,szText,charsmax(szText),a) )
  {
    if ( szText[0] != ';'
    &&  parse(szText, g_mapName[g_mapNums] ,charsmax(g_mapName[]) )
    &&  ValidMap( g_mapName[g_mapNums] ) 
    &&  !equali( g_mapName[g_mapNums] ,g_lastMap)
    &&  !equali( g_mapName[g_mapNums] ,currentMap) )
      ++g_mapNums
  }

  return g_mapNums
}

public plugin_end(){
  new current_map[32]
  get_mapname(current_map,charsmax(current_map) )
  set_localinfo("lastMap",current_map)
}
