/* AMX Mod X
*   Pause Plugins Plugin
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

// Uncomment if you want to have two new commands
// amx_off - pause plugins not marked as unpauseable
// amx_on - enable plugins not marked as unpauseable
#define DIRECT_ONOFF

#define MAX_SYSTEM 32
 
new g_menuPos[33]
new g_fileToSave[64]
new g_cstrikeRunning
new g_Modified
new g_couldntFind[] = "Couldn't find a plugin matching ^"%s^""
new g_pluginMatch[] = "Plugin matching ^"%s^" %s"
new g_addCmd[] = "amx_pausecfg add ^"%s^""
new g_system[MAX_SYSTEM]
new g_systemNum

public plugin_init(){
  register_plugin("Pause Plugins","0.1","AMXX Dev Team")
  register_concmd("amx_pausecfg","cmdPlugin",ADMIN_CFG,"- list commands for pause/unpause managment")
  register_clcmd("amx_pausecfgmenu","cmdMenu",ADMIN_CFG,"- pause/unpause plugins with menu")
#if defined DIRECT_ONOFF
  register_concmd("amx_off","cmdOFF",ADMIN_CFG,"- pauses some plugins")
  register_concmd("amx_on","cmdON",ADMIN_CFG,"- unpauses some plugins")
#endif
  register_menucmd(register_menuid("Pause/Unpause Plugins"),1023,"actionMenu")
  g_cstrikeRunning = is_running("cstrike")
  return PLUGIN_CONTINUE
}

#if defined DIRECT_ONOFF

public cmdOFF(id,level,cid){
  if (cmd_access(id,level,cid,1))
    pausePlugins(id)
  return PLUGIN_HANDLED
}

public cmdON(id,level,cid){
  if (cmd_access(id,level,cid,1))
    unpausePlugins(id)
  return PLUGIN_HANDLED
}

#endif

public plugin_cfg() {
  build_path( g_fileToSave , 63 , "$basedir/pausecfg.ini" )  
  loadSettings(g_fileToSave)
  // Put here titles of plugins which you don't want to pause
  server_cmd(g_addCmd , "Pause Plugins" )
  server_cmd(g_addCmd , "Admin Commands" )
  server_cmd(g_addCmd , "TimeLeft" )
  server_cmd(g_addCmd , "Slots Reservation" )
  server_cmd(g_addCmd , "Admin Chat" )
  server_cmd(g_addCmd , "NextMap" )
  server_cmd(g_addCmd , "Admin Help" )
  server_cmd(g_addCmd , "Admin Base" )
  server_cmd(g_addCmd , "Admin Votes" )  
  server_cmd(g_addCmd , "Welcome Message" )
  server_cmd(g_addCmd , "Stats Configuration" )
  server_cmd(g_addCmd , "Commands Menu" )
  server_cmd(g_addCmd , "Maps Menu" )
  server_cmd(g_addCmd , "Menus Front-End" )
  server_cmd(g_addCmd , "Admin Base for MySQL" )
  server_cmd(g_addCmd , "Players Menu" )
  server_cmd(g_addCmd , "Teleport Menu" )
}

public actionMenu(id,key){
  switch(key){
  case 6:{
      if (file_exists(g_fileToSave)){
        delete_file(g_fileToSave)
        client_print(id,print_chat,"* Configuration file cleared. Reload the map if needed")
      }
      else
        client_print(id,print_chat,"* Configuration was already cleared!")
      displayMenu(id,g_menuPos[id])
  }
  case 7:{
      if (saveSettings(g_fileToSave)){
        g_Modified = 0
        client_print(id,print_chat,"* Configuration saved successfully")
      }
      else
        client_print(id,print_chat,"* Configuration saving failed!!!")
      displayMenu(id,g_menuPos[id])
    }
  case 8: displayMenu(id,++g_menuPos[id])
  case 9: displayMenu(id,--g_menuPos[id])
  default:{
      new option = g_menuPos[id] * 6 + key
      new file[32],status[2]
      get_plugin(option,file,31,status,0,status,0,status,0,status,1)
      switch( status[0] ) {
        case 'r': pause("ac",file)
        case 'p': {
          g_Modified = 1
          pause("dc",file)  
        }
        case 's': {
          g_Modified = 1
          unpause("ac",file)
        }
      }
      displayMenu(id,g_menuPos[id])
    }
  }
  return PLUGIN_HANDLED
}

getStatus( code, arg[], iarg ){
  switch(code){
    case 'r': copy( arg, iarg , "ON" )
    case 's': copy( arg, iarg , "STOPPED" )
    case 'p': copy( arg, iarg , "OFF" )
    case 'b': copy( arg, iarg , "ERROR" )
    default: copy( arg, iarg , "LOCKED" )
  }
}

isSystem( id ){
  for( new a = 0; a < g_systemNum; ++a)
    if ( g_system[ a ] == id )
      return 1
  return 0
}

displayMenu(id, pos){
  if (pos < 0) return
  new filename[32],title[32],status[8]
  new datanum = get_pluginsnum()
  new menu_body[512], start = pos * 6, k = 0
  if (start >= datanum) start = pos = g_menuPos[id] = 0
  new len = format(menu_body,511,
  g_cstrikeRunning ? "\yPause/Unpause Plugins\R%d/%d^n\w^n" : "Pause/Unpause Plugins %d/%d^n^n" ,
      pos + 1,((datanum/6)+((datanum%6)?1:0)))
  new end = start + 6, keys = (1<<9)|(1<<7)|(1<<6)
  if (end > datanum) end = datanum
  for(new a = start; a < end; ++a){
    get_plugin(a,filename,31,title,31,status,0,status,0,status,1)
    getStatus( status[0] , status , 7  )
    if ( isSystem( a ) || (status[0]!='O'&&status[0]!='S')) {
      if (g_cstrikeRunning){
        len += format(menu_body[len],511-len, "\d%d. %s\R%s^n\w",++k, title, status )     
      }
      else{
        ++k
        len += format(menu_body[len],511-len, "#. %s %s^n", title, status )
      }
    }
    else{
      keys |= (1<<k)
      len += format(menu_body[len],511-len,g_cstrikeRunning ? "%d. %s\y\R%s^n\w" : "%d. %s %s^n",++k,title, status )
    }
  }
  len += format(menu_body[len],511-len,"^n7. Clear file with stopped^n")  
  len += format(menu_body[len],511-len,g_cstrikeRunning ? "8. Save stopped \y\R%s^n\w"
    : "8. Save stopped %s^n" ,g_Modified ? "*" : "")
  if (end != datanum){
    format(menu_body[len],511-len,"^n9. More...^n0. %s", pos ? "Back" : "Exit")
    keys |= (1<<8)
  }
  else format(menu_body[len],511-len,"^n0. %s", pos ? "Back" : "Exit")
  show_menu(id,keys,menu_body)
}

public cmdMenu(id,level,cid){
  if (cmd_access(id,level,cid,1))
    displayMenu(id,g_menuPos[id] = 0)
  return PLUGIN_HANDLED
}

pausePlugins(id){
  new filename[32],title[32],status[2]
  new count = 0, imax = get_pluginsnum()
  for (new a=0;a<imax;++a){
    get_plugin(a,filename,31,title,31,status,0,status,0,status,1)
    if ( !isSystem( a ) && status[0]=='r' && pause("ac",filename) ) {
      //console_print(id,"Pausing %s (file ^"%s^")",title,filename)
      ++count
    }
  }
  console_print(id,"Paused %d plugin%s",count,(count==1)?"":"s")
}

unpausePlugins(id){
  new filename[32],title[32],status[2]
  new count = 0, imax = get_pluginsnum()
  for (new a=0;a<imax;++a){
    get_plugin(a,filename,31,title,31,status,0,status,0,status,1)
    if ( !isSystem( a ) && status[0]=='p' && unpause("ac",filename) ) {
      //console_print(id,"Unpausing %s (file ^"%s^")",title,filename)
      ++count
    }
  }
  console_print(id,"Unpaused %d plugin%s",count,(count==1)?"":"s")
}

findPluginByFile(arg[32],&len){
  new name[32],title[32],status[2]
  new inum  = get_pluginsnum()
  for(new a = 0; a < inum; ++a){
    get_plugin(a,name,31,title,31,status,0,status,0,status,1)
    if ( equali(name,arg,len) && (status[0]=='r'||status[0]=='p'||status[0]=='s') ){
      len = copy(arg,31,name)
      return a
    }
  }
  return -1
}

findPluginByTitle(name[],file[],len){
  new title[32],status[2]
  new inum  = get_pluginsnum()  
  for(new a = 0; a < inum; ++a){
    get_plugin(a,file,len,title,31,status,0,status,0,status,1)
    if ( equali( title , name ) )
      return a
  }
  return -1
}

public cmdPlugin(id,level,cid){
  if (!cmd_access(id,level,cid,1))
    return PLUGIN_HANDLED
  new cmds[32]
  read_argv(1,cmds,31)
  if ( equal(cmds, "add" ) && read_argc() > 2  ) {
    read_argv(2, cmds ,31)
    new file[2]
    if ( (g_system[ g_systemNum ] = findPluginByTitle( cmds , file , 0 )) != -1 ) {
      if ( g_systemNum < MAX_SYSTEM )
        g_systemNum++
      else
        console_print( id , "Can't mark more plugins as unpauseable!" )
    }
  }
  else if ( equal(cmds, "off" ) ){
    pausePlugins(id)
  }
  else if ( equal(cmds, "on" ) ){
    unpausePlugins(id)
  }
  else if ( equal(cmds, "save" ) ){
    if (saveSettings(g_fileToSave)){
      g_Modified = 0
      console_print(id,"Configuration saved successfully")
    }
    else
      console_print(id,"Configuration saving failed!!!")
  }
  else if ( equal(cmds, "clear" ) ) {
    if (file_exists(g_fileToSave)){
      delete_file(g_fileToSave)
      console_print(id,"Configuration file cleared. Reload the map if needed")
    }
    else
      console_print(id,"Configuration was already cleared!")
  }
  else if ( equal(cmds, "pause" )  )  {
    new arg[32], a ,len = read_argv(2,arg,31)
    if ( len && ((a = findPluginByFile(arg,len)) != -1) && !isSystem( a ) && pause("ac",arg) )
      console_print(id,g_pluginMatch,arg , "paused")
    else console_print(id,g_couldntFind,arg)
  }
  else if ( equal(cmds, "enable" )  ) {
    new arg[32], a , len = read_argv(2,arg,31)
    if ( len && (a = findPluginByFile(arg,len)) != -1 && !isSystem( a ) && unpause("ac",arg) )
      console_print(id,g_pluginMatch,arg , "unpaused")
    else console_print(id,g_couldntFind,arg)
  }
  else if ( equal(cmds, "stop" )  ) {
    new arg[32], a, len = read_argv(2,arg,31)
    if ( len && (a = findPluginByFile(arg,len)) != -1 && !isSystem( a ) && pause("dc",arg)){
      g_Modified = 1
      console_print(id,g_pluginMatch,arg , "stopped")
    }
    else console_print(id,g_couldntFind,arg)
  }
  else if ( equal(cmds, "list" )  ) {
  	new arg1[8], running = 0
  	new start = read_argv(2,arg1,7) ? strtonum(arg1) : 1
  	if (--start < 0) start = 0
  	new plgnum = get_pluginsnum()
  	if (start >= plgnum) start = plgnum - 1
  	console_print(id,"^n----- Pause Plugins: Loaded plugins -----")
  	console_print(id, "       %-18.17s %-8.7s %-17.16s %-16.15s %-9.8s","name","version","author","file","status")  	
  	new plugin[32],title[32],version[16],author[32],status[16]
  	new end = start + 10
  	if (end > plgnum) end = plgnum
  	for (new a = start; a < end; ++a){
    	get_plugin(a,plugin,31,title,31,version,15,author,31,status,15)
    	if (status[0] == 'r') ++running
    	console_print(id, " [%3d] %-18.17s %-8.7s %-17.16s %-16.15s %-9.8s",a+1,title,version,author,plugin, status )
  	
  	}
  	console_print(id,"----- Entries %d - %d of %d (%d running) -----",start+1,end,plgnum,running)
  	if (end < plgnum)
     	console_print(id,"----- Use 'amx_pausecfg list %d' for more -----",end+1)
  	else
     	console_print(id,"----- Use 'amx_pausecfg list 1' for begin -----")
  }
  else {
    console_print(id,"Usage:  amx_pausecfg <command> [name]")
    console_print(id,"Commands:")
    console_print(id,"^toff - pauses all plugins not in the list")   
    console_print(id,"^ton - unpauses all plugins")
    console_print(id,"^tstop <file> - stops a plugin")
    console_print(id,"^tpause <file> - pauses a plugin")
    console_print(id,"^tenable <file> - enables a plugin")
    console_print(id,"^tsave - saves a list of stopped plugins")     
    console_print(id,"^tclear - clears a list of stopped plugins")
    console_print(id,"^tlist [id] - lists plugins")
    console_print(id,"^tadd <title> - marks a plugin as unpauseable")
  }
  return PLUGIN_HANDLED
}

saveSettings(filename[]){
  if (file_exists(filename))
    delete_file(filename)
  new text[256], file[32],title[32],status[2]
  new inum  = get_pluginsnum()
  if (!write_file(filename,";Generated by Pause Plugins Plugin. Do not modify!^n;Title Filename"))
    return 0
  for(new a = 0; a < inum; ++a){
    get_plugin(a,file,31,title,31,status,0,status,0,status,1)
    if ( status[0] == 's' ){
      format(text,255,"^"%s^" ;%s",title,file)
      write_file(filename,text)
    }
  }
  return 1
}

loadSettings(filename[]){
  if (!file_exists(filename)) return 0
  new name[256], file[32], i, pos = 0
  while (read_file(filename,pos++,name,255,i)){
    if ( name[0]!= ';' && parse(name,name,31) &&
      (i = findPluginByTitle( name , file , 31 ) != -1) )
      pause("dc", file )
  }
  return 1
}
