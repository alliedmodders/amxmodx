/* AMX Mod script.
 *
 * (c) 2003, OLO
 * This file is provided as is (no warranties).
 *
 * Admin commads:
 * amx_pausemenu - displays menu by which you can pause, unpause and stop plugins
 * amx_plugin - displays help for all commands for that plugin
 *
 * WARNING: Stopped plugins won't work properly after activation 
 * (without mapchange) due to unactive status during plugins initialization
 * and at players connections. For proper activation clear the file with
 * stopped plugins (option #7 in menu) or unstop selected one then change the map.
 */
 
#include <amxmod>
#include <amxmisc>


// Uncomment if you want to have two new commands
// amx_off - pause plugins not registered in the unpauseable list
// amx_on - enable all plugins not registered in the unpauseable list
//#define DIRECT_ONOFF
 
#define MAX_PLGDATA 64
#define MAX_PLUGINS 192

enum {
  PLG_ERROR,
  PLG_ON,
  PLG_OFF,
  PLG_STOPPED
}

new g_pluginList[MAX_PLGDATA][32]
new g_pluginListNum
new g_menuPos[33]
new g_dontPause[MAX_PLGDATA]
new g_dontPauseNum
new g_pluginStatus[MAX_PLUGINS]
new bool:g_Modified

new g_fileToSave[64]
new g_cstrikeRunning

public plugin_init(){
  register_plugin("Pause Plugins","0.9","default")
  
#if defined DIRECT_ONOFF  
  register_concmd("amx_off","cmdOFF",ADMIN_CFG,"- pause some plugins")
  register_concmd("amx_on","cmdON",ADMIN_CFG,"- unpause some plugins")
#endif  
  
  register_concmd("amx_plugin","cmdPause",ADMIN_CFG,"- list cmds. for pause/unpause managment")
  register_clcmd("amx_pausemenu","cmdMenu",ADMIN_CFG,"- pause or unpause plugins via menu")
  register_menucmd(register_menuid("Pause/Unpause Plugins"),1023,"actionMenu")

  get_localinfo( "amx_basedir", g_fileToSave , 31 )  
  format( g_fileToSave , 63, "%s/ppause.ini" , g_fileToSave )
  
  loadSettings(g_fileToSave)    
    
  new mod_name[32]
  get_modname(mod_name,31)
  g_cstrikeRunning = equal(mod_name,"cstrike")
}

new g_addCmd[] = "amx_plugin add ^"%s^""

public plugin_cfg() {
  /*  Put here titles of plugins which you don't want to pause. */
  server_cmd(g_addCmd , "Pause Plugins" )
  server_cmd(g_addCmd , "Admin Commands" )
  server_cmd(g_addCmd , "TimeLeft" )
  server_cmd(g_addCmd , "Slots Reservation" )
  server_cmd(g_addCmd , "Admin Chat" )
  server_cmd(g_addCmd , "NextMap" )
  server_cmd(g_addCmd , "Admin Menu" )
  server_cmd(g_addCmd , "Admin Help" )
  server_cmd(g_addCmd , "Admin Base" )
  server_cmd(g_addCmd , "Welcome Message" )
  server_cmd(g_addCmd , "Stats Settings" )
}

public actionMenu(id,key){
  switch(key){
  case 6:{
      if (file_exists(g_fileToSave)){
        delete_file(g_fileToSave)
        client_print(id,print_chat,"* Configuration file cleared")
      }
      else
        client_print(id,print_chat,"* Configuration was already cleared!")
      displayMenu(id,g_menuPos[id])
  }
  case 7:{
      if (saveSettings(g_fileToSave)){
        g_Modified = false
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
      new filename[32],title[32],version[2],author[2],status[2]
      get_plugin(option,filename,31,title,31,version,0,author,0,status,1)
      if (status[0]=='r'){
        pause("ac",filename)
        g_pluginStatus[option]=PLG_OFF
      }
      else if ( g_pluginStatus[option]!=PLG_STOPPED && status[0]=='p'  ){
        g_pluginStatus[option]=PLG_STOPPED
        g_Modified = true
      }
      else {
        unpause("ac",filename)
        g_pluginStatus[option]=PLG_ON
      }
      displayMenu(id,g_menuPos[id])
    }
  }
  return PLUGIN_HANDLED
}

displayMenu(id, pos){
  if (pos < 0) return
  new filename[32],title[32],version[2],author[2],status[2]
  new datanum = get_pluginsnum()
  new menu_body[512], start = pos * 6, k = 0
  if (start >= datanum) start = pos = g_menuPos[id] = 0
  new len = format(menu_body,511,
  g_cstrikeRunning ? "\yPause/Unpause Plugins\R%d/%d^n\w^n" : "Pause/Unpause Plugins %d/%d^n^n" ,
      pos + 1,((datanum/6)+((datanum%6)?1:0)))
  new end = start + 6, keys = (1<<9)|(1<<7)|(1<<6)
  if (end > datanum) end = datanum
  for(new a = start; a < end; ++a){
    get_plugin(a,filename,31,title,31,version,0,author,0,status,1)
    if (dontPause(a)||(status[0]!='r'&&status[0]!='p')) {
      if (g_cstrikeRunning){
        len += format(menu_body[len],511-len, "\d%d. %s\R%s^n\w",++k,
          title, ( status[0]=='r' ) ? "ON" : ( ( status[0]=='p' ) ? "OFF" : "ERROR" ) )     
      }
      else{
        ++k
        len += format(menu_body[len],511-len, "#. %s %s^n",
          title, ( status[0]=='r' ) ? "ON" : ( ( status[0]=='p' ) ? "OFF" : "ERROR" ) )
      }
    }
    else{
      keys |= (1<<k)
      len += format(menu_body[len],511-len,g_cstrikeRunning ? "%d. %s\y\R%s^n\w" : "%d. %s %s^n",++k,
        title, ( status[0]=='r' ) ? "ON" : ((g_pluginStatus[a]==PLG_STOPPED)?"STOPPED":"OFF"))
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
  if (!cmd_access(id,level,cid,1))
    return PLUGIN_HANDLED
  if (g_dontPauseNum != g_pluginListNum) chkStatus()
  displayMenu(id,g_menuPos[id] = 0)
  return PLUGIN_HANDLED
}

pasueALL(id){
  if (g_dontPauseNum != g_pluginListNum) chkStatus()
  new filename[32],title[32],version[2],author[2],status[2]
  new count = 0, imax = get_pluginsnum()
  for (new a=0;a<imax;++a){
    get_plugin(a,filename,31,title,31,version,0,author,0,status,1)
    if (dontPause(a)||status[0]!='r') continue
    pause("ac",filename)
    ++count
    console_print(id,"Pausing %s (file ^"%s^")",title,filename)
  }
  console_print(id,"Paused %d plugins",count)
}

unpauseALL(id){
  if (g_dontPauseNum != g_pluginListNum) chkStatus()
  new filename[32],title[32],version[2],author[2],status[2]
  new count = 0, imax = get_pluginsnum()
  for (new a=0;a<imax;++a){
    get_plugin(a,filename,31,title,31,version,0,author,0,status,1)
    if (dontPause(a)||status[0]!='p') continue
    unpause("ac",filename)
    ++count
    console_print(id,"Unpausing %s (file ^"%s^")",title,filename)
  }
  console_print(id,"Unpaused %d plugins",count) 
}

#if defined DIRECT_ONOFF

public cmdOFF(id,level,cid){
  if (cmd_access(id,level,cid,1))
    pasueALL(id)
  return PLUGIN_HANDLED
}

public cmdON(id,level,cid){
  if (cmd_access(id,level,cid,1))
    unpauseALL(id)
  return PLUGIN_HANDLED
}

#endif

findPlugin(argument[32],&len){
  new plugin[32],title[32],version[2],author[2],status[2]
  new inum  = get_pluginsnum()
  for(new a = 0; a < inum; ++a){
    get_plugin(a,plugin,31,title,31,version,0,author,0,status,1)
    if ( equali(plugin,argument,len) && (status[0]=='r'||status[0]=='p') ){
      len = copy(argument,31,plugin)
      return a
    }
  }
  return -1
}

public cmdPause(id,level,cid){
  if (!cmd_access(id,level,cid,1))
    return PLUGIN_HANDLED
  new cmds[32]
  read_argv(1,cmds,31)
  if ( equal(cmds, "off" ) ){
    pasueALL(id)
  }
  else if ( equal(cmds, "on" ) ){
    unpauseALL(id)
  }
  else if ( equal(cmds, "save" ) ){
    if (saveSettings(g_fileToSave)){
      g_Modified = false
      console_print(id,"Configuration saved successfully")
    }
    else
      console_print(id,"Configuration saving failed!!!")
  }
  else if ( equal(cmds, "clear" ) ) {
    if (file_exists(g_fileToSave)){
      delete_file(g_fileToSave)
      console_print(id,"Configuration file cleared")
    }
    else
      console_print(id,"Configuration was already cleared!")
  }
  else if ( equal(cmds, "pause" )  )  {
    new arg[32], len, a
    if (  (len = read_argv(2,arg,31)) != 0 ){
      if ( (a = findPlugin(arg,len)) != -1){
        if (pause("ac",arg)) g_pluginStatus[a] = PLG_OFF
        console_print(id,"Plugin matching ^"%s^" paused",arg)
      }
    }
    if (!len || a==-1)  console_print(id,"Couldn't find plugin matching ^"%s^"",arg)
  }
  else if ( equal(cmds, "unpause" )  ) {
    new arg[32], len, a
    if (  (len = read_argv(2,arg,31)) != 0 ){
      if ( (a = findPlugin(arg,len)) != -1){
        if (unpause("ac",arg)) g_pluginStatus[a] = PLG_ON
        console_print(id,"Plugin matching ^"%s^" unpaused",arg)
      }
    }
    if (!len || a==-1)  console_print(id,"Couldn't find plugin matching ^"%s^"",arg)
  }
  else if ( equal(cmds, "stop" )  ) {
    new arg[32], len, a
    if (  (len = read_argv(2,arg,31)) != 0 ){
      if ( (a = findPlugin(arg,len)) != -1){
        pause("ac",arg)
        g_pluginStatus[a] = PLG_STOPPED
        g_Modified = true
        console_print(id,"Plugin matching ^"%s^" stopped",arg)
      }
    }
    if (!len || a==-1)  console_print(id,"Couldn't find plugin matching ^"%s^"",arg)
  }
  else if ( equal(cmds, "list" )  ) {
    new plugin[32],title[32],version[16],author[32],status[16]
    new inum  = get_pluginsnum()
    console_print(id, "Currently loaded plugins:")
    console_print(id, "       name               version  author            file             status")
    new running = 0, plugins = 0
    for(new a = 0; a < inum; ++a){
      plugins++
      get_plugin(a,plugin,31,title,31,version,15,author,31,status,15)
      if (status[0] == 'r') running++
      console_print(id, " [%3d] %-18.17s %-8.7s %-17.16s %-16.15s %-9.8s",plugins,
        title,version,author,plugin, (g_pluginStatus[a] == PLG_STOPPED) ? "stopped" : status )
    }
    console_print(id, "%d plugins, %d running",plugins,running)
  }
  else if ( equal(cmds, "add" ) && read_argc() > 2  ) {
    if ( g_pluginListNum < MAX_PLGDATA )
      read_argv(2,g_pluginList[g_pluginListNum++],31)
    else
      console_print(id, "Can't add more plugins to the unpauseable list, limit reached!")
  }
  else {
    console_print(id,"Usage:  amx_plugin <command> [name]")
    console_print(id,"Commands:")
    console_print(id,"^toff - pause all plugins not in the list")   
    console_print(id,"^ton - unpause all plugins")
    console_print(id,"^tstop <file> - stop plugin")
    console_print(id,"^tpause <file> - pause plugin")
    console_print(id,"^tunpause <file> - unpause plugin")
    console_print(id,"^tsave - save list of stopped plugins")     
    console_print(id,"^tclear - clear list of stopped plugins")
    console_print(id,"^tlist - list plugins")
    console_print(id,"^tadd <title> - add plugin to the unpauseable plugins list")    
  }
  return PLUGIN_HANDLED
}   
    
chkStatus(){
  new filename[32],title[32],version[2],author[2],status[2]
  new imax = get_pluginsnum()
  for (new a=0;a<imax;++a){
    get_plugin(a,filename,31,title,31,version,0,author,0,status,1)
    if (status[0]=='p'){
      if (g_pluginStatus[a]!=PLG_STOPPED)g_pluginStatus[a] = PLG_OFF
    }
    else if (status[0]=='r')
      g_pluginStatus[a] = PLG_ON
    else
      g_pluginStatus[a] = PLG_ERROR  
    if (dontPausePre(title))
      g_dontPause[g_dontPauseNum++] = a
  }
}

bool:dontPause(myid) {
  for(new a=0;a<g_dontPauseNum;++a)
    if (g_dontPause[a]==myid)
      return true
  return false
}

bool:dontPausePre(name[]) {
  for(new a=0;a<g_pluginListNum;++a)
    if (equali(g_pluginList[a],name))
      return true
  return false
}

saveSettings(filename[]){
  if (file_exists(filename))
    delete_file(filename)
  new text[256], plugin[32],title[32],version[2],author[2],status[2]
  new inum  = get_pluginsnum()
  if (!write_file(filename,";Generated by Pause Plugins Plugin. Do not modify!^n;Filename Description"))
    return 0
  for(new a = 0; a < inum; ++a){
    if (g_pluginStatus[a]==PLG_STOPPED){
      get_plugin(a,plugin,31,title,31,version,0,author,0,status,1)
      format(text,255,"%s ;%s",plugin,title)
      write_file(filename,text)
    }
  }
  return 1
}

loadSettings(filename[]){
  if (!file_exists(filename)) return 0
  new text[256], len, pos = 0
  while (read_file(filename,pos++,text,255,len)){
    if ( text[0] == ';' ) continue // line is a comment
    parse(text,g_pluginList[g_pluginListNum++],31)
  }
  new plugin[32],title[32],version[2],author[2],status[2]
  new inum  = get_pluginsnum()
  for(new a = 0; a < inum; ++a){
    get_plugin(a,plugin,31,title,31,version,0,author,0,status,1)
    if (!dontPausePre(plugin)) continue
    pause("ac",plugin)
    g_pluginStatus[a] = PLG_STOPPED
  }
  g_pluginListNum = 0
  return 1
}