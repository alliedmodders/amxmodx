/* AMX Mod X
*   Stats Configuration Plugin
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

#define MAX_MENU_DATA 64

new g_menuData[MAX_MENU_DATA][32]
new g_menuDataVar[MAX_MENU_DATA][32]
new g_menuDataId[MAX_MENU_DATA]
new g_menuDataNum
new g_menuPosition[33]
new g_fileToSave[] = "addons/amxx/configs/stats.ini"
new bool:g_modified

public plugin_precache(){
  register_clcmd("amx_statscfgmenu","cmdCfgMenu",ADMIN_CFG,"- displays stats configuration menu")
  register_concmd("amx_statscfg","cmdCfg",ADMIN_CFG,"- displays help for stats configuration")  
}

public plugin_init() {
  register_plugin("Stats Configuration","0.1","AMXX Dev Team")
  register_menucmd(register_menuid("\yStats Configuration"),1023,"actionCfgMenu")
  loadSettings(g_fileToSave)
}

public cmdCfg( id,level,cid ){
  if (!cmd_access(id,level,cid,1))
    return PLUGIN_HANDLED
    
  new cmds[32]
  read_argv(1,cmds,31)
  new option = equali(cmds, "on" ) ? 1 : 0
  if ( !option ) option = equali(cmds, "off" ) ? 2 : 0
  if ( read_argc() > 2 && option ) {
    new var[32], enabled = 0
    read_argv( 2 , var , 31  )
    for( new a = 0; a < g_menuDataNum; ++a ) {
      if ( containi( g_menuDataVar[ a ] ,  var  ) != -1 ) {
        g_modified = true
        ++enabled
        if ( option == 1 ) {
          set_xvar_num( g_menuDataId[a] , 1  )
          console_print(id,"Enabled %s" , g_menuData[a] )
        }
        else {
          set_xvar_num( g_menuDataId[a] , 0  )
          console_print(id,"Disabled %s" , g_menuData[a] )
        }
      }
    }
    if ( enabled )
      console_print(id,"Total %d", enabled )
    else
      console_print(id,"Couldn't find option(s) with such variable (name ^"%s^")", var )
  }
  else if ( equali(cmds, "save" )  ) {
    if ( saveSettings( g_fileToSave ) ){
      g_modified = false
      console_print(id,"Stats configuration saved successfully")
    }
    else
      console_print(id,"Failed to save stats configuration!!!")  
  }
  else if ( equali(cmds, "load" )  ) {
    if ( loadSettings( g_fileToSave ) ){
      g_modified = false
      console_print(id,"Stats configuration loaded successfully")
    }
    else
      console_print(id,"Failed to load stats configuration!!!")
  }
  else if ( equali(cmds, "list" )  ) {
    new arg1[8]
    new start = read_argv(2,arg1,7) ? str_to_num(arg1) : 1
    if (--start < 0) start = 0
    if (start >= g_menuDataNum) start = g_menuDataNum - 1
    new end = start + 10
    if (end > g_menuDataNum) end = g_menuDataNum  
    console_print(id, "^n----- Stats Configuration: -----")
    console_print(id, "     %-29.28s   %-24.23s   %-9.8s","name","variable","status")
    if ( start != -1 ) { 
      for(new a = start; a < end; ++a){
        console_print(id, "%3d: %-29.28s   %-24.23s   %-9.8s",a + 1,
        g_menuData[a], g_menuDataVar[a], get_xvar_num( g_menuDataId[ a ] ) ? "ON" : "OFF")
      }
    }
    console_print(id,"----- Entries %i - %i of %i -----",start+1,end,g_menuDataNum)    
    if (end < g_menuDataNum)
      console_print(id,"----- Use 'amx_statscfg list %i' for more -----",end+1)
    else 
      console_print(id,"----- Use 'amx_statscfg list 1' for begin -----")
  }
  else if ( equali(cmds, "add" ) &&  read_argc() > 3 ) {
      if ( g_menuDataNum < MAX_MENU_DATA  ) {
        read_argv(2, g_menuData[g_menuDataNum] , 31 )
        read_argv(3, g_menuDataVar[g_menuDataNum] , 31 )
        g_menuDataId[g_menuDataNum] = get_xvar_id( g_menuDataVar[g_menuDataNum] )
        ++g_menuDataNum
      }
      else console_print(id, "Can't add stats to the list, limit reached!")
  }  
  else {
    console_print(id,"Usage:  amx_statscfg <command> [parameters] ...")
    console_print(id,"Commands:")
    console_print(id,"^ton <variable> - enable specified option")
    console_print(id,"^toff <variable> - disable specified option")
    console_print(id,"^tsave - save stats configuration")
    console_print(id,"^tload - load stats configuration")
    console_print(id,"^tlist [id] - list stats status")
    console_print(id,"^tadd <name> <variable> - add stats to the list")    
  }
  
  return PLUGIN_HANDLED
}

public cmdCfgMenu(id,level,cid){
  if (cmd_access(id,level,cid,1))
    displayCfgMenu(id,g_menuPosition[id] = 0)
  return PLUGIN_HANDLED
}

displayCfgMenu(id,pos){
  if (pos < 0) return
  new menu_body[512], start = pos * 7
  if (start >= g_menuDataNum) start = pos = g_menuPosition[id] = 0
  new len = format(menu_body,511,"\yStats Configuration\R%d/%d^n\w^n",
    pos + 1,((g_menuDataNum/7)+((g_menuDataNum%7)?1:0)))
  new end = start + 7, keys = (1<<9)|(1<<7), k = 0
  if (end > g_menuDataNum) end = g_menuDataNum
  for(new a = start; a < end; ++a){
    keys |= (1<<k)
    len += format(menu_body[len],511-len,"%d. %s\y\R%s^n\w",++k,
      g_menuData[a], get_xvar_num( g_menuDataId[ a ] )  ? "ON" : "OFF" )
  }
  if ( g_menuDataNum == 0 )
    len += format(menu_body[len],511-len,"\dStats plugins are not^ninstalled on this server^n\w")
  len += format(menu_body[len],511-len,"^n8. Save configuration\y\R%s^n\w",g_modified ? "*" : "")
  if (end != g_menuDataNum){
    format(menu_body[len],511-len,"^n9. More...^n0. %s", pos ? "Back" : "Exit")
    keys |= (1<<8)
  }
  else format(menu_body[len],511-len,"^n0. %s", pos ? "Back" : "Exit")
  show_menu(id,keys,menu_body)
}

public actionCfgMenu(id,key){
  switch(key){
  case 7:{
      if (saveSettings(g_fileToSave)){
        g_modified = false
        client_print(id,print_chat,"* Configuration saved successfully")
      }
      else
        client_print(id,print_chat,"* Failed to save configuration!!!")
      displayCfgMenu(id,g_menuPosition[id])
    }
  case 8: displayCfgMenu(id,++g_menuPosition[id])
  case 9: displayCfgMenu(id,--g_menuPosition[id])
  default:{
      g_modified = true  
      new a = g_menuPosition[id] * 7 + key
      set_xvar_num( g_menuDataId[ a ] , 1 - get_xvar_num( g_menuDataId[ a ] )  )
      displayCfgMenu( id , g_menuPosition[ id ] )
    }
  }
  return PLUGIN_HANDLED
}

saveSettings(filename[]){
  if (file_exists(filename))
    delete_file(filename)
  if (!write_file(filename,";Generated by Stats Configuration Plugin. Do not modify!^n;Variable  Description"))
    return 0
  new text[256]
  for(new a = 0; a < g_menuDataNum; ++a){
    if ( get_xvar_num( g_menuDataId[ a ] ) ) {
      format(text,255,"%-24.23s ;%s",g_menuDataVar[a],g_menuData[a])
      write_file(filename,text)
    }
  } 
  return 1
}

loadSettings(filename[]){
  if (!file_exists(filename)) 
    return 0
  new text[256], name[32]
  new len, pos = 0, xid
  while (read_file(filename,pos++,text,255,len)) {
    if ( text[0] == ';' ) continue // line is a comment
    parse( text , name , 31 )
    if (  ( xid = get_xvar_id( name ) ) != -1  )
      set_xvar_num( xid , 1 )
  }
  return 1
}