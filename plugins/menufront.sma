/* AMX Mod script.
*
* (c) 2003, OLO
* This file is provided as is (no warranties).
*
*/

#include <amxmod>
#include <amxmisc>

new g_menuPosition[33]

#define MENUS_NUMBER 15

new g_menuBody[MENUS_NUMBER][] = {
"Kick Player",
"Ban Player",
"Slap/Slay Player",
"Team Player^n",

"Changelevel",
"Vote for maps^n",

"Speech Stuff",
"Client Commands",

// Next Page

"Server Commands",
"Cvars Settings",
"Configuration",
"Stats Settings^n",

"Pause Plugins",
"Restrict Weapons",

"Teleport Player" /* Last is Teleport menu - if you want to move 
  it change also code in displayMenu (look for fun module check) */

}

new g_menuCmd[MENUS_NUMBER][] = {
"amx_kickmenu",
"amx_banmenu",
"amx_slapmenu",
"amx_teammenu",

"amx_mapmenu",
"amx_votemapmenu",

"amx_speechmenu",
"amx_clcmdmenu",

// Next Page

"amx_cmdmenu",
"amx_cvarmenu",
"amx_cfgmenu",
"amx_statscfgmenu",

"amx_pausecfgmenu",
"amx_restmenu",

"amx_teleportmenu"
}

// Second value sets if menu is only for CS...
new g_menuAccess[MENUS_NUMBER][2] = {
  {ADMIN_KICK,0},
  {ADMIN_BAN,0},
  {ADMIN_SLAY,0},
  {ADMIN_LEVEL_A,1},

  {ADMIN_MAP,0},
  {ADMIN_MAP,0},

  {ADMIN_MENU,0},
  {ADMIN_LEVEL_A,0},

// Next Page

  {ADMIN_MENU,0},
  {ADMIN_CVAR,0},
  {ADMIN_MENU,0},
  {ADMIN_CFG,1},

  {ADMIN_CFG,0},
  {ADMIN_CFG,1},

  {ADMIN_LEVEL_A,0}
}

new g_cstrikeRunning
new g_funModule

public plugin_init()
{
  register_plugin("Menus Front-End","0.9","default")  

  register_menucmd(register_menuid("AMX Mod Menu"),1023,"actionMenu") 
  register_clcmd("amxmodmenu","cmdMenu",ADMIN_MENU,"- displays menus")    
  
  g_cstrikeRunning = is_running("cstrike")
  g_funModule = cvar_exists( "fun_version" )
}

public actionMenu(id,key)
{
  switch(key){
  case 8: displayMenu(id,++g_menuPosition[id])
  case 9: displayMenu(id,--g_menuPosition[id])
  default: client_cmd(id, g_menuCmd[ g_menuPosition[id] * 8 + key ] )
  }
  return PLUGIN_HANDLED
}

displayMenu(id,pos){

  if (pos < 0)  return
    
  new menuBody[512]
  new b = 0
  new start = pos * 8
  
  if ( start >= MENUS_NUMBER )
    start = pos = g_menuPosition[id] = 0
      
  new len = format(menuBody,511,
   g_cstrikeRunning ? "\yAMX Mod Menu\R%d/%d^n\w^n" : "AMX Mod Menu %d/%d^n^n" , pos+1, 2 )
    
  new end = start + 8
  new keys = (1<<9)
  
  if (end > MENUS_NUMBER )
    end = MENUS_NUMBER
    
  new flags = get_user_flags(id)
    
  for(new a = start; a < end; ++a)
  {
      if ( a == MENUS_NUMBER - 1 && !g_funModule ) 
        continue // checks if there is fun module for teleport menu
  
      if ( (flags & g_menuAccess[a][0]) && ( g_menuAccess[a][1] ? g_cstrikeRunning : 1 ) )
      {
        keys |= (1<<b)
        len += format(menuBody[len],511-len,"%d. %s^n",++b, g_menuBody[ a ] )
      }
      else
      {
        ++b     
        if ( g_cstrikeRunning )
          len += format(menuBody[len],511-len, "\d%d. %s^n\w",b, g_menuBody[ a ] )
        else
          len += format(menuBody[len],511-len, "#. %s^n",g_menuBody[ a ] )

      }
  }
      
  if (end != MENUS_NUMBER )
  {
    format(menuBody[len],511-len,"^n9. More...^n0. %s", pos ? "Back" : "Exit")
    keys |= (1<<8)
  }
  else format(menuBody[len],511-len,"^n0. %s", pos ? "Back" : "Exit")
 
  show_menu(id,keys,menuBody)
}

public cmdMenu(id,level,cid)
{
  if (cmd_access(id,level,cid,1))
    displayMenu(id,g_menuPosition[id] = 0)
  return PLUGIN_HANDLED
}