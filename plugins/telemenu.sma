/* AMX Mod X script.
*
* (c) 2002-2004, OLO
*  modified by the AMX Mod X Development Team
*
* This file is provided as is (no warranties).
*/

#include <amxmod>
#include <amxmisc>
#include <fun>

new g_menuPosition[33] 
new g_menuPlayers[33][32] 
new g_menuPlayersNum[33] 
new g_menuOption[33] = { -1 , ... } 
new g_menuOrgin[33][3] 
new g_logFile[16]
new g_cstrikeRunning

public plugin_init() 
{ 
  register_plugin("Teleport Menu","0.1","default") 
  register_clcmd("amx_teleportmenu","cmdTelMenu",ADMIN_CFG,"- displays teleport menu") 
  register_menucmd(register_menuid("Teleport Menu"),1023,"actionTelMenu") 
  get_logfile(g_logFile,15) 

  g_cstrikeRunning = is_running("cstrike")
} 

public actionTelMenu(id,key) 
{ 
    switch(key){ 
    case 6:{ 
            g_menuOption[id] = 1 - g_menuOption[id] 
            displayTelMenu(id,g_menuPosition[id]) 
        }     
    case 7:{ 
            if (g_menuOption[id] < 0) /* unlocking position for the first time */ 
                g_menuOption[id] = 0 
            get_user_origin(id,g_menuOrgin[id]) 
            displayTelMenu(id,g_menuPosition[id]) 
        } 
    case 8: displayTelMenu(id,++g_menuPosition[id]) 
    case 9:    displayTelMenu(id,--g_menuPosition[id]) 
    default:{ 
            new player = g_menuPlayers[id][g_menuPosition[id] * 6 + key] 
             
            new name2[32] 
            get_user_name(player,name2,31) 
             
            if (!is_user_alive(player)) 
            { 
                client_print(id,print_chat,"That action can't be performed on dead client ^"%s^"",name2) 
                displayTelMenu(id,g_menuPosition[id]) 
                return PLUGIN_HANDLED 
            } 
             
            if (g_menuOption[id] > 0) 
            { 
                set_user_origin(player,g_menuOrgin[id]) 
            } 
            else 
            { 
                new origin[3] 
                get_user_origin(id,origin) 
                set_user_origin(player,origin) 
            } 
             
            new authid[32],authid2[32], name[32] 

            get_user_authid(id,authid,31) 
            get_user_authid(player,authid2,31) 
            get_user_name(id,name,31) 
                 
            log_to_file(g_logFile,"Cmd: ^"%s<%d><%s><>^" teleport ^"%s<%d><%s><>^"", 
                name,get_user_userid(id),authid, name2,get_user_userid(player),authid2 ) 
         
            switch(get_cvar_num("amx_show_activity"))    { 
            case 2:    client_print(0,print_chat,"ADMIN %s: teleport %s",name,name2) 
            case 1:    client_print(0,print_chat,"ADMIN: teleport %s",name2) 
            }             
             
            displayTelMenu(id,g_menuPosition[id]) 
        } 
    } 
    return PLUGIN_HANDLED 
} 


displayTelMenu(id,pos){ 

    if (pos < 0) 
        return 
         
    get_players(g_menuPlayers[id],g_menuPlayersNum[id]) 
         
    new menuBody[512] 
    new b = 0 
    new i 
    new name[32] 
    new start = pos * 6 
    new bool:blockMenu = (is_user_alive(id)&&g_menuOption[id]<1) ? true : false 
     
    if (start >= g_menuPlayersNum[id]) 
        start = pos = g_menuPosition[id] = 0 
         
    new len = format(menuBody,511, g_cstrikeRunning ? 
      "\yTeleport Menu\R%d/%d^n\w^n" : "Teleport Menu %d/%d^n^n" , 
        pos+1,(  g_menuPlayersNum[id] / 6 + ((g_menuPlayersNum[id] % 6) ? 1 : 0 )) ) 
         
    new end = start + 6 
    new keys = (1<<9)|(1<<7) 
     
    if (end > g_menuPlayersNum[id]) 
        end = g_menuPlayersNum[id] 
         
    for(new a = start; a < end; ++a) 
    { 
      i = g_menuPlayers[id][a] 
      get_user_name(i,name,31) 
         
      if ( blockMenu ||  !is_user_alive(i) || (get_user_flags(i)&ADMIN_IMMUNITY) ) 
      { 
        ++b     
        if ( g_cstrikeRunning )
          len += format(menuBody[len],511-len,"\d%d. %s^n\w",b,name)
        else
          len += format(menuBody[len],511-len,"#. %s^n",name)
      } 
      else 
      { 
        keys |= (1<<b) 
        len += format(menuBody[len],511-len,"%d. %s^n",++b,name) 
      } 
    } 
     
    if ( g_menuOption[id] > 0 ) // 1 
    { 
        keys |= (1<<6) 
        len += format(menuBody[len],511-len,"^n7. To location: %d %d %d^n", 
            g_menuOrgin[id][0],g_menuOrgin[id][1] ,g_menuOrgin[id][2]) 
    } 
    else if ( g_menuOption[id] ) // -1 
    { 
      if ( g_cstrikeRunning )
        len += format(menuBody[len],511-len,"^n\d7. Current Location^n\w") 
      else
        len += format(menuBody[len],511-len,"^n#. Current Location^n")
    } 
    else // 0 
    { 
        keys |= (1<<6) 
        len += format(menuBody[len],511-len,"^n7. Current Location^n") 
    } 
     
    len += format(menuBody[len],511-len,"8. Save Location^n") 
     
    if (end != g_menuPlayersNum[id]) 
    { 
        format(menuBody[len],511-len,"^n9. More...^n0. %s", pos ? "Back" : "Exit") 
        keys |= (1<<8) 
    } 
    else 
        format(menuBody[len],511-len,"^n0. %s", pos ? "Back" : "Exit") 
     
    show_menu(id,keys,menuBody) 
} 

public cmdTelMenu(id,level,cid) 
{ 
  if (cmd_access(id,level,cid,1)) 
    displayTelMenu(id,g_menuPosition[id] = 0) 
     
  return PLUGIN_HANDLED 
} 

