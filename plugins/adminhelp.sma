/* AMX Mod X script.
*
* (c) 2002-2004, tcquest78
*  modified by BAILOPAN,Manip,PM,SniperBeamer
*
* This file is provided as is (no warranties).
*/

#include <amxmod>

#define HELPAMOUNT 10 // Number of commands per page

new g_typeHelp[] = "Type 'amx_help' in the console to see available commands"
new g_timeInfo1[] = "Time Left: %d:%02d min. Next Map: %s"
new g_timeInfo2[] = "No Time Limit. Next Map: %s"

public plugin_init() {
  register_plugin("Admin Help","0.1","tcquest78") 
  register_concmd("amx_help","cmdHelp",0,"- displays this help")
  setHelp(0)
}

public client_putinserver(id) 
  setHelp(id)

public cmdHelp(id,level,cid){
  new arg1[8],flags = get_user_flags(id)
  new start = read_argv(1,arg1,7) ? strtonum(arg1) : 1
  if (--start < 0) start = 0
  new clcmdsnum = get_concmdsnum(flags,id)
  if (start >= clcmdsnum) start = clcmdsnum - 1
  console_print(id,"^n----- AMX Help: Commands -----")
  new info[128], cmd[32], eflags
  new end = start + HELPAMOUNT
  if (end > clcmdsnum) end = clcmdsnum
  for (new i = start; i < end; i++){
    get_concmd(i,cmd,31,eflags,info,127,flags,id)
    console_print(id,"%3d: %s %s",i+1,cmd,info)
  }
  console_print(id,"----- Entries %d - %d of %d -----",start+1,end,clcmdsnum)
  if (end < clcmdsnum)
     console_print(id,"----- Use 'amx_help %d' for more -----",end+1)
  else
     console_print(id,"----- Use 'amx_help 1' for begin -----")
  return PLUGIN_HANDLED
}

public dispInfo(id){
  if (id) client_print(id,print_chat, g_typeHelp )
  else server_print( g_typeHelp )
  new nextmap[32]
  get_cvar_string("amx_nextmap",nextmap,31)
  if (get_cvar_float("mp_timelimit")){
    new timeleft = get_timeleft()
    if (timeleft > 0){
      if (id) client_print(id,print_chat, g_timeInfo1 , timeleft / 60, timeleft % 60,nextmap)
      else server_print( g_timeInfo1 , timeleft / 60, timeleft % 60,nextmap)     
    }
  }
  else if (id) client_print(id,print_chat, g_timeInfo2 ,nextmap)
  else server_print( g_timeInfo2 ,nextmap)
}

setHelp(id) 
  set_task(15.0,"dispInfo",id)