/* AMX Mod X script.
*
* (c) 2002-2004, OLO
*  modified by BAILOPAN,Manip,PM,SniperBeamer
*
* This file is provided as is (no warranties).
*/

#include <amxmod>
#include <amxmisc>

#define MAX_MESSAGES	6
#define X_POS			-1.0
#define Y_POS			0.30
#define HOLD_TIME		12.0

new g_Values[MAX_MESSAGES][3]
new g_Messages[MAX_MESSAGES][384]
new g_MessagesNum
new g_Current

public plugin_init(){
  register_plugin("Info. Messages","0.1","default")
  register_srvcmd("amx_imessage","setMessage")
  register_cvar("amx_freq_imessage","10")
  new lastinfo[8]
  get_localinfo("lastinfomsg",lastinfo,7)
  g_Current = strtonum(lastinfo)
  set_localinfo("lastinfomsg","")
}

public infoMessage(){
  if (g_Current >= g_MessagesNum)
    g_Current = 0
  set_hudmessage(g_Values[g_Current][0], g_Values[g_Current][1], g_Values[g_Current][2], 
    X_POS, Y_POS, 0, 0.5, HOLD_TIME , 2.0, 2.0, 1)
  show_hudmessage(0,g_Messages[g_Current])
  client_print(0,print_console,g_Messages[g_Current])
  ++g_Current
  new Float:freq_im = get_cvar_float("amx_freq_imessage")
  if ( freq_im > 0.0 ) set_task( freq_im ,"infoMessage",12345)
}

public setMessage(id,level,cid) {
  if (!cmd_access(id,level,cid,3))
    return PLUGIN_HANDLED
  if (g_MessagesNum >= MAX_MESSAGES)  {
    console_print(id,"Information Messages limit reached!")
    return PLUGIN_HANDLED
  }
  remove_task(12345)
  read_argv(1,g_Messages[g_MessagesNum],380)
  new hostname[64]
  get_cvar_string("hostname",hostname,63)
  replace(g_Messages[g_MessagesNum],380,"%hostname%",hostname)
  while(replace(g_Messages[g_MessagesNum],380,"\n","^n")){}
  new mycol[12]
  read_argv(2,mycol,11) // RRRGGGBBB
  g_Values[g_MessagesNum][2] = strtonum(mycol[6])
  mycol[6] = 0
  g_Values[g_MessagesNum][1] = strtonum(mycol[3])
  mycol[3] = 0
  g_Values[g_MessagesNum][0] = strtonum(mycol[0])
  g_MessagesNum++
  new Float:freq_im = get_cvar_float("amx_freq_imessage")
  if ( freq_im > 0.0 ) set_task( freq_im ,"infoMessage",12345)
  return PLUGIN_HANDLED
}

public plugin_end(){
  new lastinfo[8]
  numtostr(g_Current,lastinfo,7)
  set_localinfo("lastinfomsg",lastinfo)
}