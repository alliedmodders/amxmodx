/* AMX Mod X script.
*
* (c) 2002-2004, OLO
*  modified by BAILOPAN,Manip,PM,SniperBeamer
*
* This file is provided as is (no warranties).
*/

#include <amxmod>
#include <amxmisc>

#define SPEED 0.3

new g_startPos
new g_endPos
new g_scrollMsg[384]
new g_displayMsg[384]
new Float:g_xPos
new g_Length
new g_Frequency

public plugin_init(){
  register_plugin("Scrolling Message","0.1","default")
  register_srvcmd("amx_scrollmsg","setMessage")
}

public showMsg(){
  new a = g_startPos, i = 0
  
  while( a < g_endPos )
    g_displayMsg[i++] = g_scrollMsg[a++]
    
  g_displayMsg[i] = 0
  
  if (g_endPos < g_Length)
    g_endPos++
    
  if (g_xPos > 0.35)
    g_xPos -= 0.0063
  else 
  {
    g_startPos++
    g_xPos = 0.35
  }
  
  set_hudmessage(200, 100, 0, g_xPos, 0.90, 0, SPEED, SPEED, 0.05, 0.05, 2)
  show_hudmessage(0,g_displayMsg)
}

public msgInit(){
  g_endPos = 1
  g_startPos = 0
  g_xPos = 0.65
  set_task( SPEED , "showMsg",123,"",0,"a", g_Length + 48)
  client_print(0,print_console,g_scrollMsg)
}

public setMessage(id,level,cid) {
  if (!cmd_access(id,level,cid,3))
    return PLUGIN_HANDLED
  remove_task(123) /* remove current messaging */
  read_argv(1,g_scrollMsg,380)
  new hostname[64]
  get_cvar_string("hostname",hostname,63)
  replace(g_scrollMsg,380,"%hostname%",hostname)
  g_Length = strlen(g_scrollMsg)
  new mytime[32]
  read_argv(2,mytime,31)
  g_Frequency = strtonum(mytime)
  if (g_Frequency > 0) {    
    new minimal = floatround((g_Length + 48) * (SPEED + 0.1))
    if (g_Frequency < minimal)  {
      console_print(id,"Minimal frequency for this message is %d seconds",minimal)
      g_Frequency = minimal
    }
    console_print(id,"Scrolling message displaying frequency: %d:%02d minutes",
      g_Frequency/60,g_Frequency%60)
    set_task(float(g_Frequency),"msgInit",123,"",0,"b")
  }
  else
    console_print(id,"Scrolling message disabled")
  return PLUGIN_HANDLED
}