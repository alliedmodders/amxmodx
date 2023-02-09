// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// TimeLeft Plugin
//

#include <amxmodx>

new g_TimeSet[32][2]
new g_LastTime
new g_CountDown
new g_Switch
new Float:g_roundStartTime = 999999.9
new bool:is_combat
new g_amx_time_print_to_all, g_amx_time_show_cmd_in_chat

public plugin_init() {
  register_plugin("TimeLeft",AMXX_VERSION_STR,"AMXX Dev Team")
  register_cvar("amx_time_voice","1")
  register_srvcmd("amx_time_display","setDisplaying")
  register_cvar("amx_timeleft","00:00",FCVAR_SERVER|FCVAR_EXTDLL|FCVAR_UNLOGGED|FCVAR_SPONLY)
  register_clcmd("say timeleft","sayTimeLeft",0,"- displays timeleft")
  register_clcmd("say thetime","sayTheTime",0,"- displays current time")
  set_task(0.8,"timeRemain",8648458,"",0,"b")

  g_amx_time_print_to_all = register_cvar("amx_time_print_to_all", "1")
  g_amx_time_show_cmd_in_chat = register_cvar("amx_time_show_cmd_in_chat", "1")
  
  new szMapName[4]
  get_mapname(szMapName, charsmax(szMapName))
  if (equal(szMapName, "co_")) {
    register_event("PlayHUDNot", "roundChange", "bc", "1=0", "2>56", "2<59")
    is_combat = true
  }
}

public sayTheTime(id){
  if ( get_cvar_num("amx_time_voice") ){
    new mhours[6], mmins[6], whours[32], wmins[32], wpm[6]
    get_time("%H",mhours,charsmax(mhours))
    get_time("%M",mmins,charsmax(mmins))
    new mins = str_to_num(mmins)
    new hrs = str_to_num(mhours)
    if (mins)
      num_to_word(mins,wmins,charsmax(wmins))
    else
      wmins[0] = 0
    if (hrs < 12)
      wpm = "am "
    else {
      if (hrs > 12) hrs -= 12
      wpm = "pm "
    }
    if (hrs) 
      num_to_word(hrs,whours,charsmax(whours))
    else
      whours = "twelve "
    client_cmd(id, "spk ^"fvox/time_is_now %s_period %s%s^"",whours,wmins,wpm )
  }
  new ctime[64]  
  get_time("%m/%d/%Y - %H:%M:%S",ctime,charsmax(ctime))  
  client_print(get_pcvar_bool(g_amx_time_print_to_all) ? 0 : id,print_chat, "The time:   %s",ctime )
  return get_pcvar_bool(g_amx_time_show_cmd_in_chat) ? PLUGIN_CONTINUE : PLUGIN_HANDLED
}

public sayTimeLeft(id){
  if (get_cvar_float("mp_timelimit")) {
    new a = get_timeleft()
    if ( get_cvar_num("amx_time_voice") ) {
      new svoice[128]
      setTimeVoice( svoice , charsmax(svoice) , 0 , a )
      client_cmd( id , "%s", svoice  )
    }    
    client_print(get_pcvar_bool(g_amx_time_print_to_all) ? 0 : id,print_chat, "Time Left:  %d:%02d", (a / 60) , (a % 60) )
  }
  else
    client_print(get_pcvar_bool(g_amx_time_print_to_all) ? 0 : id,print_chat, "No Time Limit" )
  return get_pcvar_bool(g_amx_time_show_cmd_in_chat) ? PLUGIN_CONTINUE : PLUGIN_HANDLED
}

setTimeText(text[],len,tmlf){
  new secs = tmlf % 60
  new mins = tmlf / 60
  if (secs == 0)
    format(text,len,"%d minute%s", mins , (mins > 1) ? "s" : "" )
  else if (mins == 0)
    format(text,len,"%d second%s",  secs,(secs > 1) ? "s" : ""   )  
  else
    format(text,len,"%d minute%s %d second%s", mins , (mins > 1) ? "s" : "" ,secs ,(secs > 1) ? "s" : ""  )
}

/*
temp[0] = number of hours
temp[1] = "hours "
temp[2] = number of minutes
temp[3] = "minutes "
temp[4] =
temp[5] = 
temp[6] = "remaining "
Flags:
4 (c) - Don't add "remaining "
8 (d)- Don't add "hours " or "minutes"
*/
setTimeVoice(text[],len,flags,tmlf){
  new temp[7][32]
  new secs = tmlf % 60
  new mins = tmlf / 60
  for(new a = 0;a < sizeof(temp);++a)
    temp[a][0] = 0
  if (secs > 0){
    num_to_word(secs,temp[4],charsmax(temp[]))
    if (!(flags & 8)) temp[5] = "seconds " /* there is no "second" in default hl */
  }
  if (mins > 59){
    new hours = mins / 60
    num_to_word(hours,temp[0],charsmax(temp[]))
    if (!(flags & 8)) temp[1] = "hours "
    mins = mins % 60
  }
  if (mins > 0) {
    num_to_word(mins ,temp[2],charsmax(temp[]))
    if (!(flags & 8)) temp[3] =  "minutes "
  }
  if (!(flags & 4)) temp[6] = "remaining "
  return format(text,len,"spk ^"vox/%s%s%s%s%s%s%s^"", temp[0],temp[1],temp[2],temp[3],temp[4],temp[5],temp[6] )
}

findDispFormat(timeleft){
  for(new i = 0;g_TimeSet[i][0];++i){
    if (g_TimeSet[i][1] & 16){			// show/speak if current time is less than this set in parameter
      if (g_TimeSet[i][0] > timeleft){
        if (!g_Switch) {
          g_CountDown = g_Switch = timeleft
          remove_task(8648458)
          set_task(0.98,"timeRemain",34543,"",0,"b")	// 0.98 because of cumulative time lost during each server frame
        }
        return i
      }
    }
    else if (g_TimeSet[i][0] == timeleft){
      return i
    }
  }
  return -1
}

/*
Displaying of round time remaining on combat maps
a (1) - display white text on bottom
b (2) - use voice
c (4) - don't add "remaining" (only in voice)
d (8) - don't add "hours/minutes/seconds" (only in voice)
e (16) - show/speak if current time is less than this set in parameter
amx_time_display "ab 1200" "ab 600" "ab 300" "ab 180" "ab 60" "bcde 11"

g_TimeSet[i][0] = timeleft at which to display its value
g_timeSet[i][1] = bit mask of the flags abcde
*/
public setDisplaying(){
  new arg[32], flags[32], num[32]
  new argc = read_argc() - 1
  for(new i; (argc > i < 32); ++i) {
    read_argv(i+1,arg,charsmax(arg))
    parse(arg,flags,charsmax(flags),num,charsmax(num))
    g_TimeSet[i][0] = str_to_num(num)
    g_TimeSet[i][1] = read_flags(flags)
    g_TimeSet[i+1][0] = 0
  }
  return PLUGIN_HANDLED
}

public timeRemain(param[]){
  new gmtm = get_timeleft()
  new stimel[12]
  format(stimel,charsmax(stimel),"%02d:%02d",gmtm / 60, gmtm % 60)
  set_cvar_string("amx_timeleft",stimel)

  if (!is_combat)
    return

  new tmlf = g_Switch ? --g_CountDown : getCombatTimeLeft()

  if ( g_Switch && tmlf > g_Switch ) {
    g_Switch = 0
    set_task(0.8,"timeRemain",8648458,"",0,"b")
    return
  }
  
  if (tmlf > 0 && g_LastTime != tmlf){
    g_LastTime = tmlf
    new tm_set = findDispFormat(tmlf)
    if ( tm_set != -1){
      new flags = g_TimeSet[tm_set][1]
      new arg[128]
      if (flags & 1){			// display white text on bottom
        setTimeText(arg,charsmax(arg),tmlf)
        if (flags & 16)			// show/speak if current time is less than this set in parameter
          set_hudmessage(255, 255, 255, -1.0, 0.85, 0, 0.0, 1.1, 0.1, 0.5, -1)
        else
          set_hudmessage(255, 255, 255, -1.0, 0.85, 0, 0.0, 3.0, 0.0, 0.5, -1)
        show_hudmessage(0, "%s", arg)
      }
      if (flags & 2){			// use voice
        setTimeVoice(arg,charsmax(arg),flags,tmlf)
        client_cmd(0, "%s", arg)
      }
    }
  }
}

public roundChange() {
  switch ( read_data(2) ) {
    case 56: g_roundStartTime = get_gametime()
    case 57: {
    	g_roundStartTime = 999999.9	// Stop the countdown when a team wins
    	g_CountDown = 0
    	g_Switch = 0
    	remove_task(34543)
    	set_task(0.8,"timeRemain",8648458,"",0,"b")
    }
  }
}

getCombatTimeLeft() {
	new combatTime = floatround( (get_cvar_float("mp_combattime") * 60) - (get_gametime() - g_roundStartTime) )
	return (combatTime < 0) ? 0 : combatTime
}
