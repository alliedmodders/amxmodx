/* AMX Mod X script.
*
* (c) 2002-2004, OLO
*  modified by the AMX Mod X Development Team
*
* This file is provided as is (no warranties).
*/

#include <amxmod>
#include <amxmisc>

#define MOTD_LENGTH 1024

// Settings (comment unwanted options)
#define SHOW_MODS
#define READ_FROM_FILE
//#define SHOW_TIME_AND_IP

new g_cstrikeRunning

#if defined READ_FROM_FILE
new g_motdFile[64]
#endif

public plugin_init()
{
  register_plugin("Welcome Message","0.1","default")
  g_cstrikeRunning = is_running("cstrike")
#if defined READ_FROM_FILE
  build_path( g_motdFile , 63 , "$basedir/conmotd.txt" )
#endif  
}

public plugin_cfg()
{
  set_cvar_string("motdfile","")
}

public client_putinserver(id) {
  new param[1]
  param[0] = id
  set_task(2.5,"alt_motd",0,param,1)
}

public alt_motd(param[]) {
  new id = param[0]
  new motdBody[MOTD_LENGTH], name[32], hostname[64], nextmap[32], mapname[32]

  get_cvar_string("hostname",hostname,63) 
  get_user_name(id,name,31)
  
  get_mapname(mapname,31)
  get_cvar_string("amx_nextmap",nextmap,31)

  new len = copy(motdBody,MOTD_LENGTH,"<html><head><style type=^"text/css^">body{background:#000000;color:#FFB000;margin-left:8px;margin-top:8px;}")
  len += copy(motdBody[len],MOTD_LENGTH,"hr{color:#FFB000;border:0px;}ul{margin-top:2px;margin-bottom:2px;}hr{margin-top:8px;margin-bottom:8px;}</style></head><body>")
  len += format(motdBody[len],MOTD_LENGTH-len,"Hello %s, welcome to %s<ul>",name,hostname)
  
#if defined SHOW_TIME_AND_IP  
  new stime[64],ip[32]
  get_time("%A %B %d, %Y - %H:%M:%S",stime,63)
  get_user_ip(id,ip,31)
  len += format(motdBody[len],MOTD_LENGTH-len,"<li>Today is %s</li>",stime)
  len += format(motdBody[len],MOTD_LENGTH-len,"<li>You are playing from: %s</li>",ip)
#endif

  new maxplayers = get_cvar_num("sv_visiblemaxplayers")
  if ( maxplayers < 0 ) maxplayers = get_maxplayers()
  len += format(motdBody[len],MOTD_LENGTH-len,"<li>Players on server: %d/%d</li>",get_playersnum(),maxplayers)
  len += format(motdBody[len],MOTD_LENGTH-len,"<li>Current map: %s, Next map: %s</li>",mapname,nextmap)
  
  // Time limit and time remaining
  new Float:mp_timelimit = get_cvar_float("mp_timelimit")
  if (mp_timelimit){
    new timeleft = get_timeleft()
    if (timeleft > 0)
      len += format(motdBody[len],MOTD_LENGTH-len,"<li>Time Left: %d:%02d of %.0f minutes</li>",timeleft / 60, timeleft % 60, mp_timelimit)
  }
  else{
    len += copy(motdBody[len],MOTD_LENGTH-len,"<li>No time limit</li>")
  }

  // C4 and FF
  if ( g_cstrikeRunning ){
    len += format(motdBody[len],MOTD_LENGTH-len,"<li>Friendly fire is %s</li>",get_cvar_num("mp_friendlyfire") ? "ON" : "OFF")
    len += format(motdBody[len],MOTD_LENGTH-len,"<li>C4 timer is set to %.0f sec.</li>",get_cvar_float("mp_c4timer"))
  }
  len += copy(motdBody[len],MOTD_LENGTH-len,"</ul>")
        
  // Server Mods
#if defined SHOW_MODS
  new mod_ver[32]
  len += copy(motdBody[len],MOTD_LENGTH-len,"<br>Server mods:<ul>")
  get_cvar_string("amx_version",mod_ver,31)
  if (mod_ver[0]) len += format(motdBody[len],MOTD_LENGTH-len,"<li>AMX Mod %s</li>",mod_ver)    
  get_cvar_string("statsme_version",mod_ver,31)
  if (mod_ver[0]) len += format(motdBody[len],MOTD_LENGTH-len,"<li>StatsMe %s</li>",mod_ver)
  get_cvar_string("clanmod_version",mod_ver,31)
  if (mod_ver[0]) len += format(motdBody[len],MOTD_LENGTH-len,"<li>ClanMod %s</li>",mod_ver)
  get_cvar_string("admin_mod_version",mod_ver,31)
  if (mod_ver[0]) len += format(motdBody[len],MOTD_LENGTH-len,"<li>AdminMod %s</li>",mod_ver)
  get_cvar_string("chicken_version",mod_ver,31)
  if (mod_ver[0]) len += format(motdBody[len],MOTD_LENGTH-len,"<li>Chicken %s</li>",mod_ver)
  get_cvar_string("csguard_version",mod_ver,31)
  if (mod_ver[0]) len += format(motdBody[len],MOTD_LENGTH-len,"<li>CSGuard %s</li>",mod_ver)
  get_cvar_string("hlguard_version",mod_ver,31)
  if (mod_ver[0]) len += format(motdBody[len],MOTD_LENGTH-len,"<li>HLGuard %s</li>",mod_ver)
  get_cvar_string("plbot_version",mod_ver,31)
  if (mod_ver[0]) len += format(motdBody[len],MOTD_LENGTH-len,"<li>PLBot %s</li>",mod_ver)
  get_cvar_string("booster_version",mod_ver,31)
  if (mod_ver[0]) len += format(motdBody[len],MOTD_LENGTH-len,"<li>HL-Booster %s</li>",mod_ver)
  get_cvar_string("axn_version",mod_ver,31)
  if (mod_ver[0]) len += format(motdBody[len],MOTD_LENGTH-len,"<li>AXN %s</li>",mod_ver)
  get_cvar_string("bmx_version",mod_ver,31)
  if (mod_ver[0]) len += format(motdBody[len],MOTD_LENGTH-len,"<li>BMX %s</li>",mod_ver)
  get_cvar_string("cdversion",mod_ver,31)
  if (mod_ver[0]) len += format(motdBody[len],MOTD_LENGTH-len,"<li>Cheating-Death %s in %s Mode</li>",
    mod_ver, get_cvar_num("cdrequired") ? "Required" : "Optional" )
  get_cvar_string("atac_version",mod_ver,31)
  if (mod_ver[0]) len += format(motdBody[len],MOTD_LENGTH-len,"<li>ATAC %s</li>",mod_ver)
  len += copy(motdBody[len],MOTD_LENGTH-len,"</ul>")
#endif

  // Info. from custom file
#if defined READ_FROM_FILE
  if (file_exists(g_motdFile)) {
    new message[192], len2, line = 0
    len += format(motdBody[len],MOTD_LENGTH-len,"<hr noshade>")
    while(read_file(g_motdFile,line++,message,191,len2))
      len += copy(motdBody[len],MOTD_LENGTH-len,message)
  }
#endif
  len += copy(motdBody[len],MOTD_LENGTH-len,"</body></html>")
  show_motd(id,motdBody)
}