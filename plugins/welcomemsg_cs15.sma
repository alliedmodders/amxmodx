/* AMX Mod X script.
*
* (c) 2002-2004, OLO
*  modified by the AMX Mod X Development Team
*
* This file is provided as is (no warranties).
*/

#include <amxmod>
#include <amxmisc>

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

new g_Bar[] = "=============="

public client_connect(id) {
  new name[32], hostname[64], nextmap[32], mapname[32]

  get_cvar_string("hostname",hostname,63) 
  get_user_name(id,name,31)
  
  get_mapname(mapname,31)
  get_cvar_string("amx_nextmap",nextmap,31)

  client_cmd(id, "echo ;echo %s%s%s%s",g_Bar,g_Bar,g_Bar,g_Bar)
  client_cmd(id, "echo ^"   Hello %s, welcome to %s^"",name,hostname)
  
#if defined SHOW_TIME_AND_IP  
  new stime[64],ip[32]
  get_time("%A %B %d, %Y - %H:%M:%S",stime,63)
  get_user_ip(id,ip,31)
  client_cmd(id, "echo ^"   Today is %s^"",stime)
  client_cmd(id, "echo ^"   You are playing from: %s^"",ip)
#endif

  new maxplayers = get_cvar_num("sv_visiblemaxplayers")
  if ( maxplayers < 0 ) maxplayers = get_maxplayers()
  client_cmd(id, "echo ^"   Players on server: %d/%d^"",get_playersnum(),maxplayers)
  client_cmd(id, "echo ^"   Current map: %s, Next map: %s^"",mapname,nextmap)
  
  // Time limit and time remaining
  new Float:mp_timelimit = get_cvar_float("mp_timelimit")
  if (mp_timelimit){
    new timeleft = get_timeleft()
    if (timeleft > 0)
      client_cmd(id, "echo ^"   Time Left: %d:%02d of %.0f minutes^"",  timeleft / 60, timeleft % 60, mp_timelimit )
  }
  else{
    client_cmd(id, "echo ^"   No time limit^"")
  }

  // C4 and FF
  if ( g_cstrikeRunning ){
    client_cmd(id, "echo ^"   Friendly fire is %s^"", get_cvar_float("mp_friendlyfire") ? "ON" : "OFF") 
    client_cmd(id, "echo ^"   C4 timer is set to %.0f sec.^"",get_cvar_float("mp_c4timer"))
  }
        
  // Server Mods
#if defined SHOW_MODS
  new mod_ver[32]
  client_cmd(id, "echo ;echo ^"   Server mods:^"")
  get_cvar_string("amx_version",mod_ver,31)
  if (mod_ver[0]) client_cmd(id, "echo ^"   o AMX Mod %s^"",mod_ver)    
  get_cvar_string("statsme_version",mod_ver,31)
  if (mod_ver[0]) client_cmd(id, "echo ^"   o StatsMe %s^"",mod_ver)
  get_cvar_string("clanmod_version",mod_ver,31)
  if (mod_ver[0]) client_cmd(id, "echo ^"   o ClanMod %s^"",mod_ver)
  get_cvar_string("admin_mod_version",mod_ver,31)
  if (mod_ver[0]) client_cmd(id, "echo ^"   o AdminMod %s^"",mod_ver)
  get_cvar_string("chicken_version",mod_ver,31)
  if (mod_ver[0]) client_cmd(id, "echo ^"   o Chicken %s^"",mod_ver)
  get_cvar_string("csguard_version",mod_ver,31)
  if (mod_ver[0]) client_cmd(id, "echo ^"   o CSGuard %s^"",mod_ver)
  get_cvar_string("hlguard_version",mod_ver,31)
  if (mod_ver[0]) client_cmd(id, "echo ^"   o HLGuard %s^"",mod_ver)
  get_cvar_string("plbot_version",mod_ver,31)
  if (mod_ver[0]) client_cmd(id, "echo ^"   o PLBot %s^"",mod_ver)
  get_cvar_string("booster_version",mod_ver,31)
  if (mod_ver[0]) client_cmd(id, "echo ^"   o HL-Booster %s^"",mod_ver)
  get_cvar_string("axn_version",mod_ver,31)
  if (mod_ver[0]) client_cmd(id, "echo ^"   o AXN %s^"",mod_ver)
  get_cvar_string("bmx_version",mod_ver,31)
  if (mod_ver[0]) client_cmd(id, "echo ^"   o BMX %s^"",mod_ver)
  get_cvar_string("cdversion",mod_ver,31)
  if (mod_ver[0]) client_cmd(id, "echo ^"   o Cheating-Death %s in %s Mode^"",
    mod_ver, get_cvar_num("cdrequired") ? "Required" : "Optional" )
  get_cvar_string("atac_version",mod_ver,31)
  if (mod_ver[0]) client_cmd(id, "echo ^"   o ATAC %s%s^"" , mod_ver , get_cvar_num("atac_status") 
    ? " (setinfo atac_status_off 1 disables Live Status)" : "" )
  
#endif

  // Info. from custom file
#if defined READ_FROM_FILE
  if (file_exists(g_motdFile)) {
    new message[192], len, line = 0
    client_cmd(id, "echo %s%s%s%s",g_Bar,g_Bar,g_Bar,g_Bar)   
    while(read_file(g_motdFile,line++,message,191,len))
      client_cmd(id,"echo ^"%s^"",message)
  }
#endif

  client_cmd(id, "echo %s%s%s%s",g_Bar,g_Bar,g_Bar,g_Bar)
}