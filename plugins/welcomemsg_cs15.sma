/* AMX Mod X
*   Welcome Message Plugin
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
  register_plugin("Welcome Message","0.20","AMXX Dev Team")
  g_cstrikeRunning = (is_running("cstrike") || is_running("czero"))
  get_configsdir(g_motdFile, 63);
  format(g_motdFile, 63, "%s/conmotd.txt", g_motdFile);
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
  get_cvar_string("amxmodx_version",mod_ver,31)
  if (mod_ver[0]) client_cmd(id, "echo ^"   o AMX Mod X %s^"",mod_ver)    
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