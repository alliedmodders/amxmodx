/* AMX Mod X Backwards Compatibility
*
* by the AMX Mod X Development Team
*
* This file is provided as is (no warranties).
*/

#if defined _amxmod_included
  #endinput
#endif
#define _amxmod_included

#include <amxmodx>
#include <cstrike>
#include <engine>
#include <fun>

/* String */
stock num_to_str(num,string[],len)
  return int_to_str(num,string,len)

stock numtostr(num,string[],len)
  return int_to_str(num,string,len)

stock str_to_num(const string[])
  return str_to_int(string)

stock strtonum(const string[])
  return str_to_int(string)

/* Fun */
stock set_user_hitzones(index=0,target=0,body=255)
  return set_hitzones(body)
  
stock get_user_hitzones(index,target)
  return get_hitzones()
  
stock user_spawn(index)
  return spawn(index)

/* Misc */
/* use log_amx() instead */
stock get_logfile( name[], len )
  return get_time("admin%m%d.log",name,len)