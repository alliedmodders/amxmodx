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

native numtostr(num,string[],len)

native strtonum(const string[])

stock set_user_hitzones(index=0,target=0,body=255)
  return set_hitzones(body)
  
stock get_user_hitzones(index,target)
  return get_hitzones()
  
stock user_spawn(index)
  return spawn(index)

/* use log_amx() instead */
stock get_logfile( name[], len )
  return get_time("admin%m%d.log",name,len)

stock get_user_money(index)
  return cs_get_user_money(index)

stock set_user_money(index,money,flash=1)
  return cs_set_user_money(index,money,flash)