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

stock user_spawn(index)
  return spawn(index)

stock get_logfile( name[], len )
  return get_time("admin%m%d.log",name,len)

stock get_user_money(index)
  return cs_get_user_money(index)

stock set_user_money(index,money,flash=1)
  return cs_set_user_money(index,money,flash)

stock numtostr(num,string[],len)
  return num_to_str(num,string,len)

stock strtonum(const string[])
  return str_to_num(string)

stock build_path( path[] , len , {Float,_}:... )
{
  new basedir[32]
  get_localinfo("amxx_basedir",basedir,31)
  format_args(path,len,2)
  return replace(path,len,"$basedir",basedir)
}