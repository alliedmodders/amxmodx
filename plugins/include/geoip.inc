/* GeoIP module functions for AMX Mod X
   by David "BAILOPAN" Anderson
   (C)Copyrighted under the GNU General Public License, Version 2
 */

#if defined _geoip_included
  #endinput
#endif
#define _geoip_included

#if AMXX_VERSION_NUM >= 175
 #pragma reqlib geoip
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma loadlib geoip
 #endif
#else
 #pragma library geoip
#endif

//IP address can contain ports, the ports will be stripped out

//get a two character country code (eg US, CA etc)
native geoip_code2(ip[], ccode[3]);

//get a three character country code (eg USA, CAN etc)
native geoip_code3(ip[], result[4]);

//get a full country name.  max name is 45 chars
native geoip_country(ip[], result[], len=45);
