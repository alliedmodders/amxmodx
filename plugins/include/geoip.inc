// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// GeoIP Module Functions
//

#if defined geoip_included
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

/// IP addresses passed to these natives can contain ports, the ports will be ignored.

/**
 * Lookup the two character country code for a given IP address.
 * e.g: "US", "CA", etc.
 *
 * @param ip		The IP address to lookup.
 * @param result	The result buffer.  If the lookup does not succeed, the buffer is not modified.
 * @return			true on a successful lookup, false on a failed lookup.
 */
native bool:geoip_code2_ex(const ip[], result[3]);

/**
 * Lookup the three character country code for a given IP address.
 * e.g: "USA", "cAN", etc.
 *
 * @param ip		The IP address to lookup.
 * @param result	The result buffer.  If the lookup does not succeed, the buffer is not modified.
 * @return			true on a successful lookup, false on a failed lookup.
 */
native bool:geoip_code3_ex(const ip[], result[4]);

/**
 * @deprecated
 * Lookup the two character country code for a given IP address.
 *
 * @note This native will overflow the buffer by one cell on an unknown ip lookup!
 * @note Use geoip_code2_ex instead!
 *
 * @param ip		The IP address to lookup.
 * @param result	The result buffer.
 */
native geoip_code2(const ip[], ccode[3]);

/**
 * @deprecated
 * Lookup the three character country code for a given IP address.
 *
 * @note This native will overflow the buffer by one cell on an unknown ip lookup!
 * @note Use geoip_code3_ex instead!
 *
 * @param ip		The IP address to lookup.
 * @param result	The result buffer.
 */
native geoip_code3(const ip[], result[4]);

/**
 * Lookup the full country name for the given IP address.  Sets the buffer to "error" on
 * an unsuccessful lookup.
 *
 * @param ip		The IP address to lookup.
 * @param result	The result of the geoip lookup.
 * @param len		The maximum length of the result buffer.
 */
native geoip_country(const ip[], result[], len=45);



/**
 * The following natives require GeoIP City database, which can be retrieved from:
 * http://dev.maxmind.com/geoip/geoip2/geolite2/ (MaxMind DB binary)
 */

/** 
 * Look up the full city name for the given IP address.
 * 
 * @param ip		The IP address to look up. 
 * @param result	The result of the geoip look up. 
 * @param len		The maximum length of the result buffer.
 *
 * @return          The result length on successful lookup, 0 otherwise.
 */ 
native geoip_city(const ip[], result[], len); 

/**
 * Look up the region/state code for the given IP address.
 * e.g. "US-OH", "DE-HH", IT-82, "FR-U", etc.
 *
 * @param ip		The IP address to look up.
 * @param result	The result of the geoip look up.
 * @param len		The maximum length of the result buffer.
 *
 * @return			The result length on successful lookup, 0 otherwise.
 */
native geoip_region_code(const ip[], result[], len);

/**
 * Look up the full region/state name for the given IP address.
 *
 * @param ip		The IP address to look up.
 * @param result	The result of the geoip look up.
 * @param len		The maximum length of the result buffer.
 *
 * @return			The result length on successful lookup, 0 otherwise.
 */
native geoip_region_name(const ip[], result[], len);

/**
 * Look up the full time zone for the given IP address.
 * e.g. America/Los_Angeles, Europe/Paris.
 *
 * @param ip		The IP address to look up.
 * @param result	The result of the geoip look up.
 * @param len		The maximum length of the result buffer.
 *
 * @return			The result length on successful lookup, 0 otherwise.
 */
native geoip_timezone(const ip[], result[], len);

/** 
 * Look up the city's latitude for the given IP address.
 * 
 * @param ip		The IP address to look up. 
 *
 * @return			The result of the geoip look up, 0 if latitude is not found.
 */ 
native Float:geoip_latitude(const ip[]); 

/** 
 * Look up the city's longitude for the given IP address. 
 * 
 * @param ip		The IP address to look up.
 *
 * @return			The result of the geoip look up, 0 if longitude is not found. 
 */ 
native Float:geoip_longitude(const ip[]);

/**
 * Calculate the distance between geographical coordinates, latitude and longitude.
 *
 * @param lat1		The first IP latitude.
 * @param lon1		The first IP longitude.
 * @param lat2		The second IP latitude.
 * @param lon2		The second IP longitude.
 * @param system    The system of measurement, 0 = Metric(kilometers) or 1 = English(miles).  
 *
 * @return			The distance as result in specified system of measurement.
 */
#define SYSTEM_METRIC   0 // kilometers
#define SYSTEM_IMPERIAL 1 // statute miles

native Float:geoip_distance(Float:lat1, Float:lon1, Float:lat2, Float:lon2, system = SYSTEM_METRIC);