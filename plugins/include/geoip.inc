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
