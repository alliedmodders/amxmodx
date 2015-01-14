// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// GeoIP Module
//

#ifndef _INCLUDE_GEOIPUTIL_H
#define _INCLUDE_GEOIPUTIL_H

#include "geoip_main.h"

char *stripPort(char *ip);

bool lookupByIp(const char *ip, const char **path, MMDB_entry_data_s *result);
double lookupDouble(const char *ip, const char **path);
const char *lookupString(const char *ip, const char **path, int *length = NULL);

int getContinentId(const char *code);
const char *getLang(int playerIndex);

const char* stristr(const char* str, const char* substr);

extern const char GeoIPCountryCode[252][3];
extern const char GeoIPCountryCode3[252][4];

#endif // _INCLUDE_GEOIPUTIL_H
