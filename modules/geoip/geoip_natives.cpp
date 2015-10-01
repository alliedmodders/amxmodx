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

#include "geoip_main.h"
#include "geoip_natives.h"
#include "geoip_util.h"

#include <amtl/am-string.h>
#include <amtl/am-vector.h>

// native geoip_code2(const ip[], ccode[3]);
// Deprecated.
static cell AMX_NATIVE_CALL amx_geoip_code2(AMX *amx, cell *params)
{
	int length;
	char *ip = stripPort(MF_GetAmxString(amx, params[1], 0, &length));

	const char *path[] = { "country", "iso_code", NULL };
	const char *code = lookupString(ip, path);

	return MF_SetAmxString(amx, params[2], code ? code : "error", 3);
}

// native geoip_code3(const ip[], result[4]);
// Deprecated.
static cell AMX_NATIVE_CALL amx_geoip_code3(AMX *amx, cell *params)
{
	int length;
	char *ip = stripPort(MF_GetAmxString(amx, params[1], 0, &length));

	const char *path[] = { "country", "iso_code", NULL };
	const char *code = lookupString(ip, path);

	for (size_t i = 0; i < ARRAYSIZE(GeoIPCountryCode); ++i)
	{
		if (!strncmp(code, GeoIPCountryCode[i], 2))
		{
			code = GeoIPCountryCode3[i];
			break;
		}
	}

	return MF_SetAmxString(amx, params[2], code ? code : "error", 4);
}

// native bool:geoip_code2_ex(const ip[], result[3]);
static cell AMX_NATIVE_CALL amx_geoip_code2_ex(AMX *amx, cell *params)
{
	int length;
	char *ip = stripPort(MF_GetAmxString(amx, params[1], 0, &length));

	const char *path[] = { "country", "iso_code", NULL };
	const char *code = lookupString(ip, path);

	if (!code)
	{
		return 0;
	}

	MF_SetAmxString(amx, params[2], code, 2);

	return 1;
}

// native bool:geoip_code3_ex(const ip[], result[4]);
static cell AMX_NATIVE_CALL amx_geoip_code3_ex(AMX *amx, cell *params)
{
	int length;
	char *ip = stripPort(MF_GetAmxString(amx, params[1], 0, &length));

	const char *path[] = { "country", "iso_code", NULL };
	const char *code = lookupString(ip, path, &length);

	if (!code)
	{
		return 0;
	}

	for (size_t i = 0; i < ARRAYSIZE(GeoIPCountryCode); ++i)
	{
		if (!strncmp(code, GeoIPCountryCode[i], 2))
		{
			code = GeoIPCountryCode3[i];
			break;
		}
	}

	MF_SetAmxString(amx, params[2], code, 3);

	return 1;
}

// native geoip_country(const ip[], result[], len = 45);
// Deprecated.
static cell AMX_NATIVE_CALL amx_geoip_country(AMX *amx, cell *params)
{
	int length;
	char *ip = stripPort(MF_GetAmxString(amx, params[1], 0, &length));

	const char *path[] = { "country", "names", "en", NULL };
	const char *country = lookupString(ip, path, &length);

	if (!country)
	{
		return MF_SetAmxString(amx, params[2], "error", params[3]);
	}

	return MF_SetAmxStringUTF8Char(amx, params[2], country, length, params[3]);
}

// native geoip_country_ex(const ip[], result[], len, id = -1);
static cell AMX_NATIVE_CALL amx_geoip_country_ex(AMX *amx, cell *params)
{
	int length;
	char *ip = stripPort(MF_GetAmxString(amx, params[1], 0, &length));

	const char *path[] = { "country", "names", getLang(params[4]), NULL };
	const char *country = lookupString(ip, path, &length);

	return MF_SetAmxStringUTF8Char(amx, params[2], country ? country : "", length, params[3]);
}

// native geoip_city(const ip[], result[], len, id = -1);
static cell AMX_NATIVE_CALL amx_geoip_city(AMX *amx, cell *params)
{
	int length;
	char *ip = stripPort(MF_GetAmxString(amx, params[1], 0, &length));

	const char *path[] = { "city", "names", getLang(params[4]), NULL };
	const char *city = lookupString(ip, path, &length);

	return MF_SetAmxStringUTF8Char(amx, params[2], city ? city : "", length, params[3]);
}

// native geoip_region_code(const ip[], result[], len);
static cell AMX_NATIVE_CALL amx_geoip_region_code(AMX *amx, cell *params)
{
	int length;
	int finalLength = 0;
	char code[12]; // This should be largely enough to hold xx-yyyy and more if needed.

	char *ip = stripPort(MF_GetAmxString(amx, params[1], 0, &length));

	const char *pathCountry[] = { "country", "iso_code", NULL };
	const char *countryCode = lookupString(ip, pathCountry, &length);

	if (countryCode)
	{
		finalLength = length + 1; // + 1 for dash.
		ke::SafeSprintf(code, finalLength + 1, "%s-", countryCode); // + EOS.

		const char *pathRegion[] = { "subdivisions", "0", "iso_code", NULL }; // First result.
		const char *regionCode = lookupString(ip, pathRegion, &length);

		if (regionCode)
		{
			finalLength += length;
			strncat(code, regionCode, length);
		}
		else
		{
			finalLength = 0;
		}
	}

	return MF_SetAmxString(amx, params[2], finalLength ? code : "", ke::Min(finalLength, params[3]));
}

// native geoip_region_name(const ip[], result[], len, id = -1);
static cell AMX_NATIVE_CALL amx_geoip_region_name(AMX *amx, cell *params)
{
	int length;
	char *ip = stripPort(MF_GetAmxString(amx, params[1], 0, &length));

	const char *path[] = { "subdivisions", "0", "names", getLang(params[4]), NULL }; // First result.
	const char *region = lookupString(ip, path, &length);

	return MF_SetAmxStringUTF8Char(amx, params[2], region ? region : "", length, params[3]);
}

// native geoip_timezone(const ip[], result[], len);
static cell AMX_NATIVE_CALL amx_geoip_timezone(AMX *amx, cell *params)
{
	int length;
	char *ip = stripPort(MF_GetAmxString(amx, params[1], 0, &length));

	const char *path[] = { "location", "time_zone", NULL };
	const char *timezone = lookupString(ip, path, &length);

	return MF_SetAmxString(amx, params[2], timezone ? timezone : "", ke::Min(length, params[3]));
}

// native geoip_latitude(const ip[]);
static cell AMX_NATIVE_CALL amx_geoip_latitude(AMX *amx, cell *params)
{
	int length;
	char *ip = stripPort(MF_GetAmxString(amx, params[1], 0, &length));

	const char *path[] = { "location", "latitude", NULL };
	double latitude = lookupDouble(ip, path);

	return amx_ftoc(latitude);
}

// native geoip_longitude(const ip[]);
static cell AMX_NATIVE_CALL amx_geoip_longitude(AMX *amx, cell *params)
{
	int length;
	char *ip = stripPort(MF_GetAmxString(amx, params[1], 0, &length));

	const char *path[] = { "location", "longitude", NULL };
	double longitude = lookupDouble(ip, path);

	return amx_ftoc(longitude);
}

// native Float:geoip_distance(Float:lat1, Float:lon1, Float:lat2, Float:lon2, system = SYSTEM_METRIC);
static cell AMX_NATIVE_CALL amx_geoip_distance(AMX *amx, cell *params)
{
	float earthRadius = params[5] ? 3958.0 : 6370.997; // miles / km

	float lat1 = amx_ctof(params[1]) * (M_PI / 180);
	float lon1 = amx_ctof(params[2]) * (M_PI / 180);
	float lat2 = amx_ctof(params[3]) * (M_PI / 180);
	float lon2 = amx_ctof(params[4]) * (M_PI / 180);

	return amx_ftoc(earthRadius * acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon2 - lon1)));
}

// native Continent:geoip_continent_code(const ip[], result[3] = "");
static cell AMX_NATIVE_CALL amx_geoip_continent_code(AMX *amx, cell *params)
{
	int length;
	char *ip = stripPort(MF_GetAmxString(amx, params[1], 0, &length));

	const char *path[] = { "continent", "code", NULL };
	const char *code = lookupString(ip, path, &length);

	MF_SetAmxString(amx, params[2], code ? code : "", code ? 2 : 0);

	return getContinentId(code);
}

// native geoip_continent_name(const ip[], result[], len, id = -1);
static cell AMX_NATIVE_CALL amx_geoip_continent_name(AMX *amx, cell *params)
{
	int length;
	char *ip = stripPort(MF_GetAmxString(amx, params[1], 0, &length));

	const char *path[] = { "continent", "names", getLang(params[4]), NULL };
	const char *continent = lookupString(ip, path, &length);

	return MF_SetAmxStringUTF8Char(amx, params[2], continent ? continent : "", length, params[3]);
}


AMX_NATIVE_INFO GeoipNatives[] =
{
	{ "geoip_code2"         , amx_geoip_code2 }, // Deprecated
	{ "geoip_code3"         , amx_geoip_code3 }, // Deprecated

	{ "geoip_code2_ex"      , amx_geoip_code2_ex },
	{ "geoip_code3_ex"      , amx_geoip_code3_ex },

	{ "geoip_country"       , amx_geoip_country }, // Deprecated
	{ "geoip_country_ex"    , amx_geoip_country_ex },
	{ "geoip_city"          , amx_geoip_city },

	{ "geoip_region_code"   , amx_geoip_region_code },
	{ "geoip_region_name"   , amx_geoip_region_name },

	{ "geoip_timezone"      , amx_geoip_timezone },
	{ "geoip_latitude"      , amx_geoip_latitude },
	{ "geoip_longitude"     , amx_geoip_longitude },
	{ "geoip_distance"      , amx_geoip_distance },

	{ "geoip_continent_code", amx_geoip_continent_code },
	{ "geoip_continent_name", amx_geoip_continent_name },

	{ NULL, NULL },
};


/**
* GEOIP2 DATA EXAMPLE:
*
* {
*   "city":  {
*       "confidence":  25,
*       "geoname_id": 54321,
*       "names":  {
*           "de":    "Los Angeles",
*           "en":    "Los Angeles",
*           "es":    "Los Ángeles",
*           "fr":    "Los Angeles",
*           "ja":    "ロサンゼルス市",
*           "pt-BR":  "Los Angeles",
*           "ru":    "Лос-Анджелес",
*           "zh-CN": "洛杉矶"
*       }
*   },
*   "continent":  {
*       "code":       "NA",
*       "geoname_id": 123456,
*       "names":  {
*           "de":    "Nordamerika",
*           "en":    "North America",
*           "es":    "América del Norte",
*           "fr":    "Amérique du Nord",
*           "ja":    "北アメリカ",
*           "pt-BR": "América do Norte",
*           "ru":    "Северная Америка",
*           "zh-CN": "北美洲"
*
*       }
*   },
*   "country":  {
*       "confidence":  75,
*       "geoname_id": "6252001",
*       "iso_code":    "US",
*       "names":  {
*           "de":     "USA",
*           "en":     "United States",
*           "es":     "Estados Unidos",
*           "fr":     "États-Unis",
*           "ja":     "アメリカ合衆国",
*           "pt-BR":  "Estados Unidos",
*           "ru":     "США",
*           "zh-CN":  "美国"
*       }
*   },
*   "location":  {
*       "accuracy_radius":   20,
*       "latitude":          37.6293,
*       "longitude":         -122.1163,
*       "metro_code":        807,
*       "time_zone":         "America/Los_Angeles"
*   },
*   "postal": {
*       "code":       "90001",
*       "confidence": 10
*   },
*   "registered_country":  {
*       "geoname_id": "6252001",
*       "iso_code":    "US",
*       "names":  {
*           "de":     "USA",
*           "en":     "United States",
*           "es":     "Estados Unidos",
*           "fr":     "États-Unis",
*           "ja":     "アメリカ合衆国",
*           "pt-BR":  "Estados Unidos",
*           "ru":     "США",
*           "zh-CN":  "美国"
*       }
*   },
*   "represented_country":  {
*      "geoname_id": "6252001",
*       "iso_code":    "US",
*       "names":  {
*           "de":     "USA",
*           "en":     "United States",
*           "es":     "Estados Unidos",
*           "fr":     "États-Unis",
*           "ja":     "アメリカ合衆国",
*           "pt-BR":  "Estados Unidos",
*           "ru":     "США",
*           "zh-CN":  "美国"
*       },
*       "type": "military"
*   },
*   "subdivisions":  [
*       {
*           "confidence":  50,
*           "geoname_id": 5332921,
*           "iso_code":    "CA",
*           "names":  {
*               "de":    "Kalifornien",
*               "en":    "California",
*               "es":    "California",
*               "fr":    "Californie",
*               "ja":    "カリフォルニア",
*               "ru":    "Калифорния",
*               "zh-CN": "加州"
*           }
*       }
*   ],
*   "traits": {
*       "autonomous_system_number":      "1239",
*       "autonomous_system_organization": "Linkem IR WiMax Network",
*       "domain":                        "example.com",
*       "is_anonymous_proxy":            true,
*       "is_transparent_proxy":          true,
*       "isp":                           "Linkem spa",
*       "ip_address":                    "1.2.3.4",
*       "organization":                  "Linkem IR WiMax Network",
*       "user_type":                     "traveler",
*   },
*   "maxmind": {
*       "queries_remaining":            "54321"
*   }
* }
*/
