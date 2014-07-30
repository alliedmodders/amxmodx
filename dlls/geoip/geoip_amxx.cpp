<<<<<<< HEAD
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

﻿#include "geoip_amxx.h"

MMDB_s HandleDB;

char *stripPort(char *ip_port)
{
	char *tmp = strchr(ip_port, ':');

	if (!tmp)
	{
		return ip_port;
	}

	*tmp = '\0';

	return tmp;
}

const char *lookupByIp(const char *ip, const char **path, int *length = NULL)
{
	int gai_error = 0, mmdb_error = 0;
	MMDB_lookup_result_s lookup = MMDB_lookup_string(&HandleDB, ip, &gai_error, &mmdb_error);

	if (gai_error != 0 || MMDB_SUCCESS != mmdb_error || !lookup.found_entry)
	{
		return NULL;
	}

	MMDB_entry_data_s entry_data;
	MMDB_aget_value(&lookup.entry, &entry_data, path);

	if (!entry_data.has_data)
	{
		return NULL;
	}

	if (length)
	{
		*length = entry_data.data_size;
	}

	return entry_data.utf8_string;
}

// native geoip_code2(const ip[], ccode[3]);
// Deprecated.
static cell AMX_NATIVE_CALL amx_geoip_code2(AMX *amx, cell *params)
{
	int length;
	char *ip = MF_GetAmxString(amx, params[1], 0, &length);

	const char *path[] = { "country", "iso_code", NULL };
	const char *code = lookupByIp(stripPort(ip), path);

	return MF_SetAmxString(amx, params[2], code ? code : "error", 3);
}

// native geoip_code3(const ip[], result[4]);
// Deprecated.
static cell AMX_NATIVE_CALL amx_geoip_code3(AMX *amx, cell *params)
{
	int length;
	char *ip = MF_GetAmxString(amx, params[1], 0, &length);

	const char *path[] = { "country", "iso_code", NULL };
	const char *code = lookupByIp(stripPort(ip), path);

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
	char *ip = MF_GetAmxString(amx, params[1], 0, &length);

	const char *path[] = { "country", "iso_code", NULL };
	const char *code = lookupByIp(stripPort(ip), path);

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
	char *ip = MF_GetAmxString(amx, params[1], 0, &length);

	const char *path[] = { "country", "iso_code", NULL };
	const char *code = lookupByIp(stripPort(ip), path, &length);

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

// native geoip_country(const ip[], result[], len);
static cell AMX_NATIVE_CALL amx_geoip_country(AMX *amx, cell *params)
{
	int length;
	char *ip = MF_GetAmxString(amx, params[1], 0, &length);

	const char *path[] = { "country", "names", "en", NULL };
	const char *country = lookupByIp(stripPort(ip), path, &length);

	if (!country)
	{
		return 0;
	}

	return MF_SetAmxString(amx, params[2], country, length >= params[3] ? params[3] : length); // TODO: make this utf8 safe.
}

// native geoip_city(const ip[], result[], len);
static cell AMX_NATIVE_CALL amx_geoip_city(AMX *amx, cell *params)
{
	int length;
	char *ip = MF_GetAmxString(amx, params[1], 0, &length);

	const char *path[] = { "city", "names", "en", NULL };
	const char *city = lookupByIp(stripPort(ip), path, &length);

	return MF_SetAmxString(amx, params[2], city ? city : "", length >= params[3] ? params[3] : length); // TODO: make this utf8 safe.
}


void OnAmxxAttach()
{
	const char *databases[] =
	{
		"City",
		"Country" // is the default shipped database with AMXX.
	};

	const char *modName = MF_GetModname();
	const char *dataDir = MF_GetLocalInfo("amxx_datadir", "addons/amxmodx/data");

	char file[255];
	int status = -1;

	for (size_t i = 0; i < ARRAYSIZE(databases); ++i)
	{
		sprintf(file, "%s/%s/GeoLite2-%s.mmdb", modName, dataDir, databases[i]); // MF_BuildPathname not used because backslash
                                                                                 // makes CreateFileMapping failing under windows.
		status = MMDB_open(file, MMDB_MODE_MMAP, &HandleDB);

		if (status == MMDB_SUCCESS)
		{
			break;
		}
		else if (status != MMDB_FILE_OPEN_ERROR)
		{
			MF_Log("Could not open %s - %s", file, MMDB_strerror(status));

			if (status == MMDB_IO_ERROR)
			{
				MF_Log("    IO error: %s", strerror(errno));
			}
		}
	}

	if (status != MMDB_SUCCESS)
	{
		MF_Log("Could not find GeoIP2 databases. Disabled natives.");
		return;
	}

	MF_Log("Database info: %s %i.%i",
		HandleDB.metadata.description.descriptions[0]->description,
		HandleDB.metadata.binary_format_major_version,
		HandleDB.metadata.binary_format_minor_version);

	MF_AddNatives(geoip_natives);
}

void OnAmxxDetach()
{
	MMDB_close(&HandleDB);
}


AMX_NATIVE_INFO geoip_natives[] =
{
	{ "geoip_code2", amx_geoip_code2 },
	{ "geoip_code3", amx_geoip_code3 },

	{ "geoip_code2_ex", amx_geoip_code2_ex },
	{ "geoip_code3_ex", amx_geoip_code3_ex },

	{ "geoip_country", amx_geoip_country },
	{ "geoip_city"   , amx_geoip_city },

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
