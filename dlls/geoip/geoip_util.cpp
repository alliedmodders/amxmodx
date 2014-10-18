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

#include "geoip_util.h"
#include "geoip_natives.h"

char *stripPort(char *ip)
{
	char *tmp = strchr(ip, ':');

	if (tmp)
	{
		*tmp = '\0';
	}

	return ip;
}

const char* stristr(const char* str, const char* substr)
{
	register char *needle = (char *)substr;
	register char *prevloc = (char *)str;
	register char *haystack = (char *)str;

	while (*haystack)
	{
		if (tolower(*haystack) == tolower(*needle))
		{
			haystack++;

			if (!*++needle)
			{
				return prevloc;
			}
		}
		else
		{
			haystack = ++prevloc;
			needle = (char *)substr;
		}
	}

	return NULL;
}

bool lookupByIp(const char *ip, const char **path, MMDB_entry_data_s *result)
{
	int gai_error = 0, mmdb_error = 0;
	MMDB_lookup_result_s lookup = MMDB_lookup_string(&HandleDB, ip, &gai_error, &mmdb_error);

	if (gai_error != 0 || mmdb_error != MMDB_SUCCESS || !lookup.found_entry)
	{
		return false;
	}

	MMDB_entry_data_s entry_data;
	MMDB_aget_value(&lookup.entry, &entry_data, path);

	if (!entry_data.has_data)
	{
		size_t i = 0;

		// Dirty fall back to default language ("en") in case provided user's language is not localized.

		// Searh "names" position.
		while (strcmp(path[i++], "names"));  

		// No localized entry or we use already default language.
		if (!*path[i] || !strcmp(path[i], "en")) 
		{
			return false;
		}

		// Overwrite user's language.
		path[i] = "en"; 
		
		// Try again.
		gai_error = mmdb_error = 0;
		MMDB_aget_value(&lookup.entry, &entry_data, path); 

		if (!entry_data.has_data)
		{
			return false;
		}
	}

	*result = entry_data;

	return true;
}

const char *lookupString(const char *ip, const char **path, int *length)
{
	static char buffer[256]; // This should be large enough for long name in UTF-8.
	MMDB_entry_data_s result;

	if (!lookupByIp(ip, path, &result))
	{
		return NULL;
	}

	// Let's avoid a crash in case we go over the buffer size.
	size_t maxLength = ke::Min((size_t)result.data_size, sizeof(buffer)-1);

	// Strings from database are not null terminated.
	memcpy(buffer, result.utf8_string, maxLength);
	buffer[maxLength] = '\0';

	if (length)
	{
		*length = maxLength;
	}

	return buffer;
}

double lookupDouble(const char *ip, const char **path)
{
	MMDB_entry_data_s result;

	if (!lookupByIp(ip, path, &result))
	{
		return 0;
	}

	return result.double_value;
}

int getContinentId(const char *code)
{
	#define CONTINENT_UNKNOWN        0
	#define CONTINENT_AFRICA         1
	#define CONTINENT_ANTARCTICA     2
	#define CONTINENT_ASIA           3
	#define CONTINENT_EUROPE         4
	#define CONTINENT_NORTH_AMERICA  5
	#define CONTINENT_OCEANIA        6
	#define CONTINENT_SOUTH_AMERICA  7

	int index = CONTINENT_UNKNOWN;

	if (code)
	{
		switch (code[0])
		{
			case 'A':
			{
				switch (code[1])
				{
					case 'F': index = CONTINENT_AFRICA; break;
					case 'N': index = CONTINENT_ANTARCTICA; break;
					case 'S': index = CONTINENT_ASIA; break;
				}
				
				break;
			}
			
			case 'E': index = CONTINENT_EUROPE; break;
			case 'O': index = CONTINENT_OCEANIA; break;
			case 'N': index = CONTINENT_NORTH_AMERICA; break;
			case 'S': index = CONTINENT_SOUTH_AMERICA; break;
		}
	}

	return index;
}

const char *getLang(int playerIndex)
{
	static cvar_t *amxmodx_language = NULL;
	static cvar_t *amxmodx_cl_langs = NULL;

	if (!amxmodx_language)
		 amxmodx_language = CVAR_GET_POINTER("amx_language");

	if (!amxmodx_cl_langs)
		 amxmodx_cl_langs = CVAR_GET_POINTER("amx_client_languages");

	if (playerIndex >= 0 && amxmodx_cl_langs && amxmodx_language)
	{
		const char *value;
		const char *lang;

		if (playerIndex == 0 || amxmodx_cl_langs->value <= 0 || !MF_IsPlayerIngame(playerIndex))
		{
			value = amxmodx_language->string;
		}
		else
		{
			value = ENTITY_KEYVALUE(MF_GetPlayerEdict(playerIndex), "lang");
		}

		if (value && *value)
		{
			for (size_t i = 0; i < LangList.length(); ++i)
			{
				lang = LangList.at(i).chars();

				if (stristr(lang, value) != NULL)
				{
					return lang;
				}
			}
		}
	}

	return "en";
}
