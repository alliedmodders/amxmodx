/* Geoip
 *   Copyright 2007-2014
 *   By the AMX Mod X Development Team
 *
 *  Geoip is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  Geoip is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Geoip; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of Ham Sandwich with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */
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

	if (gai_error != 0 || MMDB_SUCCESS != mmdb_error || !lookup.found_entry)
	{
		return false;
	}

	MMDB_entry_data_s entry_data;
	MMDB_aget_value(&lookup.entry, &entry_data, path);

	if (!entry_data.has_data)
	{
		return false;
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
	size_t maxLength = ke::Min((size_t)result.data_size, sizeof(buffer));

	// Strings from database are not null terminated.
	memcpy(buffer, result.utf8_string, maxLength);
	buffer[result.data_size] = '\0';

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
	#define CONTINENT_ASIA           2
	#define CONTINENT_EUROPE         3
	#define CONTINENT_NORTH_AMERICA  4
	#define CONTINENT_OCEANIA        5
	#define CONTINENT_SOUTH_AMERICA  6

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
						case 'S': index = CONTINENT_ASIA; break;
					}
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
