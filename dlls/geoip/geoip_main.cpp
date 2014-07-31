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
#include "geoip_main.h"
#include "geoip_natives.h"

MMDB_s HandleDB;
ke::Vector<ke::AString> LangList;

void OnAmxxAttach()
{
	if (loadDatabase())
	{
		MF_AddNatives(GeoipNatives);
	}
}

void OnAmxxDetach()
{
	MMDB_close(&HandleDB);

	LangList.clear();
}

bool loadDatabase()
{
	if (HandleDB.filename) // Already loaded.
	{
		return true;
	}

	const char *databases[] =
	{
		"City",   
		"Country" // Is the default shipped database with AMXX.
	};

	const char *modName = MF_GetModname();
	const char *dataDir = MF_GetLocalInfo("amxx_datadir", "addons/amxmodx/data");

	char file[255];
	int status = -1;

	for (size_t i = 0; i < ARRAYSIZE(databases); ++i)
	{
		snprintf(file, sizeof(file)-1, "%s/%s/GeoLite2-%s.mmdb", modName, dataDir, databases[i]); // MF_BuildPathname not used because backslash
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
		return false;
	}

	MF_Log("Database info: %s %i.%i",
		HandleDB.metadata.description.descriptions[0]->description,
		HandleDB.metadata.binary_format_major_version,
		HandleDB.metadata.binary_format_minor_version);

	// Retrieve supported languages.
	for (size_t i = 0; i < HandleDB.metadata.languages.count; i++)
	{
		LangList.append(ke::AString(HandleDB.metadata.languages.names[i]));
	}

	return true;
}
