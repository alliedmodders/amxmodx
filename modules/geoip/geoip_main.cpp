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
#include <time.h>

MMDB_s HandleDB;
ke::Vector<ke::AString> LangList;

void OnAmxxAttach()
{
	if (loadDatabase())
	{
		MF_AddNatives(GeoipNatives);
	}

	REG_SVR_COMMAND("geoip", OnGeoipCommand);
}

void OnAmxxDetach()
{
	MMDB_close(&HandleDB);

	LangList.clear();
}

void OnGeoipCommand()
{
	const char *cmd = CMD_ARGV(1);

	if (!strcmp(cmd, "version"))
	{
		if (!HandleDB.filename)
		{
			MF_PrintSrvConsole("\n  Database is not loaded.\n");
			return;
		}

		const char *meta_dump = "\n"
			"  Database metadata\n"
			"    Node count:    %i\n"
			"    Record size:   %i bits\n"
			"    IP version:    IPv%i\n"
			"    Binary format: %i.%i\n"
			"    Build epoch:   %llu (%s)\n"
			"    Type:          %s\n"
			"    Languages:     ";

		char date[40];
		strftime(date, sizeof(date), "%Y-%m-%d %H:%M:%S UTC", gmtime((const time_t *)&HandleDB.metadata.build_epoch));

		fprintf(stdout, meta_dump,
				HandleDB.metadata.node_count,
				HandleDB.metadata.record_size,
				HandleDB.metadata.ip_version,
				HandleDB.metadata.binary_format_major_version,
				HandleDB.metadata.binary_format_minor_version,
				HandleDB.metadata.build_epoch,
				date,
				HandleDB.metadata.database_type);

		for (size_t i = 0; i < HandleDB.metadata.languages.count; ++i)
		{
			fprintf(stdout, "%s", HandleDB.metadata.languages.names[i]);

			if (i < HandleDB.metadata.languages.count - 1)
			{
				fprintf(stdout, " ");
			}
		}

		fprintf(stdout, "\n");
		fprintf(stdout, "    Description:\n");

		for (size_t i = 0; i < HandleDB.metadata.description.count; ++i)
		{
			fprintf(stdout, "      %s:   %s\n",
					HandleDB.metadata.description.descriptions[i]->language,
					HandleDB.metadata.description.descriptions[i]->description);
		}
		fprintf(stdout, "\n");
	}
	else if (!strcmp(cmd, "dump"))
	{
		if (!HandleDB.filename)
		{
			MF_PrintSrvConsole("\n  Database is not loaded.\n\n");
			return;
		}

		int num_args = CMD_ARGC();

		if (num_args < 3)
		{
			MF_PrintSrvConsole("\n  An IP address must be provided.\n\n");
			return;
		}

		char *ip = stripPort((char *)CMD_ARGV(2));

		int gai_error = 0;
		int mmdb_error = 0;

		MMDB_lookup_result_s result = MMDB_lookup_string(&HandleDB, ip, &gai_error, &mmdb_error);

		if (gai_error != 0 || mmdb_error != MMDB_SUCCESS || !result.found_entry)
		{
			MF_PrintSrvConsole("\n  Either look up failed or no found result.\n\n");
			return;
		}

		MMDB_entry_data_list_s *entry_data_list = NULL;
		int status = -1;

		if ((status = MMDB_get_entry_data_list(&result.entry, &entry_data_list)) != MMDB_SUCCESS || entry_data_list == NULL)
		{
			MF_PrintSrvConsole("\n  Could not retrieve data list - %s.\n\n", MMDB_strerror(status));
			return;
		}

		const char *file = NULL;
		FILE *fp = NULL;

		if (num_args > 3)
		{
			file = CMD_ARGV(3);
			fp = fopen(MF_BuildPathname("%s", file), "w");
		}

		if (!fp)
		{
			file = NULL;
			fp = stdout;
		}

		fprintf(fp, "\n");
		MMDB_dump_entry_data_list(fp, entry_data_list, 2);
		fprintf(fp, "\n");

		if (file)
		{
			fclose(fp);
		}

		MMDB_free_entry_data_list(entry_data_list);
	}
	else
	{
		MF_PrintSrvConsole("\n");
		MF_PrintSrvConsole("  Usage: geoip <command> [argument]\n");
		MF_PrintSrvConsole("  Commands:\n");
		MF_PrintSrvConsole("     version                 - display geoip database metadata\n");
		MF_PrintSrvConsole("     dump <ip> [output file] - dump all data from an IP address formatted in a JSON-ish fashion.\n");
		MF_PrintSrvConsole("                               An output file is mod-based and if not provided, it will print in the console.\n");
		MF_PrintSrvConsole("\n");
	}
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
		// MF_BuildPathname not used because backslash
		// makes CreateFileMapping failing under windows.

		ke::SafeSprintf(file, sizeof(file), "%s/%s/GeoLite2-%s.mmdb", modName, dataDir, databases[i]);

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
