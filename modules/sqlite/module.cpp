// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// SQLite Module
//

#include "amxxmodule.h"
#include "sqlite_header.h"
#include "sqlheaders.h"

static int g_ident = 0;

SqlFunctions g_SqliteFuncs = 
{
	&g_Sqlite,
	SetMysqlAffinity,
	NULL
};

int SetMysqlAffinity(AMX *amx)
{
	MF_AmxReRegister(amx, g_BaseSqlNatives, -1);
	MF_AmxReRegister(amx, g_ThreadSqlNatives, -1);

	return 1;
}

bool DirExists(const char *dir)
{
#if defined WIN32 || defined _WIN32
	DWORD attr = GetFileAttributes(dir);

	if (attr == INVALID_FILE_ATTRIBUTES)
		return false;

	if (attr & FILE_ATTRIBUTE_DIRECTORY)
		return true;

#else
	struct stat s;

	if (stat(dir, &s) != 0)
		return false;

	if (S_ISDIR(s.st_mode))
		return true;
#endif

	return false;
}

void OnAmxxAttach()
{
	MF_AddNatives(g_BaseSqlNatives);
	MF_AddNatives(g_ThreadSqlNatives);
	g_SqliteFuncs.prev = (SqlFunctions *)MF_RegisterFunctionEx(&g_SqliteFuncs, SQL_DRIVER_FUNC);

	MF_AddLibraries("dbi", LibType_Class, &g_ident);

	//override any mysqlx old compat stuff
	MF_AddNatives(g_OldCompatNatives);
	MF_OverrideNatives(g_OldCompatNatives, MODULE_NAME);

	char path[255];
	MF_BuildPathnameR(path, sizeof(path)-1, "%s/sqlite3", MF_GetLocalInfo("amxx_datadir", "addons/amxmodx/data"));
	if (!DirExists(path))
	{
		mkdir(path
#if defined(__linux__) || defined(__APPLE__)
			, 0775
#endif
			);
	}
}

void OnAmxxDetach()
{
	ShutdownThreading();
	MF_RemoveLibraries(&g_ident);
}

void OnPluginsUnloaded()
{
	FreeAllHandles(Handle_OldResult);
	FreeAllHandles(Handle_OldDb);
	FreeAllHandles(Handle_Connection);
}

extern "C" void __cxa_pure_virtual(void)
{
}

