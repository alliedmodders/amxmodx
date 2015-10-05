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
#include "SqliteHeaders.h"
#include "SqliteDriver.h"
#include "SqliteDatabase.h"
#include <amtl/am-string.h>

#if defined WIN32
#define WINDOWS_LEAN_AND_MEAN
#include <windows.h>
#else
#include <unistd.h>
#endif

using namespace SourceMod;

bool SqliteDriver::IsCompatDriver(const char *namestr)
{
	return (strncasecmp(namestr, "sqlite", 5) == 0);
}

const char *SqliteDriver::NameString()
{
	return "sqlite";
}

int busy_handler(void *unused1, int unused2)
{
#if defined __linux__ || defined __APPLE__
	usleep(100000);
#else
	Sleep(100);
#endif

	return 1;
}
IDatabase *SqliteDriver::Connect2(DatabaseInfo *info, int *errcode, char *error, size_t maxlength)
{
	return Connect(info, errcode, error, maxlength);
}

IDatabase *SqliteDriver::Connect(DatabaseInfo *info, int *errcode, char *error, size_t maxlength)
{
	sqlite3 *pSql;
	int err = sqlite3_open(info->database, &pSql);
	if (err != SQLITE_OK)
	{
		if (errcode)
		{
			*errcode = sqlite3_errcode(pSql);
		}
		if (error)
		{
			ke::SafeSprintf(error, maxlength, "%s", sqlite3_errmsg(pSql));
		}
		sqlite3_close(pSql);
		return NULL;
	} else {
		sqlite3_busy_handler(pSql, busy_handler, NULL);
		SqliteDatabase *pDb = new SqliteDatabase(pSql, this);
		return static_cast<IDatabase *>(pDb);
	}
}

int SqliteDriver::QuoteString(const char *str, char buffer[], size_t maxlen, size_t *newsize)
{
	char *res = sqlite3_snprintf(static_cast<int>(maxlen), buffer, "%q", str);

	if (res != NULL && newsize != NULL)
	{
		*newsize = strlen(buffer);
	}

	return 0;
}

