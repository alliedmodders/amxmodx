#include <string.h>
#include <stdio.h>
#include "SqliteHeaders.h"
#include "SqliteDriver.h"
#include "SqliteDatabase.h"

#if defined WIN32
#define snprintf _snprintf
#define strncasecmp strnicmp
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
			snprintf(error, maxlength, "%s", sqlite3_errmsg(pSql));
		}
		sqlite3_close(pSql);
		return NULL;
	} else {
		SqliteDatabase *pDb = new SqliteDatabase(pSql, this);
		return static_cast<IDatabase *>(pDb);
	}
}
