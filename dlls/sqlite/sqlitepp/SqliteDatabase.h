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

#ifndef _INCLUDE_SOURCEMOD_SQLITE_DATABASE_H
#define _INCLUDE_SOURCEMOD_SQLITE_DATABASE_H

#include "SqliteHeaders.h"
#include "SqliteDriver.h"

namespace SourceMod
{
	class SqliteDriver;

	class SqliteDatabase : public IDatabase
	{
		friend class SqliteQuery;
	public:
		SqliteDatabase(sqlite3 *sql, SqliteDriver *drvr);
		~SqliteDatabase();
	public:
		void FreeHandle();
		ISQLDriver *Driver();
	public:
		IQuery *PrepareQueryFmt(const char *fmt, ...);
		IQuery *PrepareQueryFmt(const char *fmt, va_list ap);
		IQuery *PrepareQuery(const char *query);
		int QuoteString(const char *str, char buffer[], size_t maxlen, size_t *newsize);
		bool SetCharacterSet(const char *characterset);
	private:
		void Disconnect();
	private:
		sqlite3 *m_pSql;
		SqliteDriver *m_pParent;
	};
};

#endif //_INCLUDE_SOURCEMOD_SQLITE_DATABASE_H

