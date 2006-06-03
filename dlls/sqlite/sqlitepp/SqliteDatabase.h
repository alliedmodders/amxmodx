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
	private:
		void Disconnect();
	private:
		sqlite3 *m_pSql;
		SqliteDriver *m_pParent;
	};
};

#endif //_INCLUDE_SOURCEMOD_SQLITE_DATABASE_H

