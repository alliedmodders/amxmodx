#include <stdio.h>
#include <string.h>
#include "SqliteDriver.h"
#include "SqliteDatabase.h"
#include "SqliteQuery.h"

#if defined WIN32 && !defined vsnprintf
#define vsnprintf _vsnprintf
#endif

using namespace SourceMod;

SqliteDatabase::SqliteDatabase(sqlite3 *sql, SqliteDriver *drvr) : 
	m_pSql(sql), m_pParent(drvr)
{
}

SqliteDatabase::~SqliteDatabase()
{
	Disconnect();
}

void SqliteDatabase::Disconnect()
{
	if (m_pSql)
	{
		sqlite3_close(m_pSql);
		m_pSql = NULL;
	}
}

void SqliteDatabase::FreeHandle()
{
	delete this;
}

ISQLDriver *SqliteDatabase::Driver()
{
	return static_cast<ISQLDriver *>(m_pParent);
}

IQuery *SqliteDatabase::PrepareQuery(const char *query)
{
	SqliteQuery *pQuery = new SqliteQuery(this, query);
	return static_cast<IQuery *>(pQuery);
}

IQuery *SqliteDatabase::PrepareQueryFmt(const char *fmt, va_list ap)
{
	char buffer[4096];

	vsnprintf(buffer, sizeof(buffer)-1, fmt, ap);
	
	return PrepareQuery(buffer);
}

IQuery *SqliteDatabase::PrepareQueryFmt(const char *fmt, ...)
{
	va_list ap;
	IQuery *qry;

	va_start(ap, fmt);
	qry = PrepareQueryFmt(fmt, ap);
	va_end(ap);

	return qry;
}

int SqliteDatabase::QuoteString(const char *str, char buffer[], size_t maxlen, size_t *newsize)
{
	char *res = sqlite3_snprintf(static_cast<int>(maxlen), buffer, "%q", str);

	if (res != NULL && newsize != NULL)
	{
		*newsize = strlen(buffer);
	}

	return 0;
}

